# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261022

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) BOUNDARY, FORMULA, VALIDATION AND LONGITUDINAL STORAGE
# <-----------------------------------------------------------------------------

for (module in c('catalog.R', 'derivation.R', 'price_analysis.R',
    'catalog_refresh.R', 'refresh_validation.R', 'snapshot_analysis.R',
    'conalco_daily.R')) source(file.path(APM_ROOT, 'R', module), local = F)

daily_fixture_products = \(id, date = '2026-10-22',
  abv = c(0, 0.05, 0.499, 0.5, 0.501, 1.2, 5, NA_real_)) {
  x = with_dir(APM_ROOT, parse_conalco(file.path('tests/fixtures',
      'conalco_synthetic/beer_deposit.json')))
  x = x[rep(1L, length(abv)), ]
  x$run_id = id
  x$retailer_product_id = sprintf('%04d', seq_along(abv))
  x$observed_at = paste0(date, 'T08:00:00.000000Z')
  x$date = date
  x$product_name = 'Fiktives Lager'
  x$designation_raw = 'Bier'
  x$retailer_category = 'Bier'
  x$archived_description_raw = 'Synthetisches Bier ohne reale Haendlerdaten.'
  x$ingredients_raw = 'Wasser, Malz, Hopfen'
  x$in_rtd_rts_category = F
  x$alcohol_content = abv
  x$abv = abv
  x$price_current = 1
  x$volume_l = 0.5
  x$unit_volume_l = 0.5
  x$pack_count = 1
  x$pack_basis = 'single_unit_no_pack_marker'
  x$price_per_l = 2
  x$price_per_l_displayed = 2
  x$deposit = 0.08
  x$availability = 'in_stock'
  x
}

daily_fixture_manifest = \(x) {
  id = unique(x$run_id)
  directory = ensure_dir(file.path('data/processed/catalog_refresh', id,
      'validated'))
  urls = c('https://www.conalco.de/ready-to-drink/',
    'https://www.conalco.de/Ready-to-Serve')
  tables = list(products = x,
    category_completeness = data.frame(category_url = urls,
      category_status = 'complete_observed_pagination', expected_pages = 1L,
      successful_pages = 1L, unresolved_details = 0L),
    page_completeness = data.frame(category_url = urls, page = 1L,
      status = 'ok', terminal_page_verified = T),
    product_ids = x['retailer_product_id'],
    product_details = data.frame(retailer_product_id = x$retailer_product_id,
      detail_status = 'processed'), category_links = data.frame(
      category_url = urls))
  proof = lapply(names(tables), \(name) {
    path = file.path(directory, paste0(name, '.csv'))
    write_csv_once(tables[[name]], path)
    refresh_file_record(path)
  })
  names(proof) = names(tables)
  path = file.path(run_dir(id), 'manifest.json')
  write_json_once(list(run_id = id, status = 'complete', aborted = F,
      catalog_coverage_status = 'complete_defined_relevant_scope',
      scope = list(navigation_reviewed = T, discovery_exhausted = T),
      proof = proof), path)
  path
}

daily_test_root = \() {
  root = tempfile()
  ensure_dir(file.path(root, 'R'))
  for (name in c('classification', 'quality', 'price_analysis',
      'snapshot_analysis', 'conalco_daily', 'storage_transport',
      'conalco_actions'))
    stopifnot(file.copy(file.path(APM_ROOT, 'R', paste0(name, '.R')),
      file.path(root, 'R')))
  root
}

test_that('ABV uses inclusive 0.5 percent and distinguishes missing values', {
  x = daily_fixture_products('boundary')
  result = daily_analyse_products(x)
  expect_identical(result$population_status, c(rep('below_0.5', 3),
      rep('included_ge_0.5', 4), 'missing_abv'))
  expect_identical(which(result$daily_price_eligible), 4:7)
  expect_equal(nrow(result), nrow(x))
  expect_identical(result$alcohol_content, x$alcohol_content)
  expect_identical(result$beverage_group, rep('beer', nrow(x)))
  expect_equal(result$price_eur_per_10g[4], 10 / (500 * .005 * .789))
  expect_equal(result$price_eur_per_l[4], 2)
  expect_match(result$daily_exclusion_reasons[8], 'missing_abv')
  expect_false(grepl('below_0.5', result$daily_exclusion_reasons[8], fixed = T))
  bad = daily_analyse_products(daily_fixture_products('bad',
      abv = c(-1, Inf, 101)))
  expect_true(all(bad$population_status == 'invalid_abv'))
  expect_false(any(bad$daily_price_eligible))
})

test_that('summaries count exclusions and use independent type 7 quartiles', {
  x = daily_analyse_products(daily_fixture_products('statistics'))
  stats = daily_statistics(x)
  all = stats[stats$category == '__all__' &
      stats$partition == 'primary_exact15', ]
  expect_equal(all$n_observed, c(8, 8))
  expect_equal(all$n, c(4, 4))
  expect_equal(all$n_excluded, c(4, 4))
  expect_equal(all$n_missing_abv, c(1, 1))
  expect_equal(all$n_below_0.5, c(3, 3))
  values = sort(10 / (500 * c(.005, .00501, .012, .05) * .789))
  ethanol = all[all$price_measure == 'price_eur_per_10g', ]
  expect_equal(ethanol$mean, sum(values) / 4)
  expect_equal(ethanol$median, sum(values[2:3]) / 2)
  expect_equal(ethanol$q1, .25 * values[1] + .75 * values[2])
  expect_equal(ethanol$q3, .75 * values[3] + .25 * values[4])
  expect_equal(ethanol$iqr, ethanol$q3 - ethanol$q1)
})

test_that('append preserves history and repeated execution creates no rows', {
  local_dir(daily_test_root())
  first = daily_fixture_manifest(daily_fixture_products('day1'))
  append_conalco_daily(first)
  before = read_conalco_daily_panel()
  shard = file.path(CONALCO_DAILY_ROOT, 'runs/day1/observations.csv.gz')
  sha = digest(file = shard, algo = 'sha256')
  append_conalco_daily(first)
  expect_identical(read_conalco_daily_panel(), before)
  second = daily_fixture_manifest(daily_fixture_products('day2',
      '2026-10-23'))
  append_conalco_daily(second)
  after = read_conalco_daily_panel()
  expect_equal(nrow(after), 16L)
  expect_identical(after[after$run_id == 'day1', ], before)
  expect_identical(digest(file = shard, algo = 'sha256'), sha)
  expect_identical(after$retailer_product_id[1], '0001')
  third = daily_fixture_manifest(daily_fixture_products('day2_duplicate',
      '2026-10-23'))
  expect_error(append_conalco_daily(third), 'already committed')
  expect_identical(read_conalco_daily_panel(), after)
  backup = daily_archive_run(first, 'backup')
  expect_true(backup$restore_byte_identical)
  expect_false(backup$originals_removed)
  expect_identical(daily_archive_run(first, 'backup'), backup)
})

test_that('failed validation and date boundaries cannot publish a daily run', {
  local_dir(daily_test_root())
  x = daily_fixture_products('partial')
  path = daily_fixture_manifest(x)
  manifest = read_json(path)
  manifest$status = 'partial'
  write_json(manifest, path, auto_unbox = T)
  expect_error(append_conalco_daily(path), 'status is not complete')
  expect_false(dir.exists(CONALCO_DAILY_ROOT))
  path = daily_fixture_manifest(daily_fixture_products('changed'))
  cat('\nchanged', file = read_json(path)$proof$products$path, append = T)
  expect_error(append_conalco_daily(path), 'changed completeness evidence')
  x = daily_fixture_products('midnight')
  x$observed_at[1] = '2026-10-22T22:01:00.000000Z'
  path = daily_fixture_manifest(x)
  expect_error(append_conalco_daily(path), 'multiple Berlin dates')
})

test_that('interrupted view publication is detected and repaired offline', {
  local_dir(daily_test_root())
  path = daily_fixture_manifest(daily_fixture_products('recovery'))
  append_conalco_daily(path)
  before = read_conalco_daily_panel()
  writeLines('interrupted', file.path(CONALCO_DAILY_ROOT, 'statistics.csv'))
  expect_error(read_conalco_daily_panel(), 'checksum mismatch')
  append_conalco_daily(path)
  expect_identical(read_conalco_daily_panel(), before)
  lock = file.path(CONALCO_DAILY_ROOT, '.write_lock')
  daily_lock(lock)
  expect_error(append_conalco_daily(path), 'Daily lock exists')
  expect_true(file.exists(file.path(lock, 'owner.json')))
})

test_that('disabled or unreviewed daily operation sends zero requests', {
  local_dir(daily_test_root())
  requests = 0L
  client = \(url) {
    requests = requests + 1L
    stop('Unexpected network call')
  }
  path = 'daily.json'
  config = list(enabled = F, timezone = 'Europe/Berlin',
    start_date = '2026-10-23', scheduled_local_time = '09:00')
  write_json(config, path, auto_unbox = T)
  expect_error(run_conalco_daily(T, path, client), 'disabled')
  config$enabled = T
  write_json(config, path, auto_unbox = T)
  expect_error(run_conalco_daily(T, path, client), 'Missing current')
  expect_equal(requests, 0L)
  now = as.POSIXct('2026-10-23 06:59:00', tz = 'UTC')
  expect_identical(run_conalco_daily(F, path, client, now), 'before_schedule')
  expect_equal(requests, 0L)
  expect_equal(berlin_date('2026-10-25T23:30:00.000000Z'), '2026-10-26')
})

test_that('retry responses cannot exceed the archived request budget', {
  local_dir(daily_test_root())
  state = new.env()
  state$cache = new.env()
  state$calls = 0L
  id = 'catalog_refresh_budget_test'
  directory = ensure_dir(run_dir(id))
  client = \(url) {
    state$calls = state$calls + 1L
    list(request_url = url, response_status = 503L, headers = list(),
      observed_at_utc = utc_now(), body = raw())
  }
  expect_error(refresh_fetch('https://www.conalco.de/synthetic-budget/',
      'category', id, state, directory, client, sleep = \(seconds) NULL,
      max_requests = 1L), 'Bounded refresh budget')
  expect_equal(state$calls, 1L)
  expect_length(list.files(directory, '^[0-9]+_category[.]json$'), 1L)
})

test_that('the daily runner reviews, validates, commits and backs up once', {
  local_dir(daily_test_root())
  today = format(Sys.time(), '%Y-%m-%d', tz = 'Europe/Berlin')
  calls = new.env()
  calls$urls = character()
  client = \(url) {
    calls$urls = c(calls$urls, url)
    body = if (endsWith(url, '/agb')) '<main>Synthetic reviewed terms</main>'
      else paste0('<nav><a class="main-navigation-link" ',
        'href="/ready-to-drink/">RTD</a></nav>')
    list(request_url = url, final_url = url, observed_at_utc = utc_now(),
      response_status = if (endsWith(url, '/robots.txt')) 404L else 200L,
      headers = list(), body = charToRaw(body), error = NA_character_)
  }
  collector = \(id, guarded_client, dynamic = T, max_requests, max_hours) {
    expect_equal(max_requests, 1500L)
    expect_equal(max_hours, 4)
    urls = c('https://www.conalco.de/ready-to-drink/',
      'https://www.conalco.de/Ready-to-Serve')
    directory = run_dir(id)
    metadata = archive_response(list(request_url = urls[1],
        response_status = 200L, observed_at_utc = utc_now(),
        body = charToRaw('<main><div data-listing="true"></div></main>')),
      id, 'category', 4L)
    products = daily_fixture_products(id, today, abv = c(.5, 5))
    products$listing_metadata_file = metadata$metadata_file
    products$detail_metadata_file = NA_character_
    products$detail_coverage_method =
      'same_product_full_description_embedded_in_listing'
    parsed_file = file.path(directory, 'page.rds')
    saveRDS(list(links = data.frame(category_url = urls,
          decision = 'crawl')), parsed_file)
    pages = lapply(urls, \(url) list(url = url, page = 1L, last = 1L,
        status = 'ok', terminal_page_verified = T, ids =
          products$retailer_product_id, metadata_file = metadata$metadata_file,
        parsed_file = parsed_file))
    names(pages) = paste(urls, 1L, sep = '|')
    saveRDS(list(stage = 'complete_pending_scope_validation', pages = pages),
      file.path(directory, 'checkpoint.rds'))
    output = ensure_dir(file.path('data/processed/catalog_refresh', id))
    write_csv_once(products, file.path(output, 'products_collected.csv'))
    write_csv_once(data.frame(category_url = rep(urls, each = 2),
        retailer_product_id = rep(products$retailer_product_id, 2)),
      file.path(output, 'category_memberships.csv'))
    write_csv_once(data.frame(category_url = urls,
        category_status = 'complete_observed_pagination',
        pagination_stability_status = 'not_rechecked',
        expected_pages = 1L, successful_pages = 1L, unresolved_details = 0L),
      file.path(output, 'category_completeness.csv'))
    write_json_once(list(output_directory = output, ended_at_utc = utc_now()),
      file.path(directory, 'collection_finished.json'))
    write_json_once(list(scope = 'Synthetic two-category complete scope'),
      file.path(directory, 'collection_scope.json'))
  }
  runner = run_conalco_daily
  environment(runner) = list2env(list(run_conalco_refresh = collector),
    parent = environment(run_conalco_daily))
  config = list(enabled = T, timezone = 'Europe/Berlin',
    start_date = today, scheduled_local_time = '09:00',
    collection_authorization = 'Synthetic explicit daily authorization',
    reviewed_terms_text_sha256 = digest('Synthetic reviewed terms',
      algo = 'sha256', serialize = F),
    backup_restore_verified = T, backup_directory = ensure_dir('backup'),
    max_raw_bytes = 1e7, max_archived_requests = 1500L,
    max_hours_per_invocation = 4)
  write_json(config, 'daily.json', auto_unbox = T)
  id = runner(T, 'daily.json', client)
  expect_length(calls$urls, 3L)
  expect_equal(nrow(read_conalco_daily_panel()), 2L)
  expect_true(file.exists(file.path('backup', paste0(id, '.tar.gz.json'))))
  expect_identical(runner(T, 'daily.json', client), id)
  expect_length(calls$urls, 3L)
  expect_identical(read_json(file.path('logs/conalco_daily',
        paste0(today, '.json')))$status, 'complete')
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) DYNAMIC NAVIGATION, LOCAL GAPS AND IMMUTABLE PARTIAL OBSERVATIONS
# <-----------------------------------------------------------------------------

dynamic_test_root = \() {
  root = tempfile()
  dir.create(root)
  stopifnot(file.copy(file.path(APM_ROOT, 'R'), root, recursive = T))
  dir.create(file.path(root, 'scripts'))
  stopifnot(file.copy(file.path(APM_ROOT, 'scripts/refresh_conalco.R'),
      file.path(root, 'scripts')))
  root
}

dynamic_client = \(slugs, statuses = integer(), labels = slugs,
  category_ids = seq_along(slugs), date = '2026-10-22') {
  calls = new.env(parent = emptyenv())
  calls$urls = character()
  client = \(url) {
    calls$urls = c(calls$urls, url)
    slug = sub('/$', '', sub('^https://www.conalco.de/', '', url))
    index = match(slug, slugs)
    status = if (slug %in% names(statuses)) statuses[[slug]] else 200L
    body = if (endsWith(url, '/robots.txt')) '' else if (endsWith(url, '/agb'))
      '<main>Synthetic reviewed dynamic terms</main>' else if (url ==
        'https://www.conalco.de/') paste0('<nav>', paste0(
          '<a class="main-navigation-link" href="/', slugs, '/">',
          labels, '</a>', collapse = ''), '</nav>') else {
        if (is.na(index)) stop('Unplanned synthetic request: ', url)
        info = toJSON(list(id = sprintf('%032x', category_ids[index]),
            name = 'Fiktives Bier - 0,5L 5% vol'), auto_unbox = T)
        options = toJSON(list(dataUrl = paste0('/widgets/cms/navigation/',
              sprintf('%032x', category_ids[index]))), auto_unbox = T)
        paste0('<main><div data-listing="true" ',
          "data-listing-pagination-options='{\"page\":1}' ",
          "data-listing-options='", options, "'>",
          "<div data-product-information='", info, "'>",
          '<a class="product-name" href="https://www.conalco.de/test">',
          'Fiktives Bier</a><span class="product-price">1,99 €</span>',
          '<span class="price-unit-content">0,5 l</span>',
          '<div class="product-description">Bier. Alkoholgehalt: 5% vol. ',
          paste(rep('Fiktive vollstaendige Produktbeschreibung.', 4),
            collapse = ' '), '</div></div></div></main>')
      }
    if (endsWith(url, '/robots.txt')) status = 404L
    if (!is.na(status) && status != 200L) body = '<main>Fixture error</main>'
    list(request_url = url, final_url = url, response_status = status,
      observed_at_utc = paste0(date, 'T08:00:00.000000Z'),
      headers = if (identical(status, 302L)) list(location = paste0('/',
          slug, '/')) else list(), body = if (is.na(status)) NULL else
        charToRaw(body), error = if (is.na(status)) 'Synthetic timeout' else NA)
  }
  list(client = client, calls = calls)
}

dynamic_prepare = \(id, client, date = '2026-10-22') {
  directory = ensure_dir(run_dir(id))
  write_json_once(list(run_id = id,
      started_at_utc = paste0(date, 'T07:00:00.000000Z')),
    file.path(directory, 'started.json'))
  terms = archive_response(client('https://www.conalco.de/agb'), id,
    'terms', 1L)
  archive_response(client('https://www.conalco.de/'), id, 'navigation', 2L)
  write_json_once(list(decision = 'Synthetic dynamic access review',
      user_authorization = 'Synthetic explicit test authorization',
      terms_metadata = terms$metadata_file,
      terms_raw_sha256 = terms$raw_sha256),
    file.path(directory, 'access_review.json'))
}

test_that('new categories come only from fresh navigation and its closure', {
  local_dir(dynamic_test_root())
  fixture = dynamic_client(c('bier-neu', 'wein-neu'))
  dynamic_prepare('new_categories', fixture$client)
  seeds = refresh_scope_seeds(run_dir('new_categories'), dynamic = T)
  expect_setequal(seeds$category_url, paste0('https://www.conalco.de/',
      c('bier-neu/', 'wein-neu/')))
  expect_false('https://www.conalco.de/gin/' %in% seeds$category_url)
  run_conalco_refresh('new_categories', fixture$client, dynamic = T)
  path = validate_conalco_refresh('new_categories', '')
  expect_silent(require_complete_catalog(path))
  proof = read_json(path)$proof
  discovery = read_json(proof$navigation_discovery$path, simplifyVector = T)
  expect_true(discovery$exhausted)
  expect_true(all(discovery$changes$status == 'new_in_navigation'))
  append_conalco_daily(path)
  expect_equal(nrow(read_conalco_daily_panel()), 2L)
})

test_that('removed and renamed categories need stable navigation evidence', {
  local_dir(dynamic_test_root())
  first = dynamic_client(c('bier-alt', 'entfernt'),
    category_ids = c(1L, 2L), date = '2026-10-21')
  dynamic_prepare('day_20261021', first$client, '2026-10-21')
  run_conalco_refresh('day_20261021', first$client, dynamic = T)
  old = validate_conalco_refresh('day_20261021', '')
  append_conalco_daily(old)
  second = dynamic_client(c('bier-neu', 'neu'), category_ids = c(1L, 3L))
  dynamic_prepare('day_20261022', second$client)
  run_conalco_refresh('day_20261022', second$client, dynamic = T)
  path = validate_conalco_refresh('day_20261022', '')
  changes = read_json(read_json(path)$proof$navigation_discovery$path,
    simplifyVector = T)$changes
  expect_setequal(changes$status, c('url_changed_same_category_id',
      'new_in_navigation', 'removed_from_navigation'))
  expect_false(any(grepl('/entfernt/', second$calls$urls, fixed = T)))
  append_conalco_daily(path)
  append_conalco_daily(path)
  expect_equal(nrow(read_conalco_daily_panel()), 4L)
  third = dynamic_client(c('bier-neu', 'neu'), labels = c('Bier anders', 'neu'),
    category_ids = c(1L, 3L), date = '2026-10-23')
  dynamic_prepare('day_20261023', third$client, '2026-10-23')
  run_conalco_refresh('day_20261023', third$client, dynamic = T)
  path = validate_conalco_refresh('day_20261023', '')
  changes = read_json(read_json(path)$proof$navigation_discovery$path,
    simplifyVector = T)$changes
  expect_true('renamed_label' %in% changes$status)
  expect_silent(require_complete_catalog(path))
})

test_that('self redirects, 404s and temporary gaps preserve other categories', {
  for (status in c(302L, 404L, 503L, NA_integer_)) {
    root = dynamic_test_root()
    with_dir(root, {
      id = 'local_gap'
      fixture = dynamic_client(c('bier', 'fehler', 'wein'),
        statuses = c(fehler = status))
      dynamic_prepare(id, fixture$client)
      run_conalco_refresh(id, fixture$client, dynamic = T)
      state = readRDS(file.path(run_dir(id), 'checkpoint.rds'))
      expect_identical(state$stage, 'partial')
      expect_false(file.exists(file.path(run_dir(id),
          'collection_finished.json')))
      expect_equal(sum(endsWith(fixture$calls$urls, '/fehler/')), 1L)
      expect_true(any(endsWith(fixture$calls$urls, '/wein/')))
      manifest = refresh_store_partial(id)
      receipt = read_json(manifest)
      expect_identical(receipt$status, 'partial')
      expect_false(receipt$price_statistics_calculated)
      expect_equal(receipt$n_products, 2L)
      expect_identical(refresh_store_partial(id), manifest)
      expect_error(append_conalco_daily(manifest), 'status is not complete')
      expect_false(file.exists(file.path(CONALCO_DAILY_ROOT, 'panel.csv.gz')))
      expect_false(file.exists(file.path(CONALCO_DAILY_ROOT, 'statistics.csv')))
      changes = read.csv(receipt$proof$navigation_changes$path)
      expect_true('linked_fetch_gap' %in% changes$status)
      before = lapply(state$pages, \(p) digest(file = p$parsed_file,
          algo = 'sha256'))
      resumed = dynamic_client(c('bier', 'fehler', 'wein'))
      archived = length(list.files(run_dir(id),
          '^[0-9]+_[a-z]+[.]json$'))
      expect_error(run_conalco_refresh(id, resumed$client, dynamic = T,
          max_requests = archived, max_hours = 4), 'Bounded refresh budget')
      expect_length(resumed$calls$urls, 0L)
      run_conalco_refresh(id, resumed$client, dynamic = T,
        max_requests = archived + 1L, max_hours = 4)
      expect_identical(resumed$calls$urls, 'https://www.conalco.de/fehler/')
      after = readRDS(file.path(run_dir(id), 'checkpoint.rds'))
      for (key in names(before)[!grepl('/fehler/', names(before), fixed = T)])
        expect_identical(digest(file = after$pages[[key]]$parsed_file,
            algo = 'sha256'), before[[key]])
      path = validate_conalco_refresh(id, '')
      expect_silent(require_complete_catalog(path))
      append_conalco_daily(path)
      append_conalco_daily(path)
      expect_equal(nrow(read_conalco_daily_panel()), 3L)
      expect_true(file.exists(manifest))
    })
  }
})

test_that('access blocks and rate limits stop before the next category', {
  for (status in c(403L, 429L)) with_dir(dynamic_test_root(), {
    fixture = dynamic_client(c('bier', 'gesperrt', 'wein'),
      statuses = c(gesperrt = status))
    dynamic_prepare('global_stop', fixture$client)
    expect_error(run_conalco_refresh('global_stop', fixture$client,
        dynamic = T),
      'Access/rate-limit stop', class = 'conalco_global_error')
    expect_false(any(endsWith(fixture$calls$urls, '/wein/')))
    expect_equal(sum(endsWith(fixture$calls$urls, '/gesperrt/')), 1L)
    path = refresh_store_partial('global_stop')
    expect_equal(read_json(path)$n_products, 1L)
  })
})

test_that('the daily runner saves partial days without publishing statistics', {
  local_dir(dynamic_test_root())
  today = format(Sys.time(), '%Y-%m-%d', tz = 'Europe/Berlin')
  fixture = dynamic_client(c('bier', 'fehler', 'wein'),
    statuses = c(fehler = 302L), date = today)
  config = list(enabled = T, timezone = 'Europe/Berlin', start_date = today,
    scheduled_local_time = '09:00',
    collection_authorization = 'Synthetic explicit dynamic daily approval',
    reviewed_terms_text_sha256 = digest('Synthetic reviewed dynamic terms',
      algo = 'sha256', serialize = F), backup_restore_verified = T,
    backup_directory = ensure_dir('backup'), max_raw_bytes = 1e7)
  write_json(config, 'daily.json', auto_unbox = T)
  id = run_conalco_daily(T, 'daily.json', fixture$client)
  record = read_json(file.path('logs/conalco_daily', paste0(today, '.json')))
  expect_identical(record$status, 'partial')
  expect_true(file.exists(record$partial_manifest$path))
  expect_false(file.exists(file.path(CONALCO_DAILY_ROOT, 'panel.csv.gz')))
  expect_false(file.exists(file.path(CONALCO_DAILY_ROOT, 'statistics.csv')))
  expect_length(fixture$calls$urls, 6L)
  resumed = dynamic_client(c('bier', 'fehler', 'wein'), date = today)
  expect_identical(run_conalco_daily(T, 'daily.json', resumed$client), id)
  expect_identical(resumed$calls$urls, 'https://www.conalco.de/fehler/')
  expect_equal(nrow(read_conalco_daily_panel()), 3L)
  final = read_json(file.path('logs/conalco_daily', paste0(today, '.json')))
  expect_identical(final$status, 'complete')
  expect_null(final$partial_manifest)
})

test_that('fresh child navigation and containers prove their scope', {
  local_dir(dynamic_test_root())
  fixture = dynamic_client(c('mehr', 'bier-neu'))
  client = \(url) {
    response = fixture$client(url)
    if (url == 'https://www.conalco.de/') {
      response$body = charToRaw(paste0('<nav><a class="main-navigation-link"',
        ' href="/mehr/">Mehr</a></nav>'))
    } else if (url == 'https://www.conalco.de/mehr/') {
      response$body = charToRaw(paste0('<main><div class="cms-block-html">',
        '<ul class="category-navigation">',
        '<li><a class="category-navigation-link" aria-current="page" ',
        'href="/mehr/">Mehr</a><ul><li><a href="/bier-neu/">Bier neu</a>',
        '</li></ul></li></ul></div></main>'))
    }
    response
  }
  dynamic_prepare('nested', client)
  expect_equal(nrow(refresh_scope_seeds(run_dir('nested'))), 1L)
  run_conalco_refresh('nested', client)
  expect_true(any(endsWith(fixture$calls$urls, '/bier-neu/')))
  path = validate_conalco_refresh('nested', '')
  coverage = read.csv(read_json(path)$proof$category_completeness$path)
  expect_true('navigation_container_verified' %in% coverage$category_status)
  expect_silent(require_complete_catalog(path))
  append_conalco_daily(path)
  expect_equal(nrow(read_conalco_daily_panel()), 1L)
})

test_that('a missing primary listing is a gap, never a navigation container', {
  local_dir(dynamic_test_root())
  fixture = dynamic_client(c('bier', 'wein'))
  client = \(url) {
    response = fixture$client(url)
    if (endsWith(url, '/bier/')) response$body = charToRaw(paste0(
      '<main><div class="cms-block-product-listing"></div>',
      '<ul class="category-navigation"><li><a ',
      'class="category-navigation-link" aria-current="page" ',
      'href="/bier/">Bier</a><ul><li><a href="/wein/">Wein</a>',
      '</li></ul></li></ul></main>'))
    response
  }
  dynamic_prepare('missing_primary', client)
  run_conalco_refresh('missing_primary', client)
  state = readRDS(file.path(run_dir('missing_primary'), 'checkpoint.rds'))
  expect_identical(state$stage, 'partial')
  expect_equal(nrow(refresh_auto_navigation_review(state)), 0L)
  expect_false(file.exists(file.path(run_dir('missing_primary'),
      'collection_finished.json')))
  expect_equal(read_json(refresh_store_partial('missing_primary'))$n_products,
    1L)
})

test_that('an incomplete traversal cannot prove category removal', {
  local_dir(dynamic_test_root())
  first = dynamic_client(c('bier', 'alt'), date = '2026-10-21')
  dynamic_prepare('before_gap', first$client, '2026-10-21')
  run_conalco_refresh('before_gap', first$client)
  validate_conalco_refresh('before_gap', '')
  second = dynamic_client(c('bier', 'fehler'), statuses = c(fehler = 302L))
  dynamic_prepare('with_gap', second$client)
  run_conalco_refresh('with_gap', second$client)
  path = refresh_store_partial('with_gap')
  changes = read.csv(read_json(path)$proof$navigation_changes$path)
  expect_false('removed_from_navigation' %in% changes$status)
  expect_true('not_rediscovered_unresolved' %in% changes$status)
  receipt = read_json(path)
  cat('changed', file = receipt$proof$observations_PARTIAL$path, append = T)
  expect_error(refresh_store_partial('with_gap'))
})

test_that('a response crossing midnight is archived outside the day panel', {
  local_dir(dynamic_test_root())
  fixture = dynamic_client(c('bier', 'wein'))
  dynamic_prepare('date_boundary', fixture$client)
  client = \(url) {
    response = fixture$client(url)
    response$observed_at_utc = '2026-10-22T22:00:01.000000Z'
    response
  }
  expect_error(run_conalco_refresh('date_boundary', client),
    'Berlin date changed', class = 'conalco_global_error')
  expect_equal(sum(endsWith(fixture$calls$urls, '/wein/')), 0L)
  state = readRDS(file.path(run_dir('date_boundary'), 'checkpoint.rds'))
  expect_length(state$pages, 0L)
  expect_length(list.files(run_dir('date_boundary'),
      '^[0-9]+_category[.]json$'), 1L)
})

test_that('an HTTP 200 access challenge stops the collector globally', {
  local_dir(dynamic_test_root())
  fixture = dynamic_client(c('bier', 'wein'))
  dynamic_prepare('challenge', fixture$client)
  client = \(url) {
    response = fixture$client(url)
    response$body = charToRaw('<html><title>Access denied</title></html>')
    response
  }
  expect_error(run_conalco_refresh('challenge', client),
    'Access/rate-limit stop', class = 'conalco_global_error')
  expect_false(any(endsWith(fixture$calls$urls, '/wein/')))
})

test_that('a new day can be complete after a linked category disappears', {
  local_dir(dynamic_test_root())
  first = dynamic_client(c('bier', 'fehler'), statuses = c(fehler = 302L),
    date = '2026-10-21')
  dynamic_prepare('partial_yesterday', first$client, '2026-10-21')
  run_conalco_refresh('partial_yesterday', first$client)
  partial = refresh_store_partial('partial_yesterday')
  second = dynamic_client('bier')
  dynamic_prepare('complete_today', second$client)
  run_conalco_refresh('complete_today', second$client)
  path = validate_conalco_refresh('complete_today', '')
  changes = read_json(read_json(path)$proof$navigation_discovery$path,
    simplifyVector = T)$changes
  expect_true('removed_from_navigation' %in% changes$status)
  expect_false(any(endsWith(second$calls$urls, '/fehler/')))
  append_conalco_daily(path)
  panel = read_conalco_daily_panel()
  expect_equal(nrow(panel), 1L)
  expect_true(all(panel$daily_date == '2026-10-22'))
  expect_identical(read_json(partial)$date, '2026-10-21')
})

test_that('daily route transfer uses verified identities, never old prices', {
  local_dir(dynamic_test_root())
  fixture = dynamic_client('bier')
  logical = 'https://www.conalco.de/bier/'
  actual = paste0('https://www.conalco.de/widgets/cms/navigation/',
    sprintf('%032x', 1L), '?p=1')
  route = list(category_url = logical,
    request_url_template = sub('p=1$', 'p={page}', actual),
    fresh_success_metadata = 'must-not-transfer',
    historical_success_metadata = 'must-not-transfer')
  client = \(url) {
    response = fixture$client(if (url == actual) logical else url)
    response$request_url = url
    response$final_url = url
    response
  }
  dynamic_prepare('route_source', client)
  routes_path = file.path(run_dir('route_source'),
    'category_endpoint_routes_test.json')
  write_json_once(list(routes = list(route)), routes_path)
  run_conalco_refresh('route_source', client)
  manifest = validate_conalco_refresh('route_source', '')
  config = list(approved_listing_routes = list(
      source_manifest = refresh_file_record(manifest),
      routes = refresh_file_record(routes_path)))
  destination = ensure_dir(run_dir('route_next_day'))
  before = fixture$calls$urls
  path = daily_prepare_listing_routes(config, destination)
  transferred = read_json(path)
  expect_length(transferred$routes, 1L)
  expect_identical(transferred$routes[[1]]$category_url, logical)
  expect_null(transferred$routes[[1]]$fresh_success_metadata)
  expect_null(transferred$routes[[1]]$historical_success_metadata)
  expect_identical(fixture$calls$urls, before)
  expect_false(any(grepl('price|products|observations',
      list.files(destination))))
  original = digest(file = path, algo = 'sha256')
  expect_identical(daily_prepare_listing_routes(config, destination), path)
  expect_identical(digest(file = path, algo = 'sha256'), original)
  expect_null(daily_prepare_listing_routes(list(), destination))
  cat('changed', file = routes_path, append = T)
  expect_error(daily_prepare_listing_routes(config, destination),
    'evidence changed or is missing')
})

test_that('static galleries retain every card without inventing pagination', {
  for (paginated in c(F, T)) with_dir(dynamic_test_root(), {
    fixture = dynamic_client(c('bier', 'galerie'), category_ids = c(1L, 1L))
    client = \(url) {
      response = fixture$client(url)
      if (endsWith(url, '/galerie/')) {
        doc = read_html(response$body)
        node = xml_find_first(doc, '//*[@data-listing="true"]')
        xml_set_attr(node, 'data-listing', NA_character_)
        xml_set_attr(node, 'class',
          'cms-block-product-slider cms-element-product-slider')
        if (paginated) xml_add_child(node, 'input', name = 'p', value = '2')
        response$body = charToRaw(as.character(doc))
      }
      response
    }
    id = if (paginated) 'gallery_gap' else 'gallery_complete'
    dynamic_prepare(id, client)
    run_conalco_refresh(id, client)
    if (paginated) {
      expect_false(file.exists(file.path(run_dir(id),
          'collection_finished.json')))
      expect_equal(nrow(refresh_auto_navigation_review(readRDS(file.path(
              run_dir(id), 'checkpoint.rds')))), 0L)
    } else {
      path = validate_conalco_refresh(id, '')
      coverage = read.csv(read_json(path)$proof$category_completeness$path)
      expect_true('navigation_container_verified' %in%
          coverage$category_status)
      expect_equal(read_json(path)$n_unique_products, 1L)
      expect_match(read_json(path)$scope$exclusion_effect,
        'No navigation URL excluded')
      expect_silent(require_complete_catalog(path))
    }
  })
})

test_that('mixed landing tiles require every direct product observed fresh', {
  for (observed in c(F, T)) with_dir(dynamic_test_root(), {
    fixture = dynamic_client(c('bier', 'landing'))
    tile = 'https://www.conalco.de/Fiktiv-Likoer-0-5L-15-vol'
    client = \(url) {
      response = fixture$client(url)
      if (endsWith(url, '/landing/')) {
        response$body = charToRaw(paste0('<main><div class="cms-block-html">',
          '<a href="/bier/">Bier</a>',
          '<a href="/eingelegte-fruechte/">Eingelegte Früchte</a>',
          '<a href="', tile, '">Fiktiver Likör</a></div></main>'))
      } else if (observed && endsWith(url, '/bier/')) {
        response$body = charToRaw(gsub('https://www.conalco.de/test', tile,
          rawToChar(response$body), fixed = T))
      }
      response
    }
    id = if (observed) 'tile_complete' else 'tile_gap'
    dynamic_prepare(id, client)
    run_conalco_refresh(id, client)
    if (observed) {
      path = validate_conalco_refresh(id, '')
      expect_silent(require_complete_catalog(path))
      expect_equal(read_json(path)$n_unique_products, 1L)
    } else {
      expect_false(file.exists(file.path(run_dir(id),
          'collection_finished.json')))
      expect_equal(nrow(refresh_auto_navigation_review(readRDS(file.path(
              run_dir(id), 'checkpoint.rds')))), 0L)
    }
    expect_false(any(grepl('eingelegte-fruechte|Fiktiv-Likoer',
          fixture$calls$urls)))
  })
})

test_that('the default refresh client can be constructed without requests', {
  expect_true(is.function(eval(formals(run_conalco_refresh)$client)))
})

test_that('unrendered CMS pages cannot prove a complete empty category', {
  for (kind in c('cms-block-html', 'cms-block-product-slider'))
    with_dir(dynamic_test_root(), {
      fixture = dynamic_client(c('bier', 'landing'))
      client = \(url) {
        response = fixture$client(url)
        if (endsWith(url, '/landing/')) response$body = charToRaw(paste0(
          '<main><div class="', kind, '"></div></main>'))
        response
      }
      dynamic_prepare('unrendered', client)
      run_conalco_refresh('unrendered', client)
      expect_false(file.exists(file.path(run_dir('unrendered'),
          'collection_finished.json')))
      state = readRDS(file.path(run_dir('unrendered'), 'checkpoint.rds'))
      expect_length(state$failures, 0L)
      expect_equal(nrow(refresh_auto_navigation_review(state)), 0L)
    })
})

test_that('conflicting repeated gallery cards never receive full validation', {
  local_dir(dynamic_test_root())
  fixture = dynamic_client(c('bier', 'galerie'), category_ids = c(1L, 1L))
  client = \(url) {
    response = fixture$client(url)
    if (endsWith(url, '/galerie/')) {
      doc = read_html(response$body)
      node = xml_find_first(doc, '//*[@data-listing="true"]')
      xml_set_attr(node, 'data-listing', NA_character_)
      xml_set_attr(node, 'class',
        'cms-block-product-slider cms-element-product-slider')
      card = xml_find_first(node, './/*[@data-product-information]')
      repeated = xml_add_child(node, card, .copy = T)
      xml_set_text(xml_find_first(repeated,
        './/*[contains(@class,"product-price")]'), '9,99 €')
      response$body = charToRaw(as.character(doc))
    }
    response
  }
  dynamic_prepare('conflict_gallery', client)
  run_conalco_refresh('conflict_gallery', client)
  expect_error(validate_conalco_refresh('conflict_gallery', ''))
  expect_false(file.exists(file.path(run_dir('conflict_gallery'),
      'manifest.json')))
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) APPROVED CONTRACT CONTENT AND THE FINAL STAND METADATA
# <-----------------------------------------------------------------------------

terms_test_page = \(stand = '01.01.2026, 00:00:00',
  clause = 'Synthetic contract date: 01.01.2025.') paste0(
  '<main id="terms"><h1>Allgemeine Geschäftsbedingungen</h1>',
  '<section><p>', clause, '</p></section>',
  '<div class="terms-stand">Stand: ', stand, '</div></main>')

terms_test_archive = \(id, body, status = 200L) archive_response(list(
  request_url = 'https://www.conalco.de/agb',
  final_url = 'https://www.conalco.de/agb', response_status = status,
  observed_at_utc = '2026-10-23T07:00:00.000000Z', headers = list(),
  body = charToRaw(body), error = NA_character_), id, 'terms', 1L)

terms_test_config = \(reference) list(
  reviewed_terms_text_sha256 = daily_terms_hash(reference$metadata_file),
  reviewed_terms_reference = refresh_file_record(reference$metadata_file),
  terms_stand_change_authorization = 'Synthetic explicit Stand-only approval')

test_that('only final Stand metadata changes; approval and raw bytes persist', {
  local_dir(dynamic_test_root())
  reference = terms_test_archive('reference', terms_test_page())
  config = terms_test_config(reference)
  original = config
  first = terms_test_archive('first', terms_test_page('02.01.2026, 12:34:56'))
  raw_hash = digest(file = first$raw_file, algo = 'sha256')
  receipt = daily_check_terms(config, first$metadata_file, run_dir('first'),
    'events.jsonl')
  expect_true(receipt$stand_changed)
  expect_identical(receipt$previous_stand, 'Stand: 01.01.2026, 00:00:00')
  expect_identical(receipt$current_stand, 'Stand: 02.01.2026, 12:34:56')
  expect_false(identical(receipt$full_text_sha256,
      config$reviewed_terms_text_sha256))
  expect_identical(receipt$content_sha256,
    daily_terms_document(reference$metadata_file)$content_sha256)
  expect_identical(config, original)
  expect_identical(digest(file = first$raw_file, algo = 'sha256'), raw_hash)
  journal = readLines('events.jsonl')
  expect_identical(fromJSON(journal[1])$event, 'terms_stand_changed')
  expect_identical(fromJSON(journal[1])$new_stand, receipt$current_stand)
  expect_identical(daily_check_terms(config, first$metadata_file,
      run_dir('first'), 'events.jsonl'), receipt)
  expect_identical(readLines('events.jsonl'), journal)
  same = terms_test_archive('same', terms_test_page('02.01.2026, 12:34:56'))
  unchanged = daily_check_terms(config, same$metadata_file, run_dir('same'),
    'events.jsonl')
  expect_false(unchanged$stand_changed)
  next_day = terms_test_archive('next', terms_test_page('03.01.2026, 01:02:03'))
  changed = daily_check_terms(config, next_day$metadata_file, run_dir('next'),
    'events.jsonl')
  expect_identical(changed$previous_stand, receipt$current_stand)
  expect_true(changed$stand_changed)
  expect_identical(changed$reference_stand, 'Stand: 01.01.2026, 00:00:00')
  expect_identical(config, original)
})

test_that('clause, date and document changes cannot use the Stand exception', {
  local_dir(dynamic_test_root())
  reference = terms_test_archive('reference', terms_test_page())
  config = terms_test_config(reference)
  page = terms_test_page('02.01.2026, 00:00:00')
  cases = c(
    clause = terms_test_page(clause = 'Different contractual obligations.'),
    contract_date = terms_test_page(clause =
      'Synthetic contract date: 02.01.2025.'),
    missing_stand = sub('<div class="terms-stand">.*</div>', '', page),
    duplicate_stand = sub('</main>', paste0(
        '<div>Stand: 03.01.2026, 00:00:00</div></main>'), page, fixed = T),
    missing_main = gsub('main', 'article', page, fixed = T),
    duplicate_main = paste0(page, '<main>Unexpected second document</main>'),
    moved_stand = sub('</main>', '<p>New trailing content</p></main>', page,
      fixed = T),
    invalid_date = terms_test_page('31.02.2026, 00:00:00'),
    invalid_time = terms_test_page('02.01.2026, 25:00:00'),
    changed_structure = sub('<section>', '<section class="changed">', page,
      fixed = T),
    nested_stand = sub('Stand: ', '<span>Stand: </span>', page, fixed = T),
    empty_contract = '<main><div>Stand: 02.01.2026, 00:00:00</div></main>',
    unexpected_empty_node = sub('</main>', '<aside></aside></main>', page,
      fixed = T))
  for (name in names(cases)) {
    current = terms_test_archive(name, cases[[name]])
    expect_error(daily_check_terms(config, current$metadata_file,
        run_dir(name), 'events.jsonl'))
    expect_false(file.exists(file.path(run_dir(name),
        'terms_comparison.json')), info = name)
  }
  rejected = lapply(readLines('events.jsonl'), fromJSON)
  expect_true(all(vapply(rejected, \(entry)
      identical(entry$event, 'terms_comparison_rejected'), logical(1))))
  changed = Filter(\(entry) entry$stand_changed, rejected)
  expect_length(changed, 2L)
  expect_true(all(vapply(changed, \(entry) identical(entry$previous_stand,
        'Stand: 01.01.2026, 00:00:00') &&
        identical(entry$new_stand, 'Stand: 02.01.2026, 00:00:00'), logical(1))))
  for (status in c(403L, 404L, 429L)) {
    id = paste0('status', status)
    current = terms_test_archive(id, page, status)
    expect_error(daily_check_terms(config, current$metadata_file,
        run_dir(id), 'events.jsonl'), 'Missing or unexpected terms response')
  }
})

test_that('the original full-hash approval remains binding', {
  local_dir(dynamic_test_root())
  reference = terms_test_archive('reference', terms_test_page())
  config = terms_test_config(reference)
  original = config
  current = terms_test_archive('current',
    terms_test_page('02.01.2026, 00:00:00'))
  no_authorization = config
  no_authorization$terms_stand_change_authorization = NULL
  expect_error(daily_check_terms(no_authorization, reference$metadata_file,
      run_dir('reference'), 'events.jsonl'), 'explicit authorization')
  replacement = config
  replacement$reviewed_terms_text_sha256 =
    daily_terms_hash(current$metadata_file)
  expect_error(daily_check_terms(replacement, reference$metadata_file,
      run_dir('reference'), 'events.jsonl'), 'reference does not match')
  config$reviewed_terms_reference$sha256 = paste(rep('0', 64), collapse = '')
  expect_error(daily_check_terms(config, reference$metadata_file,
      run_dir('reference'), 'events.jsonl'), 'reference is missing or changed')
  legacy = list(reviewed_terms_text_sha256 = daily_terms_hash(
      reference$metadata_file))
  expect_silent(daily_check_terms(legacy, reference$metadata_file,
      run_dir('reference'), 'events.jsonl'))
  expect_error(daily_check_terms(legacy, current$metadata_file,
      run_dir('current'), 'events.jsonl'), 'Terms changed')
  cat('tampered', file = reference$raw_file, append = T)
  expect_error(daily_check_terms(original, reference$metadata_file,
      run_dir('reference'), 'events.jsonl'), 'Archive hash mismatch')
})

test_that('Stand-only days preserve observed prices, panels and statistics', {
  modes = c('stand', 'clause', 'missing', 'duplicate', 'structure')
  for (mode in modes) with_dir(dynamic_test_root(), {
    today = format(Sys.time(), '%Y-%m-%d', tz = 'Europe/Berlin')
    fixture = dynamic_client('bier', date = today)
    reference = terms_test_archive('approved', terms_test_page())
    config = c(terms_test_config(reference), list(enabled = T,
        timezone = 'Europe/Berlin', start_date = today,
        scheduled_local_time = '09:00', backup_restore_verified = T,
        backup_directory = ensure_dir('backup'), max_raw_bytes = 1e7,
        collection_authorization = 'Synthetic approved daily collection'))
    client = \(url) {
      response = fixture$client(url)
      if (endsWith(url, '/agb')) {
        body = terms_test_page('02.01.2026, 00:00:00',
          clause = if (mode == 'clause') 'Different contractual obligations.'
          else 'Synthetic contract date: 01.01.2025.')
        if (mode == 'missing') body = sub('<div.*</div>', '', body)
        if (mode == 'duplicate') body = sub('</main>', paste0(
            '<div>Stand: 03.01.2026, 00:00:00</div></main>'), body, fixed = T)
        if (mode == 'structure') body = sub('id="terms"',
          'id="unexpected"', body, fixed = T)
        response$body = charToRaw(body)
      }
      response
    }
    write_json(config, 'daily.json', auto_unbox = T)
    if (mode != 'stand') {
      expect_error(run_conalco_daily(T, 'daily.json', client),
        'Terms clauses changed|Stand value|Terms structure changed')
      expect_length(fixture$calls$urls, 3L)
      expect_false(file.exists(file.path(CONALCO_DAILY_ROOT, 'panel.csv.gz')))
      expect_false(file.exists(file.path(CONALCO_DAILY_ROOT, 'statistics.csv')))
    } else {
      id = run_conalco_daily(T, 'daily.json', client)
      expect_length(fixture$calls$urls, 4L)
      day = read_conalco_daily_panel()
      expect_equal(nrow(day), 1L)
      expect_equal(as.numeric(day$price_current), 1.99)
      expect_equal(as.numeric(day$price_eur_per_l), 3.98)
      expect_equal(as.numeric(day$price_eur_per_10g),
        1.99 * 10 / (0.5 * 1000 * 5 / 100 * 0.789))
      statistics = daily_read_csv(file.path(CONALCO_DAILY_ROOT,
          'statistics.csv'))
      expect_equal(nrow(statistics), 8L)
      expect_true(all(statistics$run_id == id))
      expect_identical(read_json(file.path('logs/conalco_daily',
            paste0(today, '.json')))$status, 'complete')
      expect_true(read_json(file.path(run_dir(id),
            'terms_comparison.json'))$stand_changed)
    }
  })
})
