# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261022

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) DAILY ANALYSIS; HISTORICAL CLASSIFICATION RULES REMAIN VERSIONED
# <-----------------------------------------------------------------------------

CONALCO_DAILY_VERSION = 'conalco-daily-0.1.0'
CONALCO_DAILY_ROOT = Sys.getenv('APM_DAILY_ROOT',
  'data/processed/exports/conalco_daily')

daily_analyse_products = \(products, conflict_review = NULL) {
  x = products %>% classify_products() %>% quality_checks() %>%
    assign_price_categories()
  x = snapshot_source_conflicts(x, conflict_review)
  x = snapshot_intermediate_evidence(snapshot_product_style(x))
  x$population_status = case_when(
    is.na(x$alcohol_content) ~ 'missing_abv',
    !is.finite(x$alcohol_content) | x$alcohol_content < 0 |
      x$alcohol_content > 100 ~ 'invalid_abv',
    x$alcohol_content < 0.5 ~ 'below_0.5',
    x$scope_status != 'included' ~ paste0('scope:', x$scope_status),
    TRUE ~ 'included_ge_0.5'
  )
  # main_category is descriptive, including unresolved RTD and No/Lo families.
  # Eligibility must not reapply the historical <=1.2 category separation.
  checks = x
  checks$analysis_category = x$main_category
  checks = price_eligibility(checks, 'include_unknown', 'beverage')
  x$daily_exclusion_reasons = vapply(seq_len(nrow(x)), \(i) {
    reasons = checks$price_exclusion_reasons[i]
    if (x$population_status[i] != 'included_ge_0.5')
      reasons = add_flag(reasons, x$population_status[i])
    reasons
  }, character(1))
  x$price_analysis_eligible = !nzchar(x$daily_exclusion_reasons)
  x = standardize_alcohol_price(x)
  x$daily_price_eligible = x$price_analysis_eligible
  x$price_eur_per_l = ifelse(x$daily_price_eligible,
    x$price_current / x$volume_l, NA_real_)
  x$daily_analysis_version = CONALCO_DAILY_VERSION
  x$minimum_abv_percent = 0.5
  x$ethanol_density_g_ml = ETHANOL_DENSITY_G_ML
  x$daily_date = berlin_date(x$observed_at)
  x$daily_category_exact15 = analysis_partition(x, 'primary_exact15')
  x$daily_category_tax_le15 = analysis_partition(x, 'tax_le15')
  x
}

daily_statistics = \(panel) {
  groups = split(panel, paste(panel$retailer, panel$daily_date,
      panel$run_id, sep = '|'))
  bind_rows(lapply(groups, \(day) {
    bind_rows(lapply(c('primary_exact15', 'tax_le15'), \(partition) {
      labels = day[[if (partition == 'primary_exact15')
        'daily_category_exact15' else 'daily_category_tax_le15']]
      labels[is.na(labels) | !nzchar(labels)] = 'unresolved_category'
      categories = unique(c('__all__', labels))
      bind_rows(lapply(categories, \(category) {
        rows = day[if (category == '__all__') rep(TRUE, nrow(day)) else
          labels == category, ]
        eligible = rows$daily_price_eligible %in% c(TRUE, 'TRUE')
        bind_rows(lapply(c('price_eur_per_10g', 'price_eur_per_l'), \(metric) {
          values = as.numeric(rows[[metric]][eligible])
          stopifnot(all(is.finite(values)), all(values > 0))
          snapshot_summary(values, nrow(rows)) %>% mutate(
            retailer = day$retailer[1], date = day$daily_date[1],
            run_id = day$run_id[1], partition = partition,
            category = category, price_measure = metric,
            n_unique_products = n_distinct(
              rows$retailer_product_id[eligible]),
            n_population = sum(rows$population_status == 'included_ge_0.5'),
            n_missing_abv = sum(rows$population_status == 'missing_abv'),
            n_below_0.5 = sum(rows$population_status == 'below_0.5'),
            n_invalid_abv = sum(rows$population_status == 'invalid_abv'),
            minimum_abv_percent = 0.5, quantile_type = 7L,
            analysis_version = CONALCO_DAILY_VERSION, .before = 1)
        }))
      }))
    }))
  }))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) IMMUTABLE RUN SHARDS AND RECOVERABLE PANEL VIEWS
# <-----------------------------------------------------------------------------

daily_replace = \(path, writer) {
  ensure_dir(dirname(path))
  pending = tempfile('.daily_', tmpdir = dirname(path))
  on.exit(unlink(pending), add = TRUE)
  writer(pending)
  if (!file.rename(pending, path)) stop('Cannot replace daily view: ', path)
  invisible(path)
}

daily_lock = \(path) {
  ensure_dir(dirname(path))
  if (!dir.create(path, showWarnings = FALSE))
    stop('Daily lock exists; inspect owner.json before manual recovery: ', path)
  write_json_once(list(pid = Sys.getpid(), acquired_at_utc = utc_now()),
    file.path(path, 'owner.json'))
  invisible(path)
}

daily_read_csv = \(path) read.csv(path, colClasses = 'character',
  na.strings = '', check.names = FALSE)

daily_write_gzip = \(x, path) {
  connection = gzfile(path, 'wt', compression = 9)
  on.exit(close(connection))
  write.csv(x, connection, row.names = FALSE, na = '')
}

daily_run_records = \(root) {
  paths = sort(list.files(file.path(root, 'runs'), '^manifest[.]json$',
    recursive = TRUE, full.names = TRUE))
  records = lapply(paths, read_json)
  for (record in records) {
    entry = record$observations
    if (!file.exists(entry$path) || !identical(
        digest(file = entry$path, algo = 'sha256'), entry$sha256))
      stop('Daily shard checksum mismatch: ', entry$path)
    if (!identical(record$analysis_version, CONALCO_DAILY_VERSION))
      stop('Daily analysis version changed; explicit migration required')
  }
  records
}

daily_rebuild_views = \(root, records = daily_run_records(root)) {
  if (!length(records)) stop('No committed daily runs')
  dates = vapply(records, `[[`, '', 'date')
  if (anyDuplicated(dates)) stop('Duplicate committed Berlin dates')
  records = records[order(dates)]
  columns = unique(unlist(lapply(records, \(record)
    names(read.csv(record$observations$path, nrows = 0, check.names = FALSE)))))
  state = new.env()
  state$n = 0L
  state$statistics = list()
  # Only one day's product descriptions need to fit into memory.
  daily_replace(file.path(root, 'panel.csv.gz'), \(path) {
    connection = gzfile(path, 'wt', compression = 6)
    on.exit(close(connection))
    for (record in records) {
      day = daily_read_csv(record$observations$path)
      keys = paste(day$retailer, day$retailer_product_id, day$daily_date)
      if (anyNA(day[c('retailer', 'retailer_product_id', 'run_id',
            'daily_date')]) || anyDuplicated(keys) ||
          !all(day$retailer == 'conalco') ||
          !all(day$daily_date == record$date) ||
          !all(day$run_id == record$run_id))
        stop('Duplicate, missing or inconsistent daily identities')
      day = day[order(day$retailer, day$retailer_product_id), ]
      state$statistics[[record$run_id]] = daily_statistics(day)
      for (column in setdiff(columns, names(day))) day[[column]] = NA_character_
      write.table(day[columns], connection, sep = ',', row.names = FALSE,
        col.names = state$n == 0L, na = '', qmethod = 'double')
      state$n = state$n + nrow(day)
    }
  })
  statistics = bind_rows(state$statistics)
  daily_replace(file.path(root, 'statistics.csv'), \(path)
    write.csv(statistics, path, row.names = FALSE, na = ''))
  receipt = list(analysis_version = CONALCO_DAILY_VERSION,
    updated_at_utc = utc_now(), n_observations = state$n,
    n_runs = length(records), run_ids = vapply(records, `[[`, '', 'run_id'),
    panel = refresh_file_record(file.path(root, 'panel.csv.gz')),
    statistics = refresh_file_record(file.path(root, 'statistics.csv')),
    population = 'Exact ABV >=0.5 percent; included beverage scope',
    price_basis = 'Observed sale price excluding separately displayed deposit',
    ethanol_density_g_ml = ETHANOL_DENSITY_G_ML, quantile_type = 7L,
    availability_policy = 'include_unknown', weighted = FALSE)
  daily_replace(file.path(root, 'manifest.json'), \(path)
    write_json(receipt, path, auto_unbox = TRUE, pretty = TRUE, na = 'null'))
  invisible(receipt)
}

read_conalco_daily_panel = \(root = CONALCO_DAILY_ROOT) {
  versioned = file.exists(file.path(root, 'schema.json'))
  if (missing(root) && exists('conalco_storage_mode', mode = 'function') &&
      conalco_storage_mode() == 'versioned') {
    root = conalco_versioned_root()
    versioned = TRUE
  }
  if (versioned) {
    dates = names(dedup_history(root))
    return(dedup_panel_values(dedup_restore_rows(root, dates[1],
      tail(dates, 1L))))
  }
  receipt = read_json(file.path(root, 'manifest.json'))
  records = daily_run_records(root)
  if (!setequal(unlist(receipt$run_ids),
      vapply(records, `[[`, '', 'run_id')))
    stop('Daily views need rebuilding from committed runs')
  for (entry in list(receipt$panel, receipt$statistics)) {
    if (!file.exists(entry$path) || !identical(
        digest(file = entry$path, algo = 'sha256'), entry$sha256))
      stop('Daily view checksum mismatch; rebuild views')
  }
  daily_read_csv(receipt$panel$path)
}

append_conalco_daily = \(manifest_path, root = CONALCO_DAILY_ROOT,
  conflict_review = NULL) {
  manifest = require_complete_catalog(manifest_path)
  raw = read_observations(manifest$proof$products$path)
  require_complete_catalog(manifest_path, raw)
  dates = unique(berlin_date(raw$observed_at))
  if (length(dates) != 1L || anyNA(dates))
    stop('Run spans multiple Berlin dates; daily publication requires review')
  if (exists('conalco_storage_mode', mode = 'function') &&
      conalco_storage_mode() == 'versioned') {
    return(conalco_append_versioned(manifest_path, raw, conflict_review))
  }
  lock = file.path(root, '.write_lock')
  daily_lock(lock)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  records = daily_run_records(root)
  review = if (is.null(conflict_review)) NULL else
    refresh_file_record(conflict_review)
  # An existing unmanaged panel must never be replaced by a partial history.
  if (!length(records) && file.exists(file.path(root, 'panel.csv.gz')))
    stop('Existing panel without run shards; explicit import required')
  for (record in records) {
    if (identical(record$run_id, manifest$run_id)) {
      if (!identical(record$source_manifest$sha256,
          digest(file = manifest_path, algo = 'sha256')) ||
          !identical(record$conflict_review, review))
        stop('Changed source for an already committed daily run')
      return(daily_rebuild_views(root, records))
    }
    if (identical(record$date, dates[1]))
      stop('Berlin date already committed; no second daily observation')
  }
  analysed = daily_analyse_products(raw, conflict_review)
  # Store all products, including excluded and missing ABV observations.
  directory = ensure_dir(file.path(root, 'runs', manifest$run_id))
  path = file.path(directory, 'observations.csv.gz')
  if (!file.exists(path)) write_once(path, \(p)
    daily_write_gzip(analysed, p))
  else if (!isTRUE(all.equal(daily_read_csv(path),
      as.data.frame(lapply(analysed, \(v) {
        text = as.character(v)
        text[!is.na(text) & !nzchar(text)] = NA_character_
        text
      })), check.attributes = FALSE)))
    stop('Interrupted shard differs; preserve it for review')
  code = c('R/classification.R', 'R/quality.R', 'R/price_analysis.R',
    'R/snapshot_analysis.R', 'R/conalco_daily.R')
  write_json_once(list(run_id = manifest$run_id, date = dates[1],
      created_at_utc = utc_now(), analysis_version = CONALCO_DAILY_VERSION,
      source_manifest = refresh_file_record(manifest_path),
      conflict_review = review,
      observations = refresh_file_record(path), n_products = nrow(analysed),
      code = lapply(code, refresh_file_record)),
    file.path(directory, 'manifest.json'))
  daily_rebuild_views(root)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) BOUNDED COLLECTION USING THE EXISTING REVIEW AND VALIDATION
# <-----------------------------------------------------------------------------

daily_terms_hash = \(metadata_path) {
  doc = read_html(read_archive(metadata_path)$body, encoding = 'UTF-8')
  text = node_text(doc, '//main')
  if (is.na(text)) text = node_text(doc, '//body')
  if (is.na(text) || !nzchar(text)) stop('Empty terms text')
  digest(clean_text(text), algo = 'sha256', serialize = FALSE)
}

daily_terms_document = \(metadata_path) {
  archived = read_archive(metadata_path)
  if (!identical(archived$meta$response_status, 200L) ||
      !identical(archived$meta$request_type, 'terms') ||
      !identical(archived$meta$request_url, 'https://www.conalco.de/agb'))
    stop('Missing or unexpected terms response')
  doc = read_html(archived$body, encoding = 'UTF-8')
  main = xml_find_all(doc, '//main')
  if (length(main) != 1L) stop('Ambiguous terms document structure')
  text = clean_text(xml_text(main[[1]]))
  nodes = xml_find_all(main[[1]], './/text()[contains(.,"Stand:")]')
  if (length(nodes) != 1L) stop('Missing or ambiguous terms Stand value')
  stamp = xml_parent(nodes[[1]])
  value = clean_text(xml_text(stamp))
  pattern = '^Stand: [0-9]{2}[.][0-9]{2}[.][0-9]{4}, [0-9:]{8}$'
  if (xml_name(stamp) != 'div' || length(xml_children(stamp)) ||
      !grepl(pattern, value) || !endsWith(text, value))
    stop('Unexpected final terms Stand structure')
  date = sub('^Stand: ', '', value)
  parsed = strptime(date, '%d.%m.%Y, %H:%M:%S', tz = 'UTC')
  if (is.na(parsed) || !identical(format(parsed,
        '%d.%m.%Y, %H:%M:%S', tz = 'UTC'), date))
    stop('Invalid terms Stand date')
  # Retain the element and all attributes for the strict structure comparison.
  xml_set_text(stamp, 'APM_TERMS_STAND')
  structure = digest(as.character(main[[1]]), algo = 'sha256',
    serialize = FALSE)
  xml_remove(stamp)
  content = clean_text(xml_text(main[[1]]))
  if (!nzchar(content)) stop('Empty terms contract content')
  list(stand = value, full_text_sha256 = digest(text, algo = 'sha256',
      serialize = FALSE), content_sha256 = digest(content, algo = 'sha256',
      serialize = FALSE), structure_sha256 = structure,
    raw_sha256 = archived$meta$raw_sha256)
}

daily_check_terms = \(config, metadata_path, directory, events) {
  approved = config$reviewed_terms_text_sha256
  reference = config$reviewed_terms_reference
  if (is.null(reference)) {
    # Existing exact-hash approvals receive no automatic metadata exception.
    if (!identical(daily_terms_hash(metadata_path), approved))
      stop('Terms changed; new review required before product requests')
    return(invisible(NULL))
  }
  if (nchar(scalar(config$terms_stand_change_authorization, '')) < 10L)
    stop('Missing explicit authorization for terms Stand-only changes')
  if (!file.exists(reference$path) || !identical(reference$sha256,
      digest(file = reference$path, algo = 'sha256')))
    stop('Approved terms reference is missing or changed')
  before = daily_terms_document(reference$path)
  if (!identical(before$full_text_sha256, approved))
    stop('Terms reference does not match the approved full text hash')
  current = daily_terms_document(metadata_path)
  previous = before$stand
  if (file.exists(events)) {
    history = lapply(readLines(events, warn = FALSE), fromJSON)
    history = Filter(\(entry) identical(entry$event, 'terms_verified') &&
        identical(entry$reference_metadata_sha256, reference$sha256), history)
    if (length(history)) previous = tail(history, 1L)[[1]]$current_stand
  }
  reason = if (!identical(current$content_sha256, before$content_sha256))
    'Terms clauses changed' else if (!identical(current$structure_sha256,
      before$structure_sha256)) 'Terms structure changed' else NULL
  if (!is.null(reason)) {
    log_event(events, 'terms_comparison_rejected', reason = reason,
      previous_stand = previous, new_stand = current$stand,
      stand_changed = !identical(previous, current$stand),
      reference_metadata_sha256 = reference$sha256,
      current_metadata = refresh_file_record(metadata_path),
      full_text_sha256 = current$full_text_sha256,
      content_sha256 = current$content_sha256,
      structure_sha256 = current$structure_sha256)
    stop(reason, '; new review required before product requests')
  }
  receipt = list(policy = 'conalco-terms-stand-only-0.1.0',
    reference = reference, reference_stand = before$stand,
    approved_full_text_sha256 = approved,
    current_metadata = refresh_file_record(metadata_path),
    current_stand = current$stand, full_text_sha256 = current$full_text_sha256,
    raw_sha256 = current$raw_sha256, content_sha256 = current$content_sha256,
    structure_sha256 = current$structure_sha256)
  path = file.path(directory, 'terms_comparison.json')
  if (file.exists(path)) {
    saved = read_json(path)
    if (!all(vapply(names(receipt), \(name)
          identical(saved[[name]], receipt[[name]]), logical(1))))
      stop('Existing terms comparison differs; preserve it for review')
    return(invisible(saved))
  }
  receipt$previous_stand = previous
  receipt$stand_changed = !identical(previous, current$stand)
  receipt$checked_at_utc = utc_now()
  write_json_once(receipt, path)
  if (receipt$stand_changed) log_event(events, 'terms_stand_changed',
    previous_stand = previous, new_stand = current$stand,
    reference_metadata_sha256 = reference$sha256,
    comparison = refresh_file_record(path))
  log_event(events, 'terms_verified', current_stand = current$stand,
    reference_metadata_sha256 = reference$sha256,
    content_sha256 = current$content_sha256,
    full_text_sha256 = current$full_text_sha256,
    comparison = refresh_file_record(path))
  invisible(receipt)
}

daily_archive_run = \(manifest_path, backup_directory,
  root = CONALCO_DAILY_ROOT, paths = NULL) {
  manifest = require_complete_catalog(manifest_path)
  id = manifest$run_id
  roots = c(run_dir(id), dirname(manifest$proof$products$path),
    file.path(root, 'runs', id))
  if (is.null(paths)) paths = unlist(lapply(roots, \(directory)
    list.files(directory, recursive = TRUE, full.names = TRUE,
      all.files = TRUE)))
  paths = sort(unique(paths))
  paths = paths[!dir.exists(paths)]
  if (any(grepl('^/|(^|/)[.][.](/|$)', paths)) ||
      any(nzchar(Sys.readlink(paths)))) stop('Unsafe backup source path')
  inventory = lapply(paths, refresh_file_record)
  archive = file.path(ensure_dir(backup_directory), paste0(id, '.tar.gz'))
  receipt_path = paste0(archive, '.json')
  tar_binary = Sys.which('tar')
  if (!nzchar(tar_binary)) stop('A system tar is required for long paths')
  if (!file.exists(archive)) write_once(archive, \(p)
    tar(p, files = paths, compression = 'gzip', tar = tar_binary,
      extra_flags = '--format=ustar'))
  if (file.exists(receipt_path)) {
    receipt = read_json(receipt_path)
    if (!identical(receipt$archive$sha256,
        digest(file = archive, algo = 'sha256')) ||
        !identical(receipt$files, inventory))
      stop('Backup or source changed; preserve for review')
    return(invisible(receipt))
  }
  scratch = tempfile('backup_verify_')
  ensure_dir(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  members = untar(archive, list = TRUE, tar = 'internal')
  if (!setequal(members, paths) || anyDuplicated(members))
    stop('Backup member inventory differs')
  untar(archive, exdir = scratch, tar = 'internal')
  for (entry in inventory) {
    if (!identical(digest(file = file.path(scratch, entry$path),
        algo = 'sha256'), entry$sha256)) stop('Backup restore hash differs')
  }
  receipt = list(run_id = id, verified_at_utc = utc_now(),
    archive = refresh_file_record(archive), files = inventory,
    restore_byte_identical = TRUE, originals_removed = FALSE)
  if (root != CONALCO_DAILY_ROOT) receipt$daily_root = root
  write_json_once(receipt, receipt_path)
  invisible(receipt)
}

daily_prepare_listing_routes = \(config, directory) {
  approval = config$approved_listing_routes
  if (is.null(approval)) return(invisible(NULL))
  for (entry in approval) {
    if (!file.exists(entry$path) || !identical(
        digest(file = entry$path, algo = 'sha256'), entry$sha256))
      stop('Approved listing-route evidence changed or is missing')
  }
  manifest = require_complete_catalog(approval$source_manifest$path)
  pages = read.csv(manifest$proof$page_completeness$path,
    colClasses = 'character')
  routes = read_json(approval$routes$path)$routes
  verified = list()
  for (route in routes) {
    category = route$category_url
    first = pages[pages$category_url == category & pages$page == '1', ]
    if (nrow(first) != 1L) next
    actual = refresh_request_url(category, list(route))
    metadata = read_json(first$metadata_file)
    # Only routes freshly successful in the approved complete source run.
    if (!identical(metadata$request_url, actual)) next
    refresh_verify_route(metadata, category, actual)
    route$fresh_success_metadata = NULL
    route$historical_success_metadata = NULL
    route$source_use = paste('Technical identity/route only;',
      'no historical product or price observations reused')
    verified[[length(verified) + 1L]] = route
  }
  path = file.path(directory, 'category_endpoint_routes_daily_approved.json')
  if (!file.exists(path)) write_json_once(list(routes = verified,
      approval = approval, created_at_utc = utc_now()), path)
  invisible(path)
}

run_conalco_daily = \(manual = FALSE,
  config_file = 'config/daily_collection.json',
  client = new_http_client(pause_seconds = 3, max_tries = 1,
    request_scope = 'authorized daily Conalco catalogue',
    follow_redirects = FALSE), now = Sys.time(), finalize = NULL) {
  config = read_json(config_file)
  today = format(now, '%Y-%m-%d', tz = 'Europe/Berlin')
  logs = ensure_dir('logs/conalco_daily')
  events = file.path(logs, 'events.jsonl')
  if (!isTRUE(config$enabled)) {
    log_event(events, 'blocked', reason = 'daily_collection_disabled')
    stop('Daily collection disabled; current access release is required')
  }
  if (!identical(config$timezone, 'Europe/Berlin'))
    stop('Daily timezone must be Europe/Berlin')
  if ((!manual && today < config$start_date) ||
      (!manual && format(now, '%H:%M', tz = 'Europe/Berlin') <
        config$scheduled_local_time)) return(invisible('before_schedule'))
  authorization = scalar(config$collection_authorization, '')
  approved_terms = scalar(config$reviewed_terms_text_sha256, '')
  if (nchar(authorization) < 10 || !grepl('^[0-9a-f]{64}$', approved_terms))
    stop('Missing current collection authorization or reviewed terms hash')
  if (!isTRUE(config$backup_restore_verified) ||
      !dir.exists(scalar(config$backup_directory, '')))
    stop('Verified permanent backup destination required')
  lock = file.path(logs, '.collector_lock')
  daily_lock(lock)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  record_path = file.path(logs, paste0(today, '.json'))
  record = if (file.exists(record_path)) read_json(record_path) else
    list(date = today, run_id = conalco_new_refresh_run(), status = 'prepared')
  # A failed review has no reusable set of three pages. Preserve that run,
  # then start another bounded review attempt; collected products are resumed.
  old_directory = run_dir(record$run_id)
  if (!file.exists(file.path(old_directory, 'access_review_pages.json')) &&
      length(list.files(old_directory, '^[0-9]+_[a-z]+[.]json$'))) {
    log_event(events, 'review_attempt_retained', run_id = record$run_id)
    record$run_id = conalco_new_refresh_run()
  }
  directory = run_dir(record$run_id)
  save_record = \(status, error = NULL) {
    record$status = status
    record$error = error
    if (identical(status, 'complete')) record$partial_manifest = NULL
    record$updated_at_utc = utc_now()
    daily_replace(record_path, \(path)
      write_json(record, path, auto_unbox = TRUE, pretty = TRUE, null = 'null'))
    log_event(events, status, run_id = record$run_id, reason = error)
  }
  save_record('started')
  tryCatch({
    manifest_path = file.path(directory, 'manifest.json')
    if (!file.exists(manifest_path)) {
      # Never resume yesterday's pages as today's measurements.
      if (berlin_date(read_json(file.path(directory,
          'started.json'))$started_at_utc) != today)
        stop('Attempt belongs to another date; preserve and review it')
      total_bytes = sum(file.info(list.files(c('data/raw/conalco',
            config$backup_directory),
          recursive = TRUE, full.names = TRUE))$size, na.rm = TRUE)
      if (!is.finite(config$max_raw_bytes) ||
          total_bytes >= config$max_raw_bytes)
        stop('Local raw archive quota reached; archive review required')
      if (!file.exists(file.path(directory, 'access_review_pages.json'))) {
        path = file.path(directory, 'access_review_draft.json')
        draft = read_json(path)
        draft$review_pages_approved_by_user = TRUE
        draft$approval_reference = authorization
        daily_replace(path, \(p)
          write_json(draft, p, auto_unbox = TRUE, pretty = TRUE))
        conalco_fetch_review_pages(record$run_id, client, dynamic = TRUE)
      }
      evidence = read_json(file.path(directory, 'access_review_pages.json'))
      daily_check_terms(config, evidence$terms_metadata, directory, events)
      if (!file.exists(file.path(directory, 'access_review.json')))
        conalco_approve_refresh(record$run_id,
          'Daily terms comparison passed against the approved reference',
          authorization)
      daily_prepare_listing_routes(config, directory)
      robots = if (evidence$robots_status == 200L) rawToChar(
        read_archive(evidence$robots_metadata)$body) else ''
      guarded_client = \(url) {
        if (!robots_allowed(robots, url))
          refresh_global_stop(paste('robots.txt disallows a discovered URL:',
              url))
        if (format(Sys.time(), '%Y-%m-%d', tz = 'Europe/Berlin') != today)
          refresh_global_stop('Berlin date changed; incomplete run retained')
        client(url)
      }
      if (!file.exists(file.path(directory, 'collection_finished.json')))
        run_conalco_refresh(record$run_id, guarded_client, dynamic = TRUE,
          max_requests = if (is.null(config$max_archived_requests)) 1500L else
            config$max_archived_requests,
          max_hours = if (is.null(config$max_hours_per_invocation)) 2 else
            config$max_hours_per_invocation)
      if (!file.exists(file.path(directory, 'collection_finished.json'))) {
        record$partial_manifest = refresh_file_record(
          refresh_store_partial(record$run_id, 'Local category/product gaps'))
        save_record('partial')
        return(invisible(record$run_id))
      }
      scope_path = file.path(directory, 'scope_review.csv')
      if (!file.exists(scope_path)) write_csv_once(data.frame(
          category_url = character(), decision = character(),
          reason = character(), metadata_sha256 = character()), scope_path)
      manifest_path = validate_conalco_refresh(record$run_id, scope_path)
    }
    append_conalco_daily(manifest_path)
    if (is.null(finalize))
      daily_archive_run(manifest_path, config$backup_directory)
    else finalize(manifest_path, config$backup_directory)
    save_record('complete')
    invisible(record$run_id)
  }, error = \(e) {
    if (file.exists(file.path(directory, 'checkpoint.rds'))) {
      partial = tryCatch(refresh_store_partial(record$run_id,
          conditionMessage(e)), error = \(export_error) {
          log_event(events, 'partial_export_failed', run_id = record$run_id,
            reason = conditionMessage(export_error))
          NULL
        })
      if (!is.null(partial))
        record$partial_manifest = refresh_file_record(partial)
    }
    save_record('failed_or_review_required', conditionMessage(e))
    stop(e)
  })
}


# Origin: conalco_daily.R, 261029; consolidated 261030.
# 4) THREE INDEPENDENT OPT-IN ROUTES; LEGACY IS THE DEFAULT
# <-----------------------------------------------------------------------------

conalco_storage_config = \() {
  defaults = list(version = 1L, local_storage = 'legacy',
    github_publication = 'legacy', ukebox_results = 'legacy',
    versioned_root = 'data/conalco/versioned',
    ukebox_base = 'APM/conalco/versioned/v1',
    schema = 'config/conalco_versioned_schema.json')
  override = getOption('apm.conalco.storage')
  path = 'config/conalco_storage.json'
  if (is.null(override) && file.exists(path)) {
    override = jsonlite::read_json(path, simplifyVector = TRUE)
  }
  if (!is.null(override)) {
    stopifnot(all(names(override) %in% names(defaults)))
    defaults[names(override)] = override
  }
  stopifnot(defaults$version == 1L,
    defaults$local_storage %in% c('legacy', 'versioned'),
    defaults$github_publication %in% c('legacy', 'versioned'),
    defaults$ukebox_results %in% c('legacy', 'incremental'))
  for (value in defaults[c('versioned_root', 'ukebox_base', 'schema')]) {
    stopifnot(is.character(value), length(value) == 1L,
      !grepl('^/|(^|/)[.][.](/|$)', value),
      !grepl('\\', value, fixed = TRUE))
  }
  defaults
}

conalco_storage_mode = \(kind = 'local_storage') {
  conalco_storage_config()[[kind]]
}

conalco_versioned_root = \(project = getwd()) {
  file.path(project, conalco_storage_config()$versioned_root)
}

conalco_init_versioned = \(root, schema =
    conalco_storage_config()$schema, initial_empty = FALSE) {
  s = jsonlite::read_json(schema, simplifyVector = TRUE)
  dedup_init(root, s$columns, s$tracked, s$core)
  if (initial_empty) {
    dedup_json(list(kind = 'explicit_empty_initialization', dates = character(),
      observations = 0L, initial_commit_sha256 = 'GENESIS'),
      file.path(root, 'bootstrap.json'))
  }
}

conalco_versioned_records = \(root = conalco_versioned_root()) {
  history = dedup_history(root)
  lapply(names(history), \(date) {
    entry = jsonlite::read_json(file.path(root, 'commits', date,
      'manifest.json'))$source_receipt
    entry$date = date
    durable = file.path(root, 'raw_receipts', paste0(date, '.json'))
    if (file.exists(durable)) entry$raw_archive = jsonlite::read_json(durable)
    entry
  })
}

conalco_record_raw_receipt = \(root, date, receipt) {
  stopifnot(date %in% names(dedup_history(root)))
  directory = file.path(root, 'raw_receipts')
  dir.create(directory, showWarnings = FALSE)
  path = file.path(directory, paste0(date, '.json'))
  if (file.exists(path)) {
    old = jsonlite::read_json(path)
    # Verification time may differ on a retry; the durable object must not.
    for (field in c('path', 'sha256', 'manifest_sha256')) {
      stopifnot(identical(old[[field]], receipt[[field]]))
    }
    return(invisible(path))
  }
  dedup_json(receipt, path)
  invisible(path)
}

conalco_migrate_legacy = \(source, target, initialize = TRUE) {
  source = normalizePath(source, mustWork = TRUE)
  target = file.path(normalizePath(dirname(target), mustWork = TRUE),
    basename(target))
  if (target == source || startsWith(target, paste0(source, '/')))
    stop('Migration must not write into its source')
  if (!dir.exists(target)) {
    if (!initialize) stop('Explicit initial migration is required')
    conalco_init_versioned(target)
  }
  paths = list.files(file.path(source, 'runs'), '^manifest[.]json$',
    recursive = TRUE, full.names = TRUE)
  receipts = lapply(paths, \(p) jsonlite::read_json(p, simplifyVector = TRUE))
  order = order(vapply(receipts, \(r) r$date, ''))
  paths = paths[order]
  receipts = receipts[order]
  statistics_file = file.path(source, 'statistics.csv')
  if (!file.exists(statistics_file))
    stop('Original validated statistics.csv is required for migration')
  original_statistics = dedup_read(statistics_file)
  stopifnot(length(paths) > 0L,
    !anyDuplicated(vapply(receipts, \(r) r$date, '')))
  manifest_path = file.path(source, 'manifest.json')
  if (!file.exists(manifest_path)) stop('Complete legacy manifest is required')
  manifest = jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  ids = vapply(receipts, \(r) r$run_id, '')
  if (is.null(manifest$n_runs) || manifest$n_runs != length(receipts) ||
      manifest$n_observations != sum(vapply(receipts,
        \(r) r$n_products, numeric(1))) ||
      !setequal(ids, unlist(manifest$run_ids, use.names = FALSE)) ||
      !identical(ids, basename(dirname(paths))) ||
      manifest$statistics$sha256 != dedup_hash(statistics_file))
    stop('Legacy manifest, statistics and complete run inventory differ')
  for (i in seq_along(paths)) {
    r = receipts[[i]]
    shard = file.path(dirname(paths[i]), 'observations.csv.gz')
    stopifnot(dedup_hash(shard) == r$observations$sha256)
    history = dedup_history(target)
    if (r$date %in% names(history)) {
      stored = history[[r$date]]
      stopifnot(stored$run_id == r$run_id,
        stored$source_manifest_sha256 == r$source_manifest$sha256)
      if (stored$input_sha256 != r$observations$sha256) {
        x = dedup_read(shard)[dedup_schema(target)$columns]
        x = x[order(x$retailer, x$retailer_product_id), ]
        rownames(x) = NULL
        stopifnot(identical(x, dedup_restore_rows(target, r$date, r$date)))
      }
    } else dedup_ingest(target, shard, paths[i], statistics =
      original_statistics[original_statistics$date == r$date, ],
      statistics_origin = list(kind = 'validated_legacy_statistics',
        source_sha256 = dedup_hash(statistics_file)))
    if (!is.null(r$raw_archive)) {
      conalco_record_raw_receipt(target, r$date,
        jsonlite::read_json(paths[i])$raw_archive)
    }
  }
  conalco_capture_statistics(target, statistics_file)
  conalco_mark_bootstrap(target)
  invisible(dedup_history(target))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) POST-VALIDATION APPEND; NO HISTORICAL RECLASSIFICATION
# <-----------------------------------------------------------------------------

conalco_append_versioned = \(manifest_path, raw, conflict_review = NULL) {
  root = conalco_versioned_root()
  if (!dir.exists(root)) stop('Run an explicit versioned migration first')
  conalco_check_bootstrap(root)
  manifest = require_complete_catalog(manifest_path, raw)
  date = unique(berlin_date(raw$observed_at))
  stopifnot(length(date) == 1L, !is.na(date))
  history = dedup_history(root)
  if (date %in% names(history)) {
    prior = history[[date]]
    stopifnot(prior$run_id == manifest$run_id,
      prior$source_manifest_sha256 == dedup_hash(manifest_path))
    previous = prior$source_receipt$conflict_review
    if (!length(previous)) previous = NULL
    current = if (is.null(conflict_review)) NULL else
      refresh_file_record(conflict_review)
    if (is.null(previous) || is.null(current)) {
      stopifnot(is.null(previous), is.null(current))
    } else stopifnot(identical(previous$path, current$path),
      identical(previous$sha256, current$sha256))
    return(invisible(prior))
  }
  analysed = daily_analyse_products(raw, conflict_review)
  pending = tempfile('incoming_', tmpdir = file.path(root, 'transactions'))
  dir.create(pending, recursive = TRUE)
  on.exit(unlink(pending, recursive = TRUE), add = TRUE)
  shard = file.path(pending, 'observations.csv.gz')
  daily_write_gzip(analysed, shard)
  receipt = list(run_id = manifest$run_id, date = date,
    analysis_version = CONALCO_DAILY_VERSION,
    source_manifest = refresh_file_record(manifest_path),
    conflict_review = if (is.null(conflict_review)) NULL else
      refresh_file_record(conflict_review),
    observations = list(path = 'observations.csv.gz',
      sha256 = dedup_hash(shard)), n_products = nrow(analysed),
    code = lapply(c('R/classification.R', 'R/quality.R',
      'R/price_analysis.R', 'R/snapshot_analysis.R', 'R/conalco_daily.R'),
      refresh_file_record))
  path = file.path(pending, 'manifest.json')
  dedup_json(receipt, path)
  stats = daily_statistics(dedup_panel_values(dedup_read(shard)))
  dedup_ingest(root, shard, path, statistics = stats,
    statistics_origin = list(kind = 'captured_at_collection',
      r_version = R.version.string, platform = R.version$platform,
      analysis_version = CONALCO_DAILY_VERSION))
  invisible(dedup_history(root)[[date]])
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) READ-ONLY LEGACY BRIDGES FOR STAGGERED APPROVAL AND ROLLBACK
# <-----------------------------------------------------------------------------

conalco_legacy_view = \(root, output, dates = names(dedup_history(root)),
    panel = TRUE, daily_root = 'data/conalco/daily') {
  conalco_safe_paths(daily_root)
  if (dir.exists(output)) stop('Legacy view needs a new output directory')
  dir.create(output, recursive = TRUE)
  target = file.path(output, daily_root)
  dir.create(file.path(target, 'runs'), recursive = TRUE)
  records = conalco_versioned_records(root)
  for (r in records) {
    if (!r$date %in% dates) next
    directory = file.path(target, 'runs', r$run_id)
    dir.create(directory)
    path = file.path(directory, 'observations.csv.gz')
    x = dedup_restore_rows(root, r$date, r$date)
    dedup_write(x, path, level = 9L)
    r$observations = list(path = file.path(daily_root, 'runs', r$run_id,
      'observations.csv.gz'), sha256 = dedup_hash(path))
    dedup_json(r, file.path(directory, 'manifest.json'))
  }
  if (panel) {
    records = lapply(list.files(file.path(target, 'runs'),
      '^manifest[.]json$', recursive = TRUE, full.names = TRUE), read_json)
    for (i in seq_along(records)) {
      records[[i]]$observations$path = file.path(output,
        records[[i]]$observations$path)
    }
    daily_rebuild_views(target, records)
    write.csv(conalco_versioned_statistics(root, dates),
      file.path(target, 'statistics.csv'), row.names = FALSE, na = '')
    # Freeze portable paths after the builder has validated all actual files.
    manifest = read_json(file.path(target, 'manifest.json'))
    manifest$panel$path = file.path(daily_root, 'panel.csv.gz')
    manifest$statistics$path = file.path(daily_root, 'statistics.csv')
    manifest$statistics$sha256 = dedup_hash(file.path(target, 'statistics.csv'))
    write_json(manifest, file.path(target, 'manifest.json'),
      auto_unbox = TRUE, pretty = TRUE, na = 'null')
  }
  invisible(target)
}

conalco_versioned_validate = \(root) {
  history = dedup_history(root)
  if (!length(history)) stop('Empty versioned store')
  x = dedup_restore_rows(root, names(history)[1], tail(names(history), 1L))
  stopifnot(nrow(x) == sum(vapply(history, \(r) r$n_products, integer(1))))
  conalco_versioned_statistics(root, names(history))
  if (file.exists(file.path(root, 'bootstrap.json')))
    conalco_check_bootstrap(root)
  invisible(list(dates = names(history), observations = nrow(x),
    products = length(unique(x$product_key))))
}

conalco_sync_legacy = \(legacy = CONALCO_DAILY_ROOT, initialize = FALSE) {
  target = conalco_versioned_root()
  if (!dir.exists(dirname(target)))
    dir.create(dirname(target), recursive = TRUE)
  if (!initialize) conalco_check_bootstrap(target)
  conalco_migrate_legacy(legacy, target, initialize = initialize)
  target
}

conalco_export_versioned_period = \(from, to, output, root) {
  output = file.path(normalizePath(dirname(output), mustWork = TRUE),
    basename(output))
  root = normalizePath(root, mustWork = TRUE)
  if (file.exists(output) || startsWith(output, paste0(root, '/')))
    stop('Choose a new export directory outside the store')
  x = dedup_panel_values(dedup_restore_rows(root, from, to))
  pending = tempfile('period_', tmpdir = dirname(output))
  dir.create(pending)
  on.exit(unlink(pending, recursive = TRUE), add = TRUE)
  panel = file.path(pending, 'panel.csv.gz')
  dedup_write(x, panel)
  statistics = file.path(pending, 'statistics.csv')
  write.csv(conalco_versioned_statistics(root, unique(x$daily_date)),
    statistics, row.names = FALSE, na = '')
  receipt = list(analysis_version = CONALCO_DAILY_VERSION,
    n_observations = nrow(x), n_runs = length(unique(x$daily_date)),
    run_ids = unique(x$run_id),
    panel = list(path = file.path(output, 'panel.csv.gz'),
      sha256 = dedup_hash(panel)),
    statistics = list(path = file.path(output, 'statistics.csv'),
      sha256 = dedup_hash(statistics)),
    export_period = list(from = from, to = to,
      observed_dates = unique(x$daily_date)),
    source_commits = vapply(unique(x$daily_date), \(date) {
      dedup_hash(file.path(root, 'commits', date, 'manifest.json'))
    }, ''))
  dedup_json(receipt, file.path(pending, 'manifest.json'))
  if (file.exists(output) || !file.rename(pending, output))
    stop('Cannot activate period export')
  invisible(receipt)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) IMMUTABLE STATISTICS; NEVER RECOMPUTE HISTORICAL PRICE SUMMARIES
# <-----------------------------------------------------------------------------

conalco_capture_statistics = \(root, source) {
  values = dedup_read(source)
  history = dedup_history(root)
  directory = file.path(root, 'statistics_snapshots')
  dir.create(directory, showWarnings = FALSE)
  for (date in names(history)) {
    rows = values[values$date == date, ]
    rownames(rows) = NULL
    stopifnot(nrow(rows) > 0L, all(rows$run_id == history[[date]]$run_id))
    committed = file.path(root, 'commits', date, 'statistics.csv')
    if (file.exists(committed)) {
      stopifnot(identical(rows, dedup_read(committed)))
      next
    }
    target = file.path(directory, date)
    if (dir.exists(target)) {
      stopifnot(identical(rows, conalco_versioned_statistics(root, date)))
      next
    }
    pending = tempfile('statistics_', tmpdir = file.path(root, 'transactions'))
    dir.create(pending)
    data = file.path(pending, 'data.csv')
    write.csv(rows, data, row.names = FALSE, na = '')
    dedup_json(list(date = date, source_name = basename(source),
      source_sha256 = dedup_hash(source), data_sha256 = dedup_hash(data),
      rows = nrow(rows)), file.path(pending, 'manifest.json'))
    if (!file.rename(pending, target)) stop('Cannot freeze original statistics')
  }
  invisible(directory)
}

conalco_versioned_statistics = \(root, dates) {
  history = dedup_history(root)
  stopifnot(all(dates %in% names(history)))
  tables = lapply(dates, \(date) {
    path = file.path(root, 'commits', date, 'statistics.csv')
    if (!file.exists(path)) {
      directory = file.path(root, 'statistics_snapshots', date)
      if (!dir.exists(directory))
        stop('Original price statistics missing for ', date)
      meta = jsonlite::read_json(file.path(directory, 'manifest.json'))
      path = file.path(directory, 'data.csv')
      stopifnot(meta$date == date, dedup_hash(path) == meta$data_sha256)
    }
    table = dedup_read(path)
    stopifnot(nrow(table) > 0L, all(table$date == date),
      all(table$run_id == history[[date]]$run_id))
    table
  })
  values = do.call(rbind, tables)
  rownames(values) = NULL
  values
}

conalco_check_bootstrap = \(root) {
  path = file.path(root, 'bootstrap.json')
  if (!file.exists(path)) stop('Explicit initial migration is not complete')
  bootstrap = jsonlite::read_json(path, simplifyVector = TRUE)
  dates = as.character(unlist(bootstrap$dates, use.names = FALSE))
  history = dedup_history(root)
  stopifnot(identical(dates, head(names(history), length(dates))))
  if (length(dates)) {
    stopifnot(dedup_hash(file.path(root, 'commits', tail(dates, 1L),
      'manifest.json')) == bootstrap$initial_commit_sha256)
  } else stopifnot(bootstrap$kind == 'explicit_empty_initialization')
  invisible(TRUE)
}

conalco_mark_bootstrap = \(root) {
  path = file.path(root, 'bootstrap.json')
  if (file.exists(path)) return(conalco_check_bootstrap(root))
  history = dedup_history(root)
  stopifnot(length(history) > 0L)
  conalco_versioned_statistics(root, names(history))
  pending = tempfile('bootstrap_', tmpdir = file.path(root, 'transactions'))
  dedup_json(list(kind = 'explicit_legacy_migration', dates = names(history),
    observations = sum(vapply(history, \(r) r$n_products, integer(1))),
    initial_commit_sha256 = dedup_hash(file.path(root, 'commits',
      tail(names(history), 1L), 'manifest.json'))), pending)
  if (!file.rename(pending, path)) stop('Cannot complete initial migration')
  invisible(path)
}


# Origin: conalco_daily.R, 261029; consolidated 261030.
# 8) OFFLINE PERIOD EXPORT FROM VALIDATED DAILY SHARDS
# <-----------------------------------------------------------------------------

export_conalco_period = \(from, to, output, root = CONALCO_DAILY_ROOT) {
  if (missing(root) && exists('conalco_storage_mode', mode = 'function') &&
      conalco_storage_mode() == 'versioned') root = conalco_versioned_root()
  if (file.exists(file.path(root, 'schema.json')))
    return(conalco_export_versioned_period(from, to, output, root))
  bounds = c(from, to)
  dates = tryCatch(as.Date(bounds, format = '%Y-%m-%d'),
    error = \(e) as.Date(c(NA_character_, NA_character_)))
  if (length(bounds) != 2L || anyNA(dates) ||
      !identical(format(dates, '%Y-%m-%d'), bounds) || dates[1] > dates[2])
    stop('Use an ordered date range in YYYY-MM-DD format')
  root = normalizePath(root, mustWork = TRUE)
  if (length(output) != 1L || is.na(output) || !nzchar(output))
    stop('Choose a new output directory')
  output = file.path(normalizePath(dirname(output), mustWork = TRUE),
    basename(output))
  if (file.exists(output) ||
      isTRUE(nzchar(Sys.readlink(output), keepNA = TRUE)) ||
      startsWith(output, paste0(root, '/')))
    stop('Output must be a new directory outside the daily source')
  paths = sort(list.files(file.path(root, 'runs'), '^manifest[.]json$',
    recursive = TRUE, full.names = TRUE))
  records = lapply(paths, read_json)
  selected = vapply(records, \(record) {
    date = record$date
    if (length(date) != 1L || is.na(date) ||
        !grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', date))
      stop('Invalid daily manifest date')
    date >= from && date <= to
  }, logical(1))
  paths = paths[selected]
  records = records[selected]
  if (!length(records)) stop('No validated daily runs in the selected range')
  for (i in seq_along(records)) {
    record = records[[i]]
    if (!identical(record$run_id, basename(dirname(paths[i]))))
      stop('Daily manifest run identity mismatch')
    if (!identical(record$analysis_version, CONALCO_DAILY_VERSION))
      stop('Daily analysis version changed; explicit migration required')
    # A restored shard lives beside its manifest; original paths stay intact.
    path = file.path(dirname(paths[i]), 'observations.csv.gz')
    if (!file.exists(path) || !identical(
        digest(file = path, algo = 'sha256'), record$observations$sha256))
      stop('Daily shard checksum mismatch: ', path)
    records[[i]]$observations$path = path
  }
  pending = tempfile('.conalco_period_', tmpdir = dirname(output))
  if (!dir.create(pending)) stop('Cannot create export directory')
  on.exit(unlink(pending, recursive = TRUE), add = TRUE)
  receipt = daily_rebuild_views(pending, records)
  receipt$panel$path = file.path(output, 'panel.csv.gz')
  receipt$statistics$path = file.path(output, 'statistics.csv')
  receipt$export_period = list(from = from, to = to, input_root = root,
    observed_dates = sort(vapply(records, `[[`, '', 'date')))
  receipt$source_run_manifests = lapply(paths, refresh_file_record)
  write_json(receipt, file.path(pending, 'manifest.json'), auto_unbox = TRUE,
    pretty = TRUE, na = 'null')
  if (file.exists(output) || !file.rename(pending, output))
    stop('Cannot publish export to a new output directory')
  invisible(receipt)
}
