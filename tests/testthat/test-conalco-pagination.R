# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261031

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) SYNTHETIC CHANGING LISTINGS; NO NETWORK
# <-----------------------------------------------------------------------------

for (module in c('catalog.R', 'derivation.R', 'price_analysis.R',
    'catalog_refresh.R', 'refresh_validation.R', 'snapshot_analysis.R',
    'conalco_daily.R'))
  source(file.path(APM_ROOT, 'R', module), local = F)
for (expression in parse(file.path(APM_ROOT,
    'tests/testthat/test-conalco-daily.R'))) {
  if (is.call(expression) && identical(expression[[1]], as.name('=')) &&
      as.character(expression[[2]]) %in% c('dynamic_test_root',
        'dynamic_client', 'dynamic_prepare')) eval(expression)
}

pagination_client = \(passes) {
  base = dynamic_client(c('schaumwein', 'Katalog'),
    labels = c('Schaumwein', 'Katalog'), date = '2026-10-31')$client
  calls = new.env(parent = emptyenv())
  calls$urls = character()
  calls$pass = 0L
  client = \(url) {
    calls$urls = c(calls$urls, url)
    if (url %in% c('https://www.conalco.de/',
        'https://www.conalco.de/agb')) return(base(url))
    gallery = endsWith(url, '/Katalog/')
    page = if (grepl('?p=', url, fixed = T))
      as.integer(sub('.*[?]p=', '', url)) else 1L
    if (!gallery && page == 1L) calls$pass = calls$pass + 1L
    pass = passes[[min(max(1L, calls$pass), length(passes))]]
    ids = if (gallery) 1L else pass[[page]]
    response = base('https://www.conalco.de/schaumwein/')
    response$request_url = response$final_url = url
    doc = read_html(response$body)
    listing = xml_find_first(doc, '//*[@data-listing="true"]')
    card = xml_find_first(doc, '//*[@data-product-information]')
    xml_remove(card)
    for (id in ids) {
      child = xml_add_child(listing, card, .copy = T)
      info = fromJSON(xml_attr(child, 'data-product-information'))
      info$id = sprintf('%032x', id)
      xml_set_attr(child, 'data-product-information',
        toJSON(info, auto_unbox = T))
    }
    if (gallery) {
      xml_set_attr(listing, 'data-listing', 'false')
      xml_set_attr(listing, 'class', 'cms-block cms-element-product-slider')
    } else {
      xml_set_attr(listing, 'data-listing-pagination-options',
        toJSON(list(page = page), auto_unbox = T))
      last = xml_add_child(xml_add_child(listing, 'nav'), 'li',
        class = 'page-last')
      xml_add_child(last, 'input', name = 'p',
        value = as.character(length(pass)))
      next_page = xml_add_child(listing, 'li', class = 'page-next')
      input = xml_add_child(next_page, 'input', name = 'p',
        value = as.character(page + 1L))
      if (page == length(pass)) xml_set_attr(input, 'disabled', 'disabled')
    }
    response$body = charToRaw(as.character(doc))
    response
  }
  list(client = client, calls = calls)
}

test_that('overlap requires two fresh, complete, identical category passes', {
  local_dir(dynamic_test_root())
  bad = list(1:2, 2:3, 5:6)
  good = list(1:2, 3:4, 5:6)
  fixture = pagination_client(list(bad, good, good))
  dynamic_prepare('pagination_fixture', fixture$client, '2026-10-31')
  state = run_conalco_refresh('pagination_fixture', fixture$client)
  url = 'https://www.conalco.de/schaumwein/'
  review = state$pagination_reviews[[url]]
  expect_true(review$verified)
  expect_equal(review$attempts, 2L)
  expect_length(review$passes, 3L)
  expect_equal(sum(fixture$calls$urls == url), 3L)
  expect_equal(sum(fixture$calls$urls == paste0(url, '?p=3')), 3L)
  expect_equal(length(refresh_check_pagination_reviews(
    state$pagination_reviews)), 9L)
  initial = review$passes[[1]][[1]]$metadata_file
  hash = digest(file = initial, algo = 'sha256')
  manifest = validate_conalco_refresh('pagination_fixture', '')
  record = require_complete_catalog(manifest)
  products = read.csv(record$proof$products$path)
  expect_equal(nrow(products), 6L)
  expect_identical(anyDuplicated(products$retailer_product_id), 0L)
  expect_identical(digest(file = initial, algo = 'sha256'), hash)
  expect_true('pagination_reviews' %in% names(record$proof))
  append_conalco_daily(manifest)
  before = read_conalco_daily_panel()
  append_conalco_daily(manifest)
  expect_identical(read_conalco_daily_panel(), before)
  proof = read_json(record$proof$pagination_reviews$path)
  proof[[url]]$passes[[3]][[1]]$ids[1] = 'tampered'
  write_json(proof, record$proof$pagination_reviews$path, auto_unbox = T)
  record$proof$pagination_reviews = refresh_file_record(
    record$proof$pagination_reviews$path)
  write_json(record, manifest, auto_unbox = T)
  expect_error(require_complete_catalog(manifest))
})

test_that('continued overlap exhausts bounded retries and cannot be complete', {
  local_dir(dynamic_test_root())
  fixture = pagination_client(list(list(1:2, 2:3, 5:6)))
  dynamic_prepare('unstable_fixture', fixture$client, '2026-10-31')
  state = run_conalco_refresh('unstable_fixture', fixture$client)
  url = 'https://www.conalco.de/schaumwein/'
  expect_identical(state$stage, 'partial')
  expect_true(state$pagination_reviews[[url]]$exhausted)
  expect_equal(sum(fixture$calls$urls == url), 2L)
  expect_equal(sum(fixture$calls$urls == paste0(url, '?p=3')), 2L)
  expect_identical(refresh_category_proof(state)$category_status[1],
    'unresolved')
  expect_false(file.exists(file.path(run_dir('unstable_fixture'),
    'collection_finished.json')))
  before = length(fixture$calls$urls)
  run_conalco_refresh('unstable_fixture', fixture$client)
  expect_equal(length(fixture$calls$urls), before)
})

test_that('a changed but disjoint pass needs another matching full pass', {
  local_dir(dynamic_test_root())
  bad = list(1:2, 2:3, 5:6)
  first = list(1:2, 3:4, 5:6)
  changed = list(1:2, c(3, 7), 5:6)
  fixture = pagination_client(list(bad, first, changed, changed))
  dynamic_prepare('changing_fixture', fixture$client, '2026-10-31')
  state = run_conalco_refresh('changing_fixture', fixture$client)
  review = state$pagination_reviews[['https://www.conalco.de/schaumwein/']]
  expect_true(review$verified)
  expect_equal(review$attempts, 3L)
  expect_length(refresh_check_pagination_reviews(state$pagination_reviews),
    12L)
  expect_silent(require_complete_catalog(
    validate_conalco_refresh('changing_fixture', '')))
})

test_that('a changing declared page count restarts the entire category', {
  local_dir(dynamic_test_root())
  good = list(1:2, 3:4, 5:6)
  fixture = pagination_client(list(good, good, good))
  client = \(url) {
    response = fixture$client(url)
    if (fixture$calls$pass == 1L && endsWith(url, '?p=2')) {
      doc = read_html(response$body)
      xml_set_attr(xml_find_first(doc, '//li[@class="page-last"]/input'),
        'value', '4')
      response$body = charToRaw(as.character(doc))
    }
    response
  }
  dynamic_prepare('count_fixture', client, '2026-10-31')
  state = run_conalco_refresh('count_fixture', client)
  review = state$pagination_reviews[['https://www.conalco.de/schaumwein/']]
  expect_true(review$verified)
  expect_identical(review$passes[[1]][[2]]$status,
    'pagination_changed_during_collection')
  expect_silent(require_complete_catalog(
    validate_conalco_refresh('count_fixture', '')))
})

test_that('interrupted category passes retain their request and day budgets', {
  local_dir(dynamic_test_root())
  bad = list(1:2, 2:3, 5:6)
  good = list(1:2, 3:4, 5:6)
  fixture = pagination_client(list(bad, good, good))
  dynamic_prepare('budget_fixture', fixture$client, '2026-10-31')
  expect_error(run_conalco_refresh('budget_fixture', fixture$client,
    max_requests = 6L), 'budget reached')
  state = readRDS(file.path(run_dir('budget_fixture'), 'checkpoint.rds'))
  url = 'https://www.conalco.de/schaumwein/'
  expect_equal(state$pagination_reviews[[url]]$attempts, 1L)
  first = state$pages[[paste0(url, '|1')]]
  state = run_conalco_refresh('budget_fixture', fixture$client)
  expect_true(state$pagination_reviews[[url]]$verified)
  expect_identical(state$pagination_reviews[[url]]$passes[[2]][[1]], first)
  expect_silent(require_complete_catalog(
    validate_conalco_refresh('budget_fixture', '')))
})

test_that('stable categories keep the existing single traversal', {
  local_dir(dynamic_test_root())
  fixture = pagination_client(list(list(1:2, 3:4, 5:6)))
  dynamic_prepare('stable_fixture', fixture$client, '2026-10-31')
  state = run_conalco_refresh('stable_fixture', fixture$client)
  expect_null(state$pagination_reviews)
  expect_equal(sum(grepl('/schaumwein/', fixture$calls$urls, fixed = T)), 3L)
  expect_silent(require_complete_catalog(
    validate_conalco_refresh('stable_fixture', '')))
})
