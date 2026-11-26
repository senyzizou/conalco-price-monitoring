# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261026

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

script_args = commandArgs(trailingOnly = F)
script_file = sub('^--file=', '', script_args[grepl('^--file=',
    script_args)][1])
source(file.path(dirname(script_file), 'bootstrap.R'))
load_project()
for (module in c('catalog.R', 'derivation.R', 'price_analysis.R',
    'catalog_refresh.R', 'refresh_validation.R', 'snapshot_analysis.R',
    'conalco_daily.R')) source(file.path('R', module))
args = commandArgs(trailingOnly = T)
stopifnot(all(args == '--sources' | grepl('^--run-id=[A-Za-z0-9_]+$',
      args)))
selected = sub('^--run-id=', '', args[startsWith(args, '--run-id=')])
stopifnot(length(selected) <= 1L)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) OFFLINE COLLECTION REPLAY USING THE EXISTING PARSERS
# <-----------------------------------------------------------------------------

# Adapted from verify_conalco_raw.R (original start: 261020).
verify_daily_sources = \(manifest, products) {
  pages = read.csv(manifest$proof$page_completeness$path,
    stringsAsFactors = F)
  coverage = read.csv(manifest$proof$category_completeness$path,
    stringsAsFactors = F)
  inventory = read.csv(manifest$proof$raw_inventory$path,
    stringsAsFactors = F)
  for (path in inventory$metadata_file) read_archive(path)
  parsed = lapply(seq_len(nrow(pages)), \(i) {
    page = pages[i, ]
    name = coverage$category_name[match(page$category_url,
        coverage$category_url)]
    result = refresh_page(read_json(page$metadata_file), name,
      page$category_url, page$page)
    if (i %% 100L == 0L) {
      cat(manifest$run_id, ': raw pages', i, '/', nrow(pages), '\n')
      flush.console()
    }
    result$rows
  })
  listings = bind_rows(parsed)
  memberships = unique(listings[c('retailer_product_id', 'retailer_category',
        'category_url', 'category_page', 'listing_metadata_file')])
  rebuilt = listings[!duplicated(listings$retailer_product_id), ]
  stopifnot(nrow(rebuilt) == nrow(products),
    setequal(rebuilt$retailer_product_id, products$retailer_product_id))
  for (i in which(!is.na(products$detail_metadata_file))) {
    key = products$retailer_product_id[i]
    j = match(key, rebuilt$retailer_product_id)
    path = products$detail_metadata_file[i]
    detail = catalog_detail(path, rebuilt[j, ])
    doc = read_html(read_archive(path)$body, encoding = 'UTF-8')
    description = node_text_verbatim(doc, paste0('//*[',
        class_xpath('product-detail-description-text'), ']'))
    stopifnot(nchar(description) >= 20L, detail$retailer_product_id == key)
    detail = attach_conalco_description_evidence(detail, description)
    detail$detail_coverage_method = 'individual_product_page'
    detail$listing_metadata_file = rebuilt$listing_metadata_file[j]
    for (field in setdiff(names(detail), names(rebuilt)))
      rebuilt[[field]] = NA
    rebuilt[j, names(detail)] = detail
  }
  by_id = split(memberships, memberships$retailer_product_id)
  rebuilt$retailer_category = vapply(rebuilt$retailer_product_id,
    \(key) paste(unique(by_id[[key]]$retailer_category), collapse = ' | '),
    character(1))
  rebuilt$category_urls = vapply(rebuilt$retailer_product_id,
    \(key) paste(unique(by_id[[key]]$category_url), collapse = ' | '),
    character(1))
  rebuilt$in_rtd_rts_category = rebuilt$retailer_product_id %in%
    memberships$retailer_product_id[memberships$category_url %in%
      c('https://www.conalco.de/ready-to-drink/',
        'https://www.conalco.de/Ready-to-Serve')]
  actual = csv_values(rebuilt)
  expected = daily_read_csv(manifest$proof$products$path)
  actual = actual[match(expected$retailer_product_id,
      actual$retailer_product_id), ]
  fields = setdiff(intersect(names(actual), names(expected)),
    'parser_version')
  compare_tables(actual[fields], expected[fields])
  cat('Raw collection fields reproduced:', nrow(actual), '\n')
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) UNCHANGED OBSERVATIONS, ABV BOUNDARY AND BOTH PRICE MEASURES
# <-----------------------------------------------------------------------------

csv_values = \(x) {
  path = tempfile(fileext = '.csv')
  on.exit(unlink(path))
  write.csv(x, path, row.names = F, na = '')
  daily_read_csv(path)
}

compare_tables = \(actual, expected) {
  stopifnot(identical(names(actual), names(expected)),
    nrow(actual) == nrow(expected))
  for (field in names(expected)) {
    a = actual[[field]]
    b = expected[[field]]
    if (!identical(is.na(a), is.na(b))) stop('Missingness differs: ', field)
    present = !is.na(a)
    if (!identical(a[present], b[present])) {
      numeric_a = suppressWarnings(as.numeric(a[present]))
      numeric_b = suppressWarnings(as.numeric(b[present]))
      if (anyNA(c(numeric_a, numeric_b)) || !isTRUE(all.equal(
            numeric_a, numeric_b, tolerance = 1e-12)))
        stop('Reproduction differs: ', field)
    }
  }
  invisible(T)
}

records = daily_run_records(CONALCO_DAILY_ROOT)
if (length(selected)) {
  records = Filter(\(record) record$run_id == selected, records)
  stopifnot(length(records) == 1L)
}
statistics = list()
for (record in records) {
  day = daily_read_csv(record$observations$path)
  eligible = day$daily_price_eligible == 'TRUE'
  eligible[is.na(eligible)] = F
  abv = as.numeric(day$alcohol_content)
  volume = as.numeric(day$volume_l)
  price = as.numeric(day$price_current)
  stopifnot(all(abv[eligible] >= 0.5), all(volume[eligible] > 0),
    all(price[eligible] > 0), all(day$minimum_abv_percent == '0.5'))
  stopifnot(isTRUE(all.equal(
      as.numeric(day$price_eur_per_l[eligible]),
      price[eligible] / volume[eligible], tolerance = 1e-12)))
  stopifnot(isTRUE(all.equal(
      as.numeric(day$price_eur_per_10g[eligible]),
      price[eligible] * 10 / (volume[eligible] * 1000 *
        abv[eligible] / 100 * 0.789), tolerance = 1e-12)))
  stopifnot(all(is.na(day$price_eur_per_l[!eligible])),
    all(is.na(day$price_eur_per_10g[!eligible])))
  if ('--sources' %in% args) {
    source = record$source_manifest
    stopifnot(identical(digest(file = source$path, algo = 'sha256'),
        source$sha256))
    manifest = require_complete_catalog(source$path)
    stopifnot(identical(manifest$run_id, record$run_id))
    products = read_observations(manifest$proof$products$path)
    require_complete_catalog(source$path, products)
    verify_daily_sources(manifest, products)
    review = if (is.null(record$conflict_review)) NULL else
      record$conflict_review$path
    rebuilt = daily_analyse_products(products, review)
    compare_tables(csv_values(rebuilt), day)
  }
  statistics[[record$run_id]] = daily_statistics(day)
  cat('Daily formulas and source analysis passed:', record$date,
    nrow(day), '\n')
  flush.console()
}
actual = csv_values(bind_rows(statistics))
expected = daily_read_csv(file.path(CONALCO_DAILY_ROOT, 'statistics.csv'))
if (length(selected)) expected = expected[expected$run_id == selected, ]
keys = c('retailer', 'date', 'run_id', 'partition', 'category',
  'price_measure')
actual_key = do.call(paste, c(actual[keys], sep = '|'))
expected_key = do.call(paste, c(expected[keys], sep = '|'))
stopifnot(!anyDuplicated(actual_key), !anyDuplicated(expected_key),
  setequal(actual_key, expected_key))
compare_tables(actual[match(expected_key, actual_key), ], expected)
cat('All daily statistics reproduced with the existing pipeline.\n')
