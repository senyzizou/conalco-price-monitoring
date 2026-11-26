# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261029

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) EXPLICIT SYNTHETIC FIXTURES; NEVER HISTORICAL INPUTS
# <-----------------------------------------------------------------------------

versioned_test_workspace = \() {
  root = tempfile('SYNTHETIC_VERSIONED_')
  dir.create(root)
  files = c('R/storage.R', 'R/storage_transport.R',
    'R/classification.R', 'R/quality.R', 'R/price_analysis.R',
    'R/snapshot_analysis.R', 'R/conalco_daily.R', 'renv.lock',
    'config/conalco_versioned_schema.json')
  conalco_copy_files(APM_ROOT, root, files)
  root
}

versioned_test_rows = \(date = '2030-01-01', n = 3L) {
  s = jsonlite::read_json(file.path(APM_ROOT,
    'config/conalco_versioned_schema.json'), simplifyVector = T)
  x = as.data.frame(setNames(rep(list(rep(NA_character_, n)),
    length(s$columns)), s$columns), stringsAsFactors = F)
  x$retailer = 'conalco'
  x$retailer_product_id = paste0('SYNTHETIC_', seq_len(n))
  x$product_key = paste0('conalco:', x$retailer_product_id)
  x$run_id = paste0('SYNTHETIC_', gsub('-', '', date, fixed = T))
  x$observed_at = paste0(date, 'T08:00:00.000000Z')
  x$daily_date = date
  x$date = date
  x$observation_key = paste(x$product_key, x$run_id, sep = ':')
  x$product_name = 'EXPLICIT SYNTHETIC TEST PRODUCT'
  x$product_url = paste0('https://example.invalid/', x$retailer_product_id)
  x$price_current = '1'
  x$price_reference = '1.5'
  x$abv = '5'
  x$alcohol_content = '5'
  x$volume_l = '0.5'
  x$pack_count = '1'
  x$unit_volume_l = '0.5'
  x$main_category = 'Bier'
  x$daily_category_exact15 = 'Bier'
  x$daily_category_tax_le15 = 'Bier'
  x$population_status = 'included_ge_0.5'
  x$daily_price_eligible = 'TRUE'
  x$price_analysis_eligible = 'TRUE'
  x$price_eur_per_l = '2'
  x$price_eur_per_10g = as.character(10 / (500 * .05 * .789))
  x$availability = 'in_stock'
  x$daily_exclusion_reasons = ''
  x$archived_description_raw = 'SYNTHETIC description A: ä, "quotes"\nline 2'
  x$detail_coverage_method =
    'same_product_full_description_embedded_in_listing'
  x$listing_metadata_file = paste0('synthetic/', x$run_id, '/listing.json')
  x$raw_sha256 = digest::digest('SYNTHETIC', algo = 'sha256')
  x
}

versioned_test_ingest = \(store, x, fail_at = NULL) {
  directory = tempfile('SYNTHETIC_INPUT_')
  dir.create(directory)
  shard = file.path(directory, 'observations.csv.gz')
  dedup_write(x, shard, level = 9L)
  receipt = list(date = x$daily_date[1], run_id = x$run_id[1],
    n_products = nrow(x), analysis_version = CONALCO_DAILY_VERSION,
    source_manifest = list(path = 'synthetic/manifest.json',
      sha256 = digest::digest(x$run_id[1], algo = 'sha256')),
    observations = list(path = shard, sha256 = dedup_hash(shard)))
  path = file.path(directory, 'manifest.json')
  dedup_json(receipt, path)
  dedup_ingest(store, shard, path, fail_at = fail_at,
    statistics = daily_statistics(dedup_panel_values(x)),
    statistics_origin = list(kind = 'SYNTHETIC_TEST_ONLY'))
}

