# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) CONSOLIDATED MODULES; ALL DATA BELOW ARE SYNTHETIC
# <-----------------------------------------------------------------------------

test_that('usage is reconstructed exactly and old stored usage remains valid', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('derived', initial_empty = TRUE)
  s = dedup_schema('derived')
  dedup_init('stored', s$columns, s$tracked, s$core, usage_mode = 'stored')
  for (date in c('2030-01-01', '2030-01-02')) {
    x = versioned_test_rows(date)
    if (date == '2030-01-02') x$archived_description_raw[2] = 'SYNTHETIC B'
    versioned_test_ingest('derived', x)
    versioned_test_ingest('stored', x)
    expect_false(file.exists(file.path('derived', 'commits', date,
      'version_usage.csv.gz')))
  }
  expect_identical(dedup_table('derived', 'version_usage.csv.gz'),
    dedup_table('stored', 'version_usage.csv.gz'))
  expect_identical(dedup_restore_rows('derived', '2030-01-01', '2030-01-02'),
    dedup_restore_rows('stored', '2030-01-01', '2030-01-02'))
  expect_identical(dedup_version_view('derived'), dedup_version_view('stored'))
})

test_that('a second explicit retailer adapter uses the same storage engine', {
  local_dir(versioned_test_workspace())
  schema = jsonlite::read_json('config/conalco_versioned_schema.json',
    simplifyVector = TRUE)
  profile = storage_profile('synthetic_shop', schema$columns,
    schema$tracked, schema$core)
  storage_init('shop', profile)
  x = versioned_test_rows()
  x$retailer = 'synthetic_shop'
  x$product_key = paste(x$retailer, x$retailer_product_id, sep = ':')
  x$observation_key = paste(x$product_key, x$run_id, sep = ':')
  versioned_test_ingest('shop', x)
  expect_identical(dedup_restore_rows('shop', '2030-01-01', '2030-01-01'), x)
  storage_export_panel('shop', '2030-01-01', '2030-01-01', 'shop.csv.gz')
  expect_identical(dedup_read('shop.csv.gz'), x)
  expect_error(storage_profile('unknown',
    setdiff(schema$columns, 'retailer_product_id'), schema$tracked,
    schema$core))
})

test_that('generic archive primitives restore and verify all payload files', {
  local_dir(versioned_test_workspace())
  dir.create('payload')
  writeLines('SYNTHETIC TEST PAYLOAD', 'payload/a.txt')
  receipt = storage_pack_files('payload', 'a.txt', 'payload.tar.gz')
  storage_restore_files('payload.tar.gz', receipt, 'restored_payload')
  expect_identical(dedup_hash('payload/a.txt'),
    dedup_hash('restored_payload/a.txt'))
  expect_error(storage_restore_files('payload.tar.gz', receipt,
    'restored_payload'))
})

test_that('concurrent writers cannot expose duplicate or partial commits', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = TRUE)
  x = versioned_test_rows()
  shard = file.path(getwd(), 'SYNTHETIC.csv.gz')
  dedup_write(x, shard)
  receipt = list(date = x$daily_date[1], run_id = x$run_id[1],
    n_products = nrow(x), source_manifest = list(sha256 = 'SYNTHETIC'),
    observations = list(path = shard, sha256 = dedup_hash(shard)))
  metadata = file.path(getwd(), 'SYNTHETIC.json')
  dedup_json(receipt, metadata)
  store = file.path(getwd(), 'store')
  child = callr::r_bg(\(project, store, shard, metadata) {
    source(file.path(project, 'R/storage.R'))
    original = dedup_json
    dedup_json = \(x, path) {
      original(x, path)
      if (basename(path) == 'owner.json') Sys.sleep(1)
    }
    dedup_ingest(store, shard, metadata)
  }, args = list(project = APM_ROOT, store = store, shard = shard,
    metadata = metadata), libpath = .libPaths(),
    system_profile = FALSE, user_profile = FALSE)
  on.exit(if (child$is_alive()) child$kill(), add = TRUE)
  for (i in seq_len(200L)) {
    if (file.exists(file.path(store, '.writer_lock/owner.json'))) break
    Sys.sleep(.025)
  }
  expect_true(file.exists(file.path(store, '.writer_lock/owner.json')))
  expect_error(dedup_ingest(store, shard, metadata), 'Writer lock')
  child$wait(timeout = 10000)
  expect_identical(child$get_result(), 'committed')
  expect_identical(dedup_ingest(store, shard, metadata), 'already_committed')
  expect_length(dedup_history(store), 1L)
})

test_that('explicit snapshot import verifies the whole current local state', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = TRUE)
  versioned_test_ingest('store', versioned_test_rows())
  conalco_legacy_view('store', 'legacy', daily_root = CONALCO_DAILY_ROOT)
  packed = with_dir('legacy', actions_state_pack(file.path(
    dirname(getwd()), 'SYNTHETIC_ARCHIVE')))
  expect_error(conalco_import_state(packed$commit_file, packed$archive,
    packed$sidecar, 'too_old', '2030-01-02'), 'required through')
  expect_false(dir.exists('too_old'))
  conalco_import_state(packed$commit_file, packed$archive, packed$sidecar,
    'imported', '2030-01-01', expected_run = 'SYNTHETIC_20300101',
    expected_observations = 3L)
  expect_identical(dedup_restore_rows('imported/canonical', '2030-01-01',
    '2030-01-01'), versioned_test_rows())
  expect_false(read_json('imported/import_receipt.json')$activated)
  expect_identical(conalco_storage_mode(), 'legacy')
})
