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

for (module in c('storage.R', 'storage_transport.R',
    'conalco_daily.R'))
  source(file.path(APM_ROOT, 'R', module), local = F)

test_that('all three deployment routes default to legacy', {
  local_dir(versioned_test_workspace())
  expect_identical(conalco_storage_mode(), 'legacy')
  expect_identical(conalco_storage_mode('github_publication'), 'legacy')
  expect_identical(conalco_storage_mode('ukebox_results'), 'legacy')
  expect_false(dir.exists(conalco_versioned_root()))
  local_options(apm.conalco.storage = list(ukebox_results = 'incremental'))
  expect_identical(conalco_storage_mode(), 'legacy')
  expect_identical(conalco_storage_mode('github_publication'), 'legacy')
  expect_identical(conalco_storage_mode('ukebox_results'), 'incremental')
})

test_that('CSV preserves missing, empty, quotes, UTF-8 and embedded newlines', {
  x = data.frame(a = c(NA_character_, '', 'NA', 'ä 🥃', 'a,"b"\n,,c'),
    b = c('', NA_character_, ' spaced ', '', 'x'))
  path = tempfile(fileext = '.csv.gz')
  dedup_write(x, path)
  expect_identical(dedup_read(path), x)
  expect_error(dedup_write(x, path), 'overwrite')
  expect_equal(length(unique(vapply(list(NA_character_, '', 'NA'),
    dedup_content, ''))), 3L)
})

test_that('price changes retain versions and changed details create episodes', {
  local_dir(versioned_test_workspace())
  store = 'store'
  conalco_init_versioned(store, initial_empty = T)
  x = versioned_test_rows()
  versioned_test_ingest(store, x)
  initial = dedup_table(store, 'product_versions.csv.gz')
  original_hash = dedup_hash('store/commits/2030-01-01/product_versions.csv.gz')
  expect_identical(versioned_test_ingest(store, x), 'already_committed')
  y = versioned_test_rows('2030-01-02')
  y$price_current[1] = '0.5'
  versioned_test_ingest(store, y)
  expect_equal(nrow(dedup_table(store, 'product_versions.csv.gz')), 3L)
  z = versioned_test_rows('2030-01-03', n = 4L)
  z$archived_description_raw[2] = 'SYNTHETIC description B'
  z$abv[3] = '6'
  z$alcohol_content[3] = '6'
  z$main_category[1] = 'SYNTHETIC CATEGORY'
  versioned_test_ingest(store, z)
  expect_equal(nrow(dedup_table(store, 'product_versions.csv.gz')), 7L)
  expect_identical(dedup_hash(
    'store/commits/2030-01-01/product_versions.csv.gz'), original_hash)
  a = versioned_test_rows('2030-01-04', n = 4L)
  versioned_test_ingest(store, a)
  versions = dedup_table(store, 'product_versions.csv.gz')
  expect_equal(nrow(versions), 10L)
  expect_false(anyDuplicated(versions$product_version_id) > 0L)
  repeated = versions[versions$retailer_product_id == 'SYNTHETIC_2', ]
  expect_identical(repeated$content_sha256[1], repeated$content_sha256[3])
  expect_false(repeated$product_version_id[1] == repeated$product_version_id[3])
  expect_identical(dedup_restore_rows(store, '2030-01-01', '2030-01-01'), x)
  expect_identical(dedup_restore_rows(store, '2030-01-03', '2030-01-03'), z)
  changes = dedup_table(store, 'product_changes.csv.gz')
  expect_true(all(c('abv', 'main_category', 'archived_description_raw') %in%
    changes$field))
  change = changes[changes$field == 'archived_description_raw', ][1, ]
  values = dedup_change_values(store, change)
  expect_identical(values$old_value, x$archived_description_raw[2])
  expect_identical(values$new_value, z$archived_description_raw[2])
  expect_false(any(c('old_value', 'new_value') %in% names(changes)))
  expect_equal(nrow(initial), 3L)
})

test_that('NA and empty states create explicit non-destructive transitions', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  values = list(NA_character_, '', 'SYNTHETIC INGREDIENT', NA_character_)
  for (i in seq_along(values)) {
    x = versioned_test_rows(sprintf('2030-02-%02d', i), n = 1L)
    x$ingredients_raw = values[[i]]
    versioned_test_ingest('store', x)
  }
  changes = dedup_table('store', 'product_changes.csv.gz')
  expect_setequal(changes$change_type, c('initial_product',
    'missing_to_empty', 'empty_to_known', 'known_to_missing'))
  expect_equal(nrow(dedup_table('store', 'product_versions.csv.gz')), 4L)
  view = dedup_version_view('store')
  expect_identical(view$first_observed_date, view$last_observed_date)
  expect_true(all(is.na(view$last_source_checked_at)))
})

test_that('interrupted writes never expose partial observations', {
  for (point in c('after_versions', 'before_commit')) {
    local_dir(versioned_test_workspace())
    conalco_init_versioned('store', initial_empty = T)
    versioned_test_ingest('store', versioned_test_rows())
    next_day = versioned_test_rows('2030-01-02')
    expect_error(versioned_test_ingest('store', next_day, point),
      'Injected interruption')
    expect_equal(length(dedup_history('store')), 1L)
    expect_equal(nrow(dedup_restore_rows('store', '2030-01-01',
      '2030-01-01')), 3L)
    expect_identical(versioned_test_ingest('store', next_day), 'committed')
    expect_equal(length(dedup_history('store')), 2L)
  }
})

test_that('missing and damaged version files fail closed', {
  for (kind in c('missing', 'damaged')) {
    local_dir(versioned_test_workspace())
    conalco_init_versioned('store', initial_empty = T)
    versioned_test_ingest('store', versioned_test_rows())
    path = 'store/commits/2030-01-01/product_versions.csv.gz'
    if (kind == 'missing') {
      expect_true(file.rename(path, paste0(path, '.missing')))
    } else {
      connection = file(path, 'ab')
      writeBin(charToRaw('INTENTIONAL SYNTHETIC DAMAGE'), connection)
      close(connection)
    }
    expect_error(dedup_restore_rows('store', '2030-01-01', '2030-01-01'),
      'Corrupt or missing')
  }
})

test_that('complete local publication requires manual approval every time', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  versioned_test_ingest('store', versioned_test_rows())
  for (mode in c('full', 'preflight', '')) {
    expect_error(conalco_prepare_publication('store', tempfile(), mode,
      'schedule', offline = T), 'manual')
  }
  expect_error(conalco_prepare_publication('store', tempfile(), 'publish',
    'schedule'), 'manual')
  output = tempfile('SYNTHETIC_PUBLICATION_')
  plan = conalco_prepare_publication('store', output, 'publish',
    'local_test', offline = T)
  expect_silent(conalco_verify_package(output))
  expect_false(any(grepl('panel[.]csv[.]gz$', plan$files$path)))
  expect_false(any(plan$files$path == '.gitattributes'))
  state = new.env(parent = emptyenv())
  state$calls = list()
  git = \(args) {
    state$calls[[length(state$calls) + 1L]] = args
    output = character()
    if (args[1] == 'rev-parse') output = 'SYNTHETIC_HEAD'
    if (args[1] == 'check-attr') output = paste0(plan$files$path,
      ': filter: unspecified')
    if (args[1] == 'diff' && any(vapply(state$calls,
        \(a) a[1] == 'add', logical(1)))) output = plan$files$path
    list(status = 0L, output = output)
  }
  expect_error(conalco_commit_publication(plan, git, 'full',
    'schedule', offline = T), 'manual')
  expect_length(state$calls, 0L)
  dedup_json(plan, 'SYNTHETIC_PUBLICATION_PLAN.json')
  saved_plan = jsonlite::read_json('SYNTHETIC_PUBLICATION_PLAN.json')
  expect_identical(conalco_commit_publication(saved_plan, git, 'publish',
    'local_test', offline = T), 'published')
  expect_equal(sum(vapply(state$calls, \(a) a[1] == 'push', logical(1))), 1L)
  expect_false(any(vapply(state$calls, \(a) any(a == 'lfs'), logical(1))))
})

test_that('incremental result backups restore without the original store', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  versioned_test_ingest('store', versioned_test_rows())
  one = conalco_pack_increment('store', 'backup_one', '2030-01-01')
  versioned_test_ingest('store', versioned_test_rows('2030-01-02'))
  two = conalco_pack_increment('store', 'backup_two', '2030-01-02',
    previous = one$checkpoint)
  expect_true(file.rename('store', 'hidden_original_store'))
  conalco_restore_increments(c(one$archive, two$archive),
    c(one$sidecar, two$sidecar), 'restored', expected = two$checkpoint)
  root = file.path('restored', two$checkpoint$store_root)
  expect_equal(nrow(dedup_restore_rows(root, '2030-01-01', '2030-01-02')), 6L)
  expect_error(conalco_restore_increments(one$archive, one$sidecar,
    'incomplete', expected = two$checkpoint))
  expect_false(dir.exists('incomplete'))
  connection = file(two$archive, 'ab')
  writeBin(charToRaw('INTENTIONAL DAMAGE'), connection)
  close(connection)
  expect_error(conalco_restore_increments(c(one$archive, two$archive),
    c(one$sidecar, two$sidecar), 'damaged'))
  expect_false(dir.exists('damaged'))
})

test_that('versioned storage keeps UKEbox as the production destination', {
  workflow = readLines(file.path(APM_ROOT,
    '.github/workflows/conalco_daily.yml'), warn = F)
  expect_false(any(grepl('^  publish:|--publish|contents: write', workflow)))
  expect_true(any(grepl('operation=--full', workflow, fixed = T)))
  config = read_json(file.path(APM_ROOT, 'config/conalco_storage.json'))
  expect_identical(config$ukebox_results, 'incremental')
})

test_that('the validated append hook is opt-in and idempotent', {
  local_dir(versioned_test_workspace())
  local_options(apm.conalco.storage = list(local_storage = 'versioned'))
  conalco_init_versioned(conalco_versioned_root(), initial_empty = T)
  x = versioned_test_rows()
  path = 'SYNTHETIC_CATALOG_MANIFEST.json'
  manifest = list(run_id = x$run_id[1], status = 'complete',
    proof = list(products = list(path = 'SYNTHETIC_ONLY.csv')))
  dedup_json(manifest, path)
  state = new.env(parent = emptyenv())
  state$analyses = 0L
  state$valid = T
  scope = new.env(parent = environment(append_conalco_daily))
  scope$require_complete_catalog = \(path, raw = NULL) {
    if (!state$valid) stop('SYNTHETIC incomplete collection')
    manifest
  }
  scope$read_observations = \(path) x
  scope$daily_analyse_products = \(raw, review) {
    state$analyses = state$analyses + 1L
    raw
  }
  append = append_conalco_daily
  storage_append = conalco_append_versioned
  environment(append) = scope
  environment(storage_append) = scope
  scope$conalco_append_versioned = storage_append
  append(path)
  expect_equal(state$analyses, 1L)
  append(path)
  expect_equal(state$analyses, 1L)
  expect_identical(dedup_restore_rows(conalco_versioned_root(),
    '2030-01-01', '2030-01-01'), x)
  expect_false(dir.exists(CONALCO_DAILY_ROOT))
  state$valid = F
  expect_error(append(path), 'incomplete collection')
  expect_equal(length(dedup_history(conalco_versioned_root())), 1L)
})

test_that('legacy rollback reconstructs the original analysis interface', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  x = versioned_test_rows()
  versioned_test_ingest('store', x)
  before = vapply(list.files('store/commits', recursive = T,
    full.names = T), dedup_hash, '')
  conalco_legacy_view('store', 'rollback', daily_root = CONALCO_DAILY_ROOT)
  restored = with_dir('rollback', read_conalco_daily_panel())
  expect_identical(restored, dedup_panel_values(x))
  expect_identical(before, vapply(list.files('store/commits', recursive = T,
    full.names = T), dedup_hash, ''))
  expect_length(with_dir('rollback', daily_run_records(CONALCO_DAILY_ROOT)), 1L)
  expect_true(file.exists(file.path('rollback', CONALCO_DAILY_ROOT,
    'statistics.csv')))
})

test_that('increment publication commits last and retries perform no rewrite', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  versioned_test_ingest('store', versioned_test_rows())
  state = new.env(parent = emptyenv())
  state$objects = list()
  state$calls = character()
  publish = \(source, path) {
    state$calls = c(state$calls, path)
    state$objects[[path]] = readBin(source, 'raw', n = file.size(source))
    invisible('SYNTHETIC LOCAL TRANSPORT')
  }
  first = conalco_publish_increment('store', 'first_publish', NULL, publish)
  expect_match(tail(state$calls, 1L), '/commits/2030-01-01.json', fixed = T)
  versioned_test_ingest('store', versioned_test_rows('2030-01-02'))
  # Match the JSON representation used by the real adapter, without networking.
  dedup_json(first, 'SYNTHETIC_CHECKPOINT.json')
  previous = jsonlite::read_json('SYNTHETIC_CHECKPOINT.json')
  second = conalco_publish_increment('store', 'second_publish', previous,
    publish)
  expect_length(second$objects, 2L)
  expect_length(second$dependencies, 1L)
  before_retry = length(state$calls)
  expect_identical(conalco_publish_increment('store', 'retry_publish',
    second, publish), second)
  expect_equal(length(state$calls), before_retry)
  expect_false(dir.exists('retry_publish'))
  expect_match(tail(state$calls, 1L), '/commits/2030-01-02.json', fixed = T)
  fetch = \(path, output) {
    bytes = state$objects[[path]]
    if (is.null(bytes)) return(F)
    writeBin(bytes, output)
    T
  }
  conalco_fetch_increment(second, 'recovered_from_transport', fetch)
  expect_equal(nrow(dedup_restore_rows(file.path('recovered_from_transport',
    second$store_root), '2030-01-01', '2030-01-02')), 6L)
  failure = \(source, path) {
    if (grepl('/commits/', path, fixed = T)) stop('SYNTHETIC final PUT failure')
    publish(source, path)
  }
  versioned_test_ingest('store', versioned_test_rows('2030-01-03'))
  expect_error(conalco_publish_increment('store', 'failed_publish', second,
    failure), 'final PUT failure')
  expect_null(state$objects[[paste0(conalco_increment_base(),
    '/commits/2030-01-03.json')]])
})

test_that('initial migration is explicit, complete and repeatable', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store')
  expect_error(conalco_check_bootstrap('store'), 'not complete')
  versioned_test_ingest('store', versioned_test_rows())
  conalco_mark_bootstrap('store')
  expect_silent(conalco_check_bootstrap('store'))
  conalco_legacy_view('store', 'original', daily_root = 'daily')
  source_files = list.files('original', recursive = T, full.names = T)
  hashes = vapply(source_files, dedup_hash, '')
  conalco_migrate_legacy('original/daily', 'migrated')
  expect_identical(dedup_restore_rows('migrated', '2030-01-01',
    '2030-01-01'), versioned_test_rows())
  expect_silent(conalco_check_bootstrap('migrated'))
  committed = dedup_hash('migrated/commits/2030-01-01/manifest.json')
  conalco_migrate_legacy('original/daily', 'migrated')
  expect_identical(committed,
    dedup_hash('migrated/commits/2030-01-01/manifest.json'))
  expect_identical(hashes, vapply(source_files, dedup_hash, ''))
})

test_that('frozen decimal statistics are exported without recalculation', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  x = versioned_test_rows()
  directory = tempfile('SYNTHETIC_FROZEN_STATISTICS_')
  dir.create(directory)
  shard = file.path(directory, 'observations.csv.gz')
  dedup_write(x, shard)
  statistics = as.data.frame(lapply(daily_statistics(dedup_panel_values(x)),
    as.character), stringsAsFactors = F)
  statistics$mean[1] = '39.7825545459864'
  receipt = list(date = x$daily_date[1], run_id = x$run_id[1],
    n_products = nrow(x), source_manifest = list(sha256 = 'SYNTHETIC'),
    observations = list(path = shard, sha256 = dedup_hash(shard)))
  path = file.path(directory, 'manifest.json')
  dedup_json(receipt, path)
  dedup_ingest('store', shard, path, statistics = statistics)
  scope = new.env(parent = environment(conalco_export_versioned_period))
  scope$daily_statistics = \(...) stop('Historical recalculation forbidden')
  export = conalco_export_versioned_period
  environment(export) = scope
  export('2030-01-01', '2030-01-01', 'period', 'store')
  expect_identical(dedup_read('period/statistics.csv'), statistics)
  expect_error(dedup_ingest('store', shard, path, statistics =
    daily_statistics(dedup_panel_values(x))))
})

test_that('run and observation identities are globally unique', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  first = versioned_test_rows()
  versioned_test_ingest('store', first)
  second = versioned_test_rows('2030-01-02')
  second$run_id = first$run_id
  expect_error(versioned_test_ingest('store', second))
  second = versioned_test_rows('2030-01-02')
  second$observation_key[1] = first$observation_key[1]
  expect_error(versioned_test_ingest('store', second))
  expect_length(dedup_history('store'), 1L)
})

test_that('new local storage retains original raw and legacy result formats', {
  local_dir(versioned_test_workspace())
  local_options(apm.conalco.storage = list(local_storage = 'versioned'))
  root = conalco_versioned_root()
  dir.create(dirname(root), recursive = T)
  conalco_init_versioned(root, initial_empty = T)
  x = versioned_test_rows()
  versioned_test_ingest(root, x)
  scope = new.env(parent = environment(conalco_raw_bridge))
  scope$require_complete_catalog = \(path) list(run_id = x$run_id[1])
  bridge = conalco_raw_bridge
  environment(bridge) = scope
  daily = bridge('SYNTHETIC_SOURCE_MANIFEST.json')
  raw_shard = file.path(daily, 'runs', x$run_id[1], 'observations.csv.gz')
  expect_identical(dedup_read(raw_shard), x)
  expect_identical(bridge('SYNTHETIC_SOURCE_MANIFEST.json'), daily)
  dir.create('result_output')
  history = list(SYNTHETIC = 'validated')
  scope = new.env(parent = environment(conalco_legacy_state_publish))
  scope$actions_history_unchanged = \(value)
    stopifnot(identical(value, history))
  scope$actions_history = \() history
  scope$actions_state_publish = \(dav, history, output) {
    expect_identical(conalco_storage_mode(), 'legacy')
    expect_identical(read_conalco_daily_panel(), dedup_panel_values(x))
    expect_silent(conalco_check_bootstrap(file.path(CONALCO_DAILY_ROOT,
      'versioned_bridge')))
    'SYNTHETIC LEGACY ARCHIVE TRANSPORT'
  }
  publish = conalco_legacy_state_publish
  environment(publish) = scope
  expect_identical(publish(NULL, history, 'result_output/archive'),
    'SYNTHETIC LEGACY ARCHIVE TRANSPORT')
  expect_identical(conalco_storage_mode(), 'versioned')
})

test_that('incremental archive restore can retain legacy Git shard bytes', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  x = versioned_test_rows()
  versioned_test_ingest('store', x)
  conalco_legacy_view('store', 'checkout', daily_root = CONALCO_DAILY_ROOT)
  conalco_copy_files('checkout', getwd(),
    list.files('checkout', recursive = T))
  shard = file.path(CONALCO_DAILY_ROOT, 'runs', x$run_id[1],
    'observations.csv.gz')
  original_hash = dedup_hash(shard)
  conalco_legacy_view('store', 'restored', daily_root = CONALCO_DAILY_ROOT)
  restored = conalco_legacy_publication_view(NULL, 'restored',
    list(versioned_store = 'store'))
  expect_identical(dedup_hash(file.path('restored', shard)), original_hash)
  expect_identical(restored$records[[1]]$observations$sha256, original_hash)
})

test_that('legacy result snapshots retain the explicit canonical bootstrap', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  versioned_test_ingest('store', versioned_test_rows())
  conalco_legacy_view('store', 'legacy_source',
    daily_root = CONALCO_DAILY_ROOT)
  conalco_attach_legacy_bridge('store', 'legacy_source')
  output = file.path(getwd(), 'legacy_archive')
  packed = with_dir('legacy_source', actions_state_pack(output))
  scope = new.env(parent = environment(actions_state_restore))
  scope$actions_state_list = \(dav) '2030-01-01.json'
  scope$actions_dav_read = \(dav, path, destination) {
    source = if (grepl('/commits/', path, fixed = T)) packed$commit_file else
      if (endsWith(path, '.json')) packed$sidecar else packed$archive
    file.copy(source, destination)
  }
  restore = actions_state_restore
  environment(restore) = scope
  dir.create('legacy_restored')
  restored = restore(NULL, 'legacy_restored')
  expect_silent(conalco_check_bootstrap(restored$versioned_store))
  expect_identical(dedup_restore_rows(restored$versioned_store,
    '2030-01-01', '2030-01-01'), versioned_test_rows())
  expect_identical(dedup_hash(file.path('store/commits/2030-01-01',
    'manifest.json')), dedup_hash(file.path(restored$versioned_store,
    'commits/2030-01-01/manifest.json')))
})
