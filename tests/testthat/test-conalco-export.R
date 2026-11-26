# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261029

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) OFFLINE PERIOD EXPORT WITHOUT CHANGING DAILY SOURCES
# <-----------------------------------------------------------------------------

for (module in c('catalog.R', 'derivation.R', 'price_analysis.R',
    'catalog_refresh.R', 'refresh_validation.R', 'snapshot_analysis.R',
    'conalco_daily.R'))
  source(file.path(APM_ROOT, 'R', module), local = F)
for (entry in parse(file.path(APM_ROOT,
    'tests/testthat/test-conalco-daily.R'))) {
  if (is.call(entry) && identical(entry[[1]], as.name('=')) &&
      as.character(entry[[2]]) %in% c('daily_test_root',
        'daily_fixture_products', 'daily_fixture_manifest')) eval(entry)
}

export_fixture = \() {
  for (date in c('2026-10-21', '2026-10-23', '2026-10-25')) {
    id = paste0('export_', gsub('-', '', date, fixed = T))
    manifest = daily_fixture_manifest(daily_fixture_products(id, date))
    append_conalco_daily(manifest, root = 'daily_source')
  }
  paths = list.files('daily_source', recursive = T, full.names = T)
  list(paths = paths, hashes = vapply(paths,
    \(path) digest(file = path, algo = 'sha256'), ''))
}

test_that('period bounds include both endpoints and preserve daily values', {
  local_dir(daily_test_root())
  before = export_fixture()
  receipt = export_conalco_period('2026-10-21', '2026-10-23',
    'period', 'daily_source')
  expect_equal(receipt$n_runs, 2L)
  expect_equal(receipt$n_observations, 16L)
  panel = daily_read_csv('period/panel.csv.gz')
  full = daily_read_csv('daily_source/panel.csv.gz')
  expected = full[full$daily_date <= '2026-10-23', ]
  rownames(expected) = NULL
  expect_identical(panel, expected)
  stats = daily_read_csv('daily_source/statistics.csv')
  stats = stats[stats$date <= '2026-10-23', ]
  rownames(stats) = NULL
  expect_identical(daily_read_csv('period/statistics.csv'), stats)
  expect_identical(vapply(before$paths,
    \(path) digest(file = path, algo = 'sha256'), ''), before$hashes)
  expect_identical(receipt$export_period$observed_dates,
    c('2026-10-21', '2026-10-23'))
  expect_length(receipt$source_run_manifests, 2L)
  expect_identical(receipt$panel$sha256,
    digest(file = receipt$panel$path, algo = 'sha256'))
  expect_identical(receipt$statistics$sha256,
    digest(file = receipt$statistics$path, algo = 'sha256'))
  expect_true(file.exists('period/manifest.json'))
})

test_that('a single day exports from relocated shards without a panel', {
  local_dir(daily_test_root())
  before = export_fixture()
  expect_true(file.rename('daily_source', 'restored'))
  unlink(file.path('restored', c('panel.csv.gz', 'statistics.csv',
    'manifest.json')))
  receipt = export_conalco_period('2026-10-23', '2026-10-23',
    'one_day', 'restored')
  expect_equal(receipt$n_runs, 1L)
  expect_equal(receipt$n_observations, 8L)
  expect_identical(unique(daily_read_csv('one_day/panel.csv.gz')$daily_date),
    '2026-10-23')
  paths = list.files('restored/runs', recursive = T, full.names = T)
  original = sub('^restored/', 'daily_source/', paths)
  expect_identical(unname(vapply(paths,
    \(path) digest(file = path, algo = 'sha256'), '')),
    unname(before$hashes[match(original, before$paths)]))
})

test_that('bad bounds, empty ranges and existing outputs cannot write', {
  local_dir(daily_test_root())
  before = export_fixture()
  for (bounds in list(c('2026-10-25', '2026-10-21'),
      c('2026-02-30', '2026-10-25'), c('2026-1-01', '2026-10-25'))) {
    expect_error(export_conalco_period(bounds[1], bounds[2],
      'invalid', 'daily_source'), 'YYYY-MM-DD')
  }
  expect_error(export_conalco_period('2026-10-22', '2026-10-22',
    'empty', 'daily_source'), 'No validated daily runs')
  expect_error(export_conalco_period('2026-10-21', '2026-10-25',
    'daily_source', 'daily_source'), 'new directory')
  expect_error(export_conalco_period('2026-10-21', '2026-10-25',
    'daily_source/export', 'daily_source'), 'outside the daily source')
  expect_identical(vapply(before$paths,
    \(path) digest(file = path, algo = 'sha256'), ''), before$hashes)
  expect_false(any(dir.exists(c('invalid', 'empty', 'daily_source/export'))))
})

test_that('shard checksums and duplicate days remain mandatory', {
  local_dir(daily_test_root())
  export_fixture()
  path = 'daily_source/runs/export_20261023/observations.csv.gz'
  original = readBin(path, 'raw', n = file.size(path))
  writeLines('damaged shard', path)
  expect_error(export_conalco_period('2026-10-21', '2026-10-25',
    'damaged', 'daily_source'), 'checksum mismatch')
  expect_false(dir.exists('damaged'))
  writeBin(original, path)
  path = 'daily_source/runs/export_20261023/manifest.json'
  record = read_json(path)
  record$date = '2026-10-21'
  write_json(record, path, auto_unbox = T)
  expect_error(export_conalco_period('2026-10-21', '2026-10-25',
    'duplicate', 'daily_source'), 'Duplicate committed Berlin dates')
  expect_false(dir.exists('duplicate'))
  expect_length(list.files('.', '^\\.conalco_period_', all.files = T), 0L)
})

test_that('GitHub publication requires an explicit publish request', {
  workflow = readLines(file.path(APM_ROOT,
    '.github/workflows/conalco_daily.yml'), warn = F)
  publish = match('  publish:', workflow)
  if (is.na(publish)) {
    expect_false(any(grepl('--publish-commit', workflow, fixed = T)))
  } else {
    condition = workflow[seq.int(publish, match('    runs-on: ubuntu-24.04',
      workflow[seq.int(publish, length(workflow))]) + publish - 1L)]
    expect_true(any(grepl("inputs.mode == 'publish'", condition, fixed = T)))
    expect_true(any(grepl("needs.full.result == 'success'", condition,
      fixed = T)))
    expect_false(any(grepl("github.event.repository.private == true",
      condition, fixed = T)))
  }
})
