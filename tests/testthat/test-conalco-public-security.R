# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261116

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) PUBLIC PAYLOADS AND PRIVATE HISTORICAL PACKAGE BYTES
# <-----------------------------------------------------------------------------

test_that('public publication rejects archive paths and embedded credentials', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  versioned_test_ingest('store', versioned_test_rows())
  output = tempfile('public_payload_')
  plan = conalco_prepare_publication('store', output, 'publish',
    'local_test', offline = T)
  expect_true(conalco_publication_security(output, plan$files$path))
  for (path in c('data/raw/archive.tar.gz', '.env',
      'data/conalco/versioned/commits/2030-01-01/extra.tar.gz'))
    expect_error(conalco_publication_security(output, path),
      'Unexpected public publication path')
  path = file.path(conalco_storage_config()$versioned_root,
    'commits/2030-01-01/observations.csv.gz')
  bytes = readBin(file.path(output, path), 'raw',
    file.size(file.path(output, path)))
  for (value in c(paste0('ghp_', strrep('A', 30)),
      paste0('sk-', strrep('c', 40)),
      paste0('session-', '=', strrep('b', 24)))) {
    connection = gzfile(file.path(output, path), 'wt')
    writeLines(value, connection)
    close(connection)
    expect_error(conalco_publication_security(output, path),
      'Sensitive content')
  }
  writeBin(bytes, file.path(output, path))
  expect_true(conalco_publication_security(output, plan$files$path))
})

test_that('historical package verification survives changed runtime code', {
  local_dir(versioned_test_workspace())
  conalco_init_versioned('store', initial_empty = T)
  versioned_test_ingest('store', versioned_test_rows())
  first = conalco_pack_increment('store', 'first', '2030-01-01')
  project = normalizePath('first/assembled')
  versioned_test_ingest('store', versioned_test_rows('2030-01-02'))
  write('# changed current runtime', 'R/storage_transport.R', append = T)
  expect_error(conalco_publish_increment('store', 'wrong',
    first$checkpoint, \(...) stop('Unexpected publication')),
    'Missing or damaged dependency')
  state = new.env(parent = emptyenv())
  state$writes = character()
  publish = \(source, path) {
    state$writes = c(state$writes, path)
    invisible(path)
  }
  second = conalco_publish_increment('store', 'second',
    first$checkpoint, publish, project = project)
  expect_identical(second$date, '2030-01-02')
  expect_true(endsWith(tail(state$writes, 1L),
    '/commits/2030-01-02.json'))
  write('damaged history', file.path(project, 'R/storage_transport.R'))
  expect_error(conalco_publish_increment('store', 'damaged',
    first$checkpoint, publish, project = project),
    'Missing or damaged dependency')
})

test_that('public commits reject other repositories before any Git command', {
  local_envvar(c(GITHUB_ACTIONS = 'true', GITHUB_REF = 'refs/heads/main',
    GITHUB_REPOSITORY = 'untrusted/fork', APM_PRIVATE_REPOSITORY = 'false'))
  local_options(apm.conalco.storage = list(github_publication = 'versioned'))
  expect_error(actions_commit_publication(\(...) stop('Git must not run')),
    'canonical repository')
})

test_that('schedule publication requires the canonical main context', {
  local_envvar(c(GITHUB_ACTIONS = 'true', GITHUB_REF = 'refs/heads/main',
    GITHUB_REPOSITORY = 'senyzizou/conalco-price-monitoring'))
  expect_true(conalco_manual_gate('publish', 'schedule'))
  local_envvar(GITHUB_REPOSITORY = 'untrusted/fork')
  expect_error(conalco_manual_gate('publish', 'schedule'), 'manual')
  workflow = readLines(file.path(APM_ROOT,
    '.github/workflows/conalco_daily.yml'))
  schedule = which(workflow == '  schedule:')
  expect_length(schedule, 1L)
  expect_identical(workflow[schedule + 1:2], c("    - cron: '0 9 * * *'",
    '      timezone: Europe/Berlin'))
  expect_equal(sum(grepl('cron:', workflow, fixed = T)), 1L)
})
