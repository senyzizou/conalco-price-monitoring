# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261026

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) SYNTHETIC WEBDAV; NO NETWORK OR CREDENTIAL ACCESS
# <-----------------------------------------------------------------------------

for (module in c('catalog.R', 'derivation.R', 'price_analysis.R',
    'catalog_refresh.R', 'refresh_validation.R', 'snapshot_analysis.R',
    'conalco_daily.R', 'conalco_actions.R'))
  source(file.path(APM_ROOT, 'R', module), local = F)
actions_fake_dav = \(failure = NULL) {
  state = new.env(parent = emptyenv())
  state$objects = list()
  state$calls = list()
  state$directories = character()
  state$dav = \(method, path, input = NULL, output = NULL,
    destination = NULL) {
    state$calls[[length(state$calls) + 1L]] = list(method, path)
    if (identical(failure, method)) stop('Injected WebDAV HTTP 507')
    if (method == 'MKCOL') {
      if (path %in% state$directories) return(405L)
      state$directories = c(state$directories, path)
      return(201L)
    }
    if (method == 'PROPFIND') {
      objects = as.character(names(state$objects))
      paths = objects[startsWith(objects, paste0(path, '/'))]
      writeLines(paste0('<d:multistatus xmlns:d="DAV:">',
        paste0('<d:response><d:href>/', paths,
          '</d:href></d:response>', collapse = ''), '</d:multistatus>'), output)
      return(207L)
    }
    if (method == 'GET') {
      bytes = state$objects[[path]]
      if (is.null(bytes)) return(404L)
      writeBin(bytes, output)
      return(200L)
    }
    if (method == 'PUT') {
      state$objects[[path]] = if (identical(failure, 'corrupt'))
        charToRaw('corrupt') else readBin(input, 'raw', file.size(input))
      return(201L)
    }
    if (method == 'MOVE') {
      if (!is.null(state$objects[[destination]])) return(412L)
      state$objects[[destination]] = state$objects[[path]]
      state$objects[[path]] = NULL
      return(201L)
    }
    if (method == 'DELETE') {
      state$objects[[path]] = NULL
      state$directories = setdiff(state$directories, path)
      return(204L)
    }
    stop('Unexpected fake method')
  }
  state
}

test_that('missing secrets name only the missing key', {
  get = \(name) if (name == 'UKEBOX_USERNAME') '' else 'synthetic-password'
  expect_error(actions_secrets(get), 'Missing required secret: UKEBOX_USERNAME')
  get = \(name) switch(name,
    UKEBOX_WEBDAV_URL = 'https://box.uke.de/remote.php/webdav/',
    UKEBOX_USERNAME = 'synthetic-user', UKEBOX_APP_PASSWORD = 'fake')
  expect_length(actions_secrets(get), 3L)
  expect_error(actions_secrets(\(name) 'http://unexpected'), 'endpoint')
})

test_that('phase timings preserve results and failures without contents', {
  output = capture.output({ value = actions_phase('test.phase', 42L) })
  expect_identical(value, 42L)
  expect_match(output[1], 'test.phase: start', fixed = T)
  expect_match(output[2], 'test.phase: ok [(][0-9.]+ s[)]')
  failure = simpleError('https://private.invalid/secret-content')
  output = capture.output({
    error = tryCatch(actions_phase('test.phase', stop(failure)),
      error = identity)
  })
  expect_identical(error, failure)
  expect_match(output[2], 'test.phase: failed [(][0-9.]+ s[)]')
  expect_false(any(grepl('private|secret-content', output)))
})

test_that('WebDAV keeps bounded requests and sanitized immediate failures', {
  state = new.env(parent = emptyenv())
  state$calls = 0L
  rlang::local_bindings(req_perform = \(req, path = NULL) {
    state$calls = state$calls + 1L
    state$options = req$options
    if (state$failure == 'http') return(response(status_code = 503L))
    if (state$failure == 'ok') return(response(status_code = 200L))
    parent = simpleError('private transport details')
    if (state$failure == 'timeout')
      class(parent) = c('curl_error_operation_timedout', class(parent))
    stop(structure(list(message = 'private-url private-password',
      call = NULL, parent = parent), class = c('httr2_failure',
        'error', 'condition')))
  }, .env = environment(actions_dav))
  dav = actions_dav(list('https://private.invalid', 'user', 'password'))
  errors = c(timeout = 'WebDAV request timed out',
    transport = 'WebDAV transport failure', http = 'WebDAV HTTP 503')
  for (defect in names(errors)) {
    state$failure = defect
    before = state$calls
    output = capture.output({
      error = tryCatch(dav('GET', 'private-object'), error = identity)
    })
    expect_match(conditionMessage(error), errors[[defect]], fixed = T)
    expect_identical(state$calls, before + 1L)
    expect_match(tail(output, 1L), 'WebDAV GET: failed', fixed = T)
    expect_false(any(grepl('private|password',
      c(output, conditionMessage(error)))))
    expect_equal(state$options$timeout_ms, 300000)
    expect_equal(state$options$connecttimeout, 30)
    expect_false(state$options$followlocation)
  }
  state$failure = 'ok'
  output = capture.output({ status = dav('GET', 'private-object') })
  expect_identical(status, 200L)
  expect_match(tail(output, 1L), 'WebDAV GET: ok', fixed = T)
  target = paste0('APM/conalco/raw/2026/11/',
    'catalog_refresh_20261126T080000_000000000000.tar.gz.pending-local-1-1')
  invisible(capture.output(dav('PUT', target)))
  expect_equal(state$options$timeout_ms, 900000)
  invisible(capture.output(dav('GET', target)))
  expect_equal(state$options$timeout_ms, 300000)
  sidecar = sub('.pending-', '.json.pending-', target, fixed = T)
  invisible(capture.output(dav('PUT', sidecar)))
  expect_equal(state$options$timeout_ms, 300000)
  result = sub('/raw/2026/11/', '/versioned/v1/objects/', target, fixed = T)
  invisible(capture.output(dav('PUT', result)))
  expect_equal(state$options$timeout_ms, 300000)
})

test_that('upload verifies temporary and final objects and is idempotent', {
  local = tempfile()
  writeBin(charToRaw('synthetic archive'), local)
  state = actions_fake_dav()
  target = 'APM/conalco/raw/2026/10/run.tar.gz'
  expect_identical(actions_dav_publish(state$dav, local, target), 'verified')
  expect_identical(state$objects[[target]], charToRaw('synthetic archive'))
  expect_length(state$objects, 1L)
  before = length(state$calls)
  expect_identical(actions_dav_publish(state$dav, local, target), 'identical')
  methods = vapply(state$calls[-seq_len(before)], `[[`, '', 1L)
  expect_identical(methods, c('PROPFIND', 'GET'))
  writeBin(charToRaw('different archive'), local)
  expect_error(actions_dav_publish(state$dav, local, target), 'Conflicting')
  expect_identical(state$objects[[target]], charToRaw('synthetic archive'))
})

test_that('failed PUT, corrupt reads and failed MOVE cannot publish', {
  local = tempfile()
  writeBin(charToRaw('synthetic archive'), local)
  for (failure in c('PUT', 'corrupt', 'MOVE')) {
    state = actions_fake_dav(failure)
    expect_error(actions_dav_publish(state$dav, local, 'raw/run.tar.gz'),
      '507|SHA-256')
    expect_null(state$objects[['raw/run.tar.gz']])
    expect_length(state$objects, if (failure == 'PUT') 0L else 1L)
    expect_false(any(vapply(state$calls, \(x) x[[1]] == 'DELETE', F)))
  }
  state = actions_fake_dav()
  expect_true(actions_connection_test(state$dav))
  expect_length(state$objects, 0L)
})

test_that('day guard follows Berlin dates including summer and winter', {
  stamps = c('2026-07-01T22:30:00Z', '2026-10-25T23:30:00Z')
  expect_identical(berlin_date(stamps), c('2026-07-02', '2026-10-26'))
  records = list(list(date = '2026-10-26'))
  expect_true(actions_day_exists(records, '2026-10-26'))
  expect_false(actions_day_exists(records, '2026-10-27'))
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) ORIGINAL ARCHIVE GUARDS AND COMPLETE SYNTHETIC CATALOGUE
# <-----------------------------------------------------------------------------

# Reuse the existing validated daily fixture builders without executing tests.
fixture_names = c('daily_fixture_products', 'daily_fixture_manifest',
  'daily_test_root', 'terms_test_page', 'terms_test_archive',
  'terms_test_config')
for (expression in parse(file.path(APM_ROOT,
    'tests/testthat/test-conalco-daily.R'))) {
  if (is.call(expression) && identical(expression[[1]], as.name('=')) &&
      as.character(expression[[2]]) %in% fixture_names)
    eval(expression)
}

actions_test_archive = \(id = 'synthetic_actions', date = '2026-10-22',
  archive = T, snapshot = F) {
  manifest = daily_fixture_manifest(daily_fixture_products(id, date))
  body = file.path(run_dir(id), '0001_category.body')
  metadata = file.path(run_dir(id), '0001_category.json')
  writeBin(charToRaw('synthetic original response'), body)
  write_json(list(run_id = id, raw_file = body,
      raw_sha256 = archive_sha256(body)), metadata, auto_unbox = T)
  inventory = file.path(dirname(manifest), 'raw_inventory.csv')
  write.csv(data.frame(metadata_file = metadata, raw_file = body,
      raw_sha256 = archive_sha256(body)), inventory, row.names = F)
  record = read_json(manifest)
  record$proof$raw_inventory = refresh_file_record(inventory)
  write_json(record, manifest, auto_unbox = T, pretty = T)
  append_conalco_daily(manifest)
  if (!archive) return(manifest)
  if (snapshot) snapshot_source(ensure_dir(file.path(run_dir(id),
    'collection_source')))
  receipt = daily_archive_run(manifest, 'backups')
  list(archive = receipt$archive$path, receipt = receipt)
}

test_that('R and system tar preserve gzip/xz, ustar paths and block edges', {
  local_dir(ensure_dir(tempfile('tar_portability_')))
  paths = c('payload/empty.body', 'payload/511.body', 'payload/512.body',
    'payload/513.body', paste0('payload/', strrep('a', 70), '/',
      strrep('b', 50), '/long_path.body'))
  sizes = c(0L, 511L, 512L, 513L, 17L)
  for (i in seq_along(paths)) {
    ensure_dir(dirname(paths[i]))
    writeBin(as.raw(rep(0:255, length.out = sizes[i])), paths[i])
  }
  writers = c('internal', unname(Sys.which('tar')), unname(Sys.which('gtar')))
  writers = unique(writers[nzchar(writers)])
  expect_true(length(writers) >= 2L)
  for (writer in writers) {
    # Production uses system ustar for long paths; R warns above 100 bytes.
    selected = if (writer == 'internal') paths[1:4] else paths
    inventory = lapply(selected, refresh_file_record)
    formats = if (writer == 'internal') c('gzip', 'xz') else 'gzip'
    for (compression in formats) {
      suffix = if (compression == 'gzip') '.tar.gz' else '.tar.xz'
      archive = tempfile('portable_', tmpdir = getwd(), fileext = suffix)
      flags = if (writer == 'internal') '' else '--format=ustar'
      tar(archive, files = selected, compression = compression, tar = writer,
        extra_flags = flags)
      expected = if (compression == 'gzip') as.raw(c(31, 139)) else
        as.raw(c(253, 55, 122, 88, 90, 0))
      expect_identical(readBin(archive, 'raw', length(expected)), expected)
      connection = if (compression == 'gzip') gzfile(archive, 'rb') else
        xzfile(archive, 'rb')
      header = readBin(connection, 'raw', 512L)
      close(connection)
      expect_identical(header[258:265],
        c(charToRaw('ustar'), as.raw(0), charToRaw('00')))
      expect_identical(archive_members(archive, 'payload'), selected)
      expect_identical(untar(archive, list = T, tar = 'internal'), selected)
      target = ensure_dir(tempfile('portable_restore_'))
      receipt = list(archive = refresh_file_record(archive), files = inventory)
      expect_setequal(actions_restore(archive, receipt, target), selected)
      for (entry in inventory) expect_identical(
        archive_sha256(file.path(target, entry$path)), entry$sha256)
    }
  }
})

test_that(
  'the production archive function accepts an explicit file inventory', {
  expect_true('paths' %in% names(formals(daily_archive_run)))
  local_dir(daily_test_root())
  fixture = actions_test_archive()
  manifest = file.path(run_dir('synthetic_actions'), 'manifest.json')
  paths = vapply(fixture$receipt$files, `[[`, '', 'path')
  expect_identical(daily_archive_run(manifest, 'backups', paths = paths),
    fixture$receipt)
})

test_that('complete archives restore byte-identically and reject conflicts', {
  local_dir(daily_test_root())
  fixture = actions_test_archive()
  expect_identical(actions_verify_archive(fixture$archive,
      fixture$receipt)$run_id, 'synthetic_actions')
  target = tempfile()
  dir.create(target)
  restored = actions_restore(fixture$archive, fixture$receipt, target)
  expect_setequal(restored, vapply(fixture$receipt$files, `[[`, '', 'path'))
  expect_silent(actions_restore(fixture$archive, fixture$receipt, target))
  writeLines('conflict', file.path(target, restored[1]))
  expect_error(actions_restore(fixture$archive, fixture$receipt, target),
    'Conflicting restore')
  bad = fixture$receipt
  bad$archive$sha256 = paste(rep('0', 64), collapse = '')
  expect_error(actions_restore(fixture$archive, bad, target), 'SHA-256')
  bad$files[[1]]$path = '../escape'
  expect_error(actions_restore(fixture$archive, bad, target), 'Unsafe')
  bad = fixture$receipt
  bad$run_id = 'wrong_run'
  expect_error(actions_verify_archive(fixture$archive, bad), 'BLOCKED')
})

test_that('full orchestration restores state and commits only after upload', {
  local_dir(daily_test_root())
  actions_test_archive('historical', '2026-10-21')
  history = actions_history()
  ensure_dir('config')
  write_json(list(timezone = 'Europe/Berlin'), 'config/conalco_actions.json')
  writeLines('synthetic reference', 'config/conalco_actions_reference.tar.gz')
  writeLines('{}', 'config/conalco_actions_reference.tar.gz.json')
  writeLines('{}', 'renv.lock')
  state = actions_fake_dav()
  actions_state_publish(state$dav, history, ensure_dir('seed'))
  scope = new.env(parent = environment(actions_full_run))
  scope$actions_preflight = \(...) invisible(NULL)
  scope$actions_reference = \(target) invisible(T)
  scope$actions_dav = \() state$dav
  scope$run_conalco_daily = \(manual, config_file, finalize) {
    path = actions_test_archive('new_day',
      format(Sys.time(), '%Y-%m-%d', tz = 'Europe/Berlin'), archive = F)
    directory = run_dir('new_day')
    write_json(list(terms_metadata = file.path(directory,
        '0001_category.json')), file.path(directory,
        'access_review_pages.json'), auto_unbox = T)
    writeLines('checkpoint', file.path(directory, 'checkpoint.rds'))
    finalize(path, read_json(config_file)$backup_directory)
    'new_day'
  }
  full = actions_full_run
  environment(full) = scope
  local_envvar(c(GITHUB_ACTIONS = 'true', GITHUB_REF = 'refs/heads/main'))
  state$fail_once = T
  scope$actions_dav = \() \(method, path, ...) {
    if (state$fail_once && method == 'PUT' && grepl('/raw/', path)) {
      state$fail_once = F
      stop('Injected WebDAV HTTP 507')
    }
    state$dav(method, path, ...)
  }
  expect_error(full(), '507')
  expect_false(file.exists('tmp/actions_run_id.txt'))
  expect_length(list.files('tmp/actions_recovery'), 2L)
  scope$run_conalco_daily = \(...) stop('Recovery attempted scraping')
  full(recovery = T)
  expect_true(file.exists('tmp/actions_run_id.txt'))
  expect_false(file.exists('tmp/actions_publish_files.txt'))
  expect_length(daily_run_records(CONALCO_DAILY_ROOT), 2L)
  actions_history_unchanged(history)
  target = ensure_dir(tempfile('fresh_runner_'))
  restored = actions_state_restore(state$dav, target)
  expect_length(restored$records, 2L)
  actions_retain_attempt(state$dav)
  attempt = list.files('tmp/actions_attempt', '[.]tar[.]gz$', full.names = T)
  retained = untar(attempt, list = T, tar = 'internal')
  expect_true(any(endsWith(retained, 'checkpoint.rds')))
  expect_false(any(endsWith(retained, '.tar.gz')))
  scope$run_conalco_daily = \(...) stop('Repeated day attempted scraping')
  expect_null(full())
  archive = list.files('tmp/actions_recovery', '[.]tar[.]gz$', full.names = T)
  expect_false(any(grepl('checkpoint|parsed_pages',
    untar(archive, list = T, tar = 'internal'))))
})

test_that('partial catalogues cannot be archived or published', {
  local_dir(daily_test_root())
  path = daily_fixture_manifest(daily_fixture_products('partial'))
  manifest = read_json(path)
  manifest$status = 'partial'
  write_json(manifest, path, auto_unbox = T)
  expect_error(daily_archive_run(path, 'backups'), 'not complete')
  expect_error(append_conalco_daily(path), 'not complete')
  expect_false(dir.exists('backups'))
  expect_false(file.exists('tmp/actions_publish_files.txt'))
})

test_that('durable metadata appears only after both verified uploads', {
  local_dir(daily_test_root())
  fixture = actions_test_archive()
  state = actions_fake_dav('PUT')
  expect_error(actions_upload_archive(fixture$archive, fixture$receipt,
      '2026-10-22', state$dav), '507')
  record_path = file.path(CONALCO_DAILY_ROOT,
    'runs/synthetic_actions/manifest.json')
  expect_null(read_json(record_path)$raw_archive)
  state = actions_fake_dav()
  durable = actions_upload_archive(fixture$archive, fixture$receipt,
    '2026-10-22', state$dav)
  expect_identical(durable$sha256, fixture$receipt$archive$sha256)
  expect_length(state$objects, 2L)
  actions_publish_receipt('synthetic_actions', durable)
  expect_identical(read_json(record_path)$raw_archive$sha256, durable$sha256)
  expect_silent(actions_upload_archive(fixture$archive, fixture$receipt,
      '2026-10-22', state$dav))
})

test_that('workflow preserves scheduling and isolates publication rights', {
  text = readLines(file.path(APM_ROOT, '.github/workflows/conalco_daily.yml'))
  expect_true(any(grepl('workflow_dispatch:', text, fixed = T)))
  expect_false(any(grepl('^  (push|pull_request):', text)))
  expect_true(any(grepl('default: preflight', text, fixed = T)))
  expect_true(any(grepl('cancel-in-progress: false', text, fixed = T)))
  expect_false(any(grepl('git (push|commit|config|add|fetch)', text)))
  expect_false(any(grepl('contents: write|^  publish:', text)))
  expect_false(any(grepl('upload-artifact|APM_STATE_BASE:', text)))
  expect_equal(sum(grepl('environment: conalco-ukebox', text)), 2L)
})

test_that('daily fast tests and extended startup checks both remain required', {
  workflow = readLines(file.path(APM_ROOT,
    '.github/workflows/conalco_daily.yml'))
  full = match('  full:', workflow)
  preflight = workflow[match('  preflight:', workflow):(full - 1L)]
  daily = workflow[full:length(workflow)]
  expect_true(any(grepl('--tests-fast', preflight, fixed = T)))
  expect_true(any(grepl('--tests-extended', preflight, fixed = T)))
  expect_true(any(grepl('timeout-minutes: 10', preflight, fixed = T)))
  check_preflight = \(text) {
    start = which(text == '  preflight:')
    if (length(start) != 1L) stop('Missing or ambiguous preflight job')
    jobs = grep('^  [a-zA-Z_][a-zA-Z0-9_-]*:', text)
    end = min(c(jobs[jobs > start], length(text) + 1L)) - 1L
    job = text[start:end]
    job_limit = grep('^    timeout-minutes: [0-9]+$', job, value = T)
    if (length(job_limit) != 1L) stop('Ambiguous preflight job timeout')
    job_limit = as.integer(sub('.*: ', '', job_limit))
    if (job_limit != 140L) stop('Preflight job timeout must be 140')
    start = which(job == paste0('      - name: ',
      'Offline pipeline and harmless WebDAV check'))
    if (length(start) != 1L) stop('Missing or ambiguous WebDAV step')
    steps = grep('^      - ', job)
    end = min(c(steps[steps > start], length(job) + 1L)) - 1L
    step_limit = grep('^        timeout-minutes: [0-9]+$',
      job[start:end], value = T)
    if (length(step_limit) != 1L) stop('Ambiguous WebDAV step timeout')
    step_limit = as.integer(sub('.*: ', '', step_limit))
    if (step_limit != 60L) stop('WebDAV step timeout must be 60')
    budgets = grep('^        timeout-minutes: [0-9]+$', job, value = T)
    if (job_limit <= sum(as.integer(sub('.*: ', '', budgets))))
      stop('Preflight job budget must exceed explicit step budgets')
    invisible(T)
  }
  expect_silent(check_preflight(workflow))
  # Mutate only a copy; other jobs cannot supply the required timeout values.
  job_start = match('  preflight:', workflow)
  job_rows = seq.int(job_start, full - 1L)
  job_timeout = job_rows[workflow[job_rows] == '    timeout-minutes: 140']
  webdav = match(paste0('      - name: ',
    'Offline pipeline and harmless WebDAV check'), workflow)
  wrong_job = workflow
  wrong_job[job_timeout] = '    timeout-minutes: 110'
  wrong_job[match('    timeout-minutes: 360', workflow)] =
    '    timeout-minutes: 90'
  expect_error(check_preflight(wrong_job), 'job timeout must be 140')
  wrong_step = workflow
  wrong_step[webdav + 1L] = '        timeout-minutes: 5'
  setup = match('      - name: Install R and restore locked packages',
    workflow)
  wrong_step[setup + 1L] = '        timeout-minutes: 30'
  expect_error(check_preflight(wrong_step), 'step timeout must be 60')
  insufficient = workflow
  insufficient[setup + 1L] = '        timeout-minutes: 40'
  expect_error(check_preflight(insufficient), 'exceed explicit step budgets')
  missing_step = workflow[-(webdav + 1L)]
  expect_error(check_preflight(missing_step), 'Ambiguous WebDAV step timeout')
  expect_true(any(grepl('--tests-fast', daily, fixed = T)))
  expect_false(any(grepl('--tests-extended', daily, fixed = T)))
  for (job in list(preflight, daily)) {
    step = match('      - name: Targeted offline tests', job)
    expect_identical(job[step + 1L], '        timeout-minutes: 30')
  }
  full_limit = as.integer(sub('.*: ', '',
    grep('^    timeout-minutes: ', daily, value = T)))
  step_limits = as.integer(sub('.*: ', '',
    grep('^        timeout-minutes: ', daily, value = T)))
  expect_gt(full_limit, sum(step_limits))
  expect_false(any(grepl('recovery_kind|--resume-attempt', workflow)))
  script = readLines(file.path(APM_ROOT, 'scripts/conalco_actions.R'))
  expect_true(any(grepl('pagination', script, fixed = T)))
  expect_true(any(grepl('reference', script, fixed = T)))
  expect_true(any(grepl("'^conalco-startup$'", script, fixed = T)))
})

test_that('offline reproduction binds the published archive and frozen code', {
  local_dir(daily_test_root())
  fixture = actions_test_archive(snapshot = T)
  ensure_dir('renv')
  writeLines('# synthetic offline activation', 'renv/activate.R')
  durable = list(sha256 = fixture$receipt$archive$sha256,
    manifest_sha256 = archive_sha256(paste0(fixture$archive, '.json')))
  actions_publish_receipt('synthetic_actions', durable)
  scope = new.env(parent = environment(actions_reproduce))
  state = new.env()
  state$calls = list()
  scope$release_rscript = \(...) {
    state$calls[[length(state$calls) + 1L]] = c(...)
    expect_true(file.exists('R/conalco_daily.R'))
  }
  reproduce = actions_reproduce
  environment(reproduce) = scope
  expect_true(reproduce(fixture$archive))
  expect_identical(state$calls[[1]],
    c('--vanilla', 'scripts/run_conalco_daily.R', '--rebuild'))
  expect_identical(state$calls[[2]], c('--vanilla', 'scripts/verify_daily.R',
    '--run-id=synthetic_actions', '--sources'))
  durable$sha256 = paste(rep('0', 64), collapse = '')
  actions_publish_receipt('synthetic_actions', durable)
  expect_error(reproduce(fixture$archive), 'published immutable source')
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) DURABLE STATE, FAILED COMMITS AND SHARED WRITER LOCK
# <-----------------------------------------------------------------------------

test_that('state loss or corrupt downloads never become empty history', {
  local_dir(daily_test_root())
  actions_test_archive('history', '2026-10-21')
  history = actions_history()
  state = actions_fake_dav()
  target = ensure_dir(tempfile('restore_'))
  expect_error(actions_state_restore(state$dav, target), 'No committed state')
  actions_state_publish(state$dav, history, 'seed')
  expect_length(actions_state_restore(state$dav, target)$records, 1L)
  object = names(state$objects)[endsWith(names(state$objects), '.tar.gz')][1]
  state$objects[[object]] = charToRaw('corrupt')
  fresh = ensure_dir(tempfile('corrupt_restore_'))
  expect_error(actions_state_restore(state$dav, fresh), 'checksum mismatch')
  expect_false(dir.exists(file.path(fresh, CONALCO_DAILY_ROOT)))
})

test_that('failed state upload retains the previous complete commit', {
  local_dir(daily_test_root())
  actions_test_archive('history', '2026-10-21')
  history = actions_history()
  state = actions_fake_dav()
  actions_state_publish(state$dav, history, 'seed')
  previous = state$objects
  actions_test_archive('next_day', '2026-10-22')
  failing = \(method, path, ...) {
    if (method == 'MOVE' && grepl('/commits/', path)) stop('Injected 507')
    state$dav(method, path, ...)
  }
  expect_error(actions_state_publish(failing, history, 'next'), '507')
  commits = names(previous)[grepl('/commits/', names(previous))]
  expect_identical(state$objects[commits], previous[commits])
  fresh = ensure_dir(tempfile('after_failure_'))
  expect_length(actions_state_restore(state$dav, fresh)$records, 1L)
  actions_history_unchanged(history)
})

test_that('remote lock refuses another writer until explicit recovery', {
  state = actions_fake_dav()
  lock = actions_state_lock(state$dav)
  expect_error(actions_state_lock(state$dav), 'writer lock exists')
  state$dav('DELETE', lock)
  expect_identical(actions_state_lock(state$dav), lock)
})

test_that('competing observations cannot overwrite a committed date', {
  local_dir(daily_test_root())
  actions_test_archive('first', '2026-10-21')
  history = actions_history()
  state = actions_fake_dav()
  actions_state_publish(state$dav, history, 'seed')
  before = state$objects
  # A different archive on the same date must conflict at the immutable commit.
  packed = actions_state_pack('conflict')
  writeLines('conflicting date', packed$commit_file)
  expect_error(actions_dav_publish(state$dav, packed$commit_file,
    paste0(actions_state_base(), '/commits/', basename(packed$commit_file))),
    'Conflicting remote')
  expect_identical(state$objects, before)
})


test_that('technical retention keeps partial checkpoints without panel views', {
  local_dir(daily_test_root())
  ensure_dir('logs/conalco_daily')
  ensure_dir('data/raw/conalco/partial')
  writeLines('failed', 'logs/conalco_daily/status.json')
  writeLines('checkpoint', 'data/raw/conalco/partial/checkpoint.rds')
  state = actions_fake_dav()
  remote = actions_retain_attempt(state$dav)
  expect_true(remote %in% names(state$objects))
  archive = list.files('tmp/actions_attempt', '[.]tar[.]gz$', full.names = T)
  paths = untar(archive, list = T, tar = 'internal')
  expect_true('data/raw/conalco/partial/checkpoint.rds' %in% paths)
  expect_false(any(grepl('panel.csv|statistics.csv|actions_config', paths)))
  expect_false(any(grepl('/commits/', names(state$objects))))
})

test_that('seed mapping preserves original observations and manifests', {
  local_dir(daily_test_root())
  actions_test_archive('seed_day', '2026-10-21')
  history = actions_history()
  packed = actions_prepare_seed(output = 'seed_export')
  actions_history_unchanged(history)
  expect_identical(packed$commit$daily_root, 'data/conalco/daily')
  scratch = ensure_dir(tempfile('seed_mapped_'))
  actions_restore(packed$archive, read_json(packed$sidecar), scratch)
  source = history$records[[1]]$observations
  target = file.path(scratch, 'data/conalco/daily/runs/seed_day',
    'observations.csv.gz')
  expect_identical(archive_sha256(target), source$sha256)
})

test_that('deployment guard rejects missing or changed transferred files', {
  local_dir(ensure_dir(tempfile('deployment_')))
  ensure_dir('config')
  writeLines('expected adapter', 'adapter.R')
  write_json(list(files = list(refresh_file_record('adapter.R'))),
    'config/conalco_actions_files.json', auto_unbox = T)
  expect_true(actions_check_deployment())
  writeLines('old adapter', 'adapter.R')
  expect_error(actions_check_deployment(), 'outdated Actions deployment')
  unlink('adapter.R')
  expect_error(actions_check_deployment(), 'Missing or outdated')
  write_json(list(files = list(list(path = '../escape', sha256 = 'invalid'))),
    'config/conalco_actions_files.json', auto_unbox = T)
  expect_error(actions_check_deployment(), 'Unsafe')
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) CLOUD INITIALIZATION WITH AN UNUSABLE HISTORICAL CHECKOUT
# <-----------------------------------------------------------------------------

actions_test_cloud_checkout = \(date = '2026-10-21') {
  actions_test_archive('remote_history', date, archive = F)
  state = actions_fake_dav()
  actions_state_publish(state$dav, actions_history(), 'seed')
  terms = terms_test_archive('technical_reference', terms_test_page())
  config = terms_test_config(terms)
  config$timezone = 'Europe/Berlin'
  ensure_dir('config')
  write_json(config, 'config/conalco_actions.json', auto_unbox = T)
  reference = 'config/conalco_actions_reference.tar.gz'
  paths = c(terms$metadata_file, terms$raw_file)
  tar(reference, files = paths, compression = 'gzip', tar = Sys.which('tar'),
    extra_flags = '--format=ustar')
  write_json(list(archive = refresh_file_record(reference),
      files = lapply(paths, refresh_file_record)), paste0(reference, '.json'),
    auto_unbox = T)
  # Reproduce the checkout: real files exist, but manifests name Mac paths.
  paths = list.files(CONALCO_DAILY_ROOT, '^manifest[.]json$',
    recursive = T, full.names = T)
  for (path in paths) {
    record = read_json(path)
    for (name in c('panel', 'statistics', 'observations')) {
      if (!is.null(record[[name]])) record[[name]]$path = sub(
        CONALCO_DAILY_ROOT, 'data/processed/exports/conalco_daily',
        record[[name]]$path, fixed = T)
    }
    write_json(record, path, auto_unbox = T, pretty = T)
  }
  paths = sort(list.files(CONALCO_DAILY_ROOT, recursive = T, full.names = T))
  state$calls = list()
  list(state = state, checkout = lapply(paths, refresh_file_record))
}

actions_test_bad_cloud = \(state, defect) {
  commit_path = paste0(actions_state_base(), '/commits/2026-10-21.json')
  commit = fromJSON(rawToChar(state$objects[[commit_path]]),
    simplifyVector = F)
  if (defect == 'missing_seed') {
    state$objects = setNames(list(), character())
  } else if (defect == 'missing_archive') {
    state$objects[[commit$archive]] = NULL
  } else if (defect == 'missing_sidecar') {
    state$objects[[paste0(commit$archive, '.json')]] = NULL
  } else if (defect == 'wrong_sha256') {
    state$objects[[commit$archive]] = charToRaw('damaged archive')
  } else if (defect != 'missing_commit') {
    # Recalculate transport hashes so the actual panel validator is exercised.
    target = ensure_dir(tempfile('invalid_state_'))
    previous = getwd()
    on.exit({
      setwd(previous)
      unlink(target, recursive = T)
    }, add = T)
    receipt = read_json(paste0(list.files('seed', '[.]tar[.]gz$',
          full.names = T), '.json'))
    archive = receipt$archive$path
    actions_restore(archive, receipt, target)
    setwd(target)
    path = file.path(CONALCO_DAILY_ROOT, 'manifest.json')
    manifest = read_json(path)
    if (defect == 'incomplete_panel') unlink(manifest$panel$path)
    if (defect == 'analysis_version') {
      manifest$analysis_version = 'unapproved_version'
      write_json(manifest, path, auto_unbox = T)
    }
    if (defect == 'conflicting_run_ids') {
      path = file.path(CONALCO_DAILY_ROOT, 'runs/remote_history/manifest.json')
      record = read_json(path)
      record$run_id = 'conflicting_history'
      write_json(record, path, auto_unbox = T)
    }
    if (defect == 'inconsistent_panel') {
      panel = daily_read_csv(manifest$panel$path)
      daily_write_gzip(panel[-1L, ], manifest$panel$path)
      manifest$panel = refresh_file_record(manifest$panel$path)
      write_json(manifest, path, auto_unbox = T)
    }
    paths = sort(list.files(CONALCO_DAILY_ROOT, recursive = T, full.names = T))
    archive = file.path(target, 'invalid.tar.gz')
    tar(archive, files = paths, compression = 'gzip', tar = Sys.which('tar'),
      extra_flags = '--format=ustar')
    receipt = list(archive = refresh_file_record(archive),
      files = lapply(paths, refresh_file_record))
    sidecar = paste0(archive, '.json')
    write_json(receipt, sidecar, auto_unbox = T)
    commit$sha256 = archive_sha256(archive)
    commit$archive = paste0(actions_state_base(), '/objects/', commit$sha256,
      '.tar.gz')
    commit$receipt_sha256 = archive_sha256(sidecar)
    state$objects[[commit$archive]] =
      readBin(archive, 'raw', file.size(archive))
    state$objects[[paste0(commit$archive, '.json')]] =
      readBin(sidecar, 'raw', file.size(sidecar))
    state$objects[[commit_path]] = charToRaw(toJSON(commit, auto_unbox = T))
  }
  # A listed commit may disappear between PROPFIND and GET.
  \(method, path, ...) {
    if (defect == 'missing_commit' && method == 'GET' && path == commit_path)
      return(404L)
    state$dav(method, path, ...)
  }
}

test_that('online preflight validates remote state before checkout history', {
  local_dir(daily_test_root())
  rlang::local_bindings(CONALCO_DAILY_ROOT = 'data/conalco/daily',
    .env = environment(actions_preflight))
  cloud = actions_test_cloud_checkout()
  state = cloud$state
  original_check = private_check_panel
  state$checked_roots = character()
  rlang::local_bindings(actions_dav = \() state$dav,
    private_check_panel = \(root = getwd(), check_rows = T) {
      state$checked_roots = c(state$checked_roots, normalizePath(root))
      original_check(root, check_rows)
    }, .env = environment(actions_preflight))
  expect_false(dir.exists('data/processed/exports/conalco_daily'))
  output = capture.output({ result = actions_preflight(online = T) })
  for (phase in c('preflight.reference', 'preflight.connection',
      'preflight.state_restore')) {
    expect_true(any(grepl(paste0(phase, ': start'), output, fixed = T)))
    expect_true(any(grepl(paste0(phase, ': ok'), output, fixed = T)))
  }
  expect_identical(result$records[[1]]$run_id, 'remote_history')
  expect_length(state$checked_roots, 1L)
  expect_false(getwd() %in% state$checked_roots)
  expect_identical(lapply(vapply(cloud$checkout, `[[`, '', 'path'),
    refresh_file_record), cloud$checkout)
  expect_true(any(vapply(state$calls, \(call)
    call[[1]] == 'PROPFIND', logical(1))))
})

test_that('failed connection stops preflight before state restoration', {
  local_dir(daily_test_root())
  cloud = actions_test_cloud_checkout()
  state = cloud$state
  state$restore_calls = 0L
  rlang::local_bindings(actions_dav = \() state$dav,
    actions_connection_test = \(dav) stop('Injected connection failure'),
    actions_state_restore = \(...) {
      state$restore_calls = state$restore_calls + 1L
      stop('Unexpected state restore')
    }, .env = environment(actions_preflight))
  output = capture.output(expect_error(actions_preflight(online = T),
    'Injected connection failure'))
  expect_identical(state$restore_calls, 0L)
  expect_true(any(grepl('preflight.connection: failed', output, fixed = T)))
  expect_false(any(grepl('preflight.state_restore: start', output, fixed = T)))
  expect_false(any(grepl('Preflight passed', output, fixed = T)))
})

test_that('incremental restore timings retain validation and read-only I/O', {
  local_dir(versioned_test_workspace())
  local_options(apm.conalco.storage = list(ukebox_results = 'incremental'))
  conalco_init_versioned('store', initial_empty = T)
  versioned_test_ingest('store', versioned_test_rows())
  state = actions_fake_dav()
  commit = conalco_publish_increment('store', 'seed', NULL,
    \(source, path) {
      state$objects[[path]] = readBin(source, 'raw', file.size(source))
    })
  before = state$objects
  dir.create('checkout')
  output = capture.output({
    restored = actions_state_restore(state$dav, 'checkout')
  })
  expect_identical(restored$records[[1]]$date, '2030-01-01')
  for (phase in c('state.list_commits', 'state.read_commit',
      'state.increment_download_validate', 'state.legacy_view_validate',
      'state.copy_validated_view'))
    expect_true(any(grepl(paste0(phase, ': ok'), output, fixed = T)))
  expect_identical(state$objects, before)
  expect_true(all(vapply(state$calls, \(call)
    call[[1]] %in% c('GET', 'PROPFIND'), logical(1))))
  state$objects[[commit$objects[[1]]$path]] = charToRaw('damaged archive')
  dir.create('rejected')
  output = capture.output(expect_error(actions_state_restore(state$dav,
    'rejected')))
  expect_true(any(grepl('state.increment_download_validate: failed', output,
    fixed = T)))
  expect_false(dir.exists(file.path('rejected', CONALCO_DAILY_ROOT)))
  expect_false(dir.exists('rejected/versioned_restore'))
})

for (present_today in c(F, T)) {
  test_that(paste('full entry restores an invalid checkout; today =',
      present_today), {
    local_dir(daily_test_root())
    rlang::local_bindings(CONALCO_DAILY_ROOT = 'data/conalco/daily',
      .env = environment(actions_full_run))
    date = if (present_today) berlin_date(Sys.time()) else '2026-10-21'
    cloud = actions_test_cloud_checkout(date)
    state = cloud$state
    state$collector_calls = 0L
    local_envvar(c(GITHUB_ACTIONS = 'true', GITHUB_REF = 'refs/heads/main'))
    rlang::local_bindings(actions_dav = \() state$dav,
      run_conalco_daily = \(...) {
        state$collector_calls = state$collector_calls + 1L
        stop('Synthetic collector entry reached')
      }, .env = environment(actions_full_run))
    if (present_today) {
      expect_null(actions_full_run())
      expect_identical(state$collector_calls, 0L)
    } else {
      expect_error(actions_full_run(), 'Synthetic collector entry reached')
      expect_identical(state$collector_calls, 1L)
    }
    expect_identical(actions_history()$records[[1]]$run_id, 'remote_history')
    expect_false(paste0(actions_state_base(), '/writer_lock') %in%
      state$directories)
  })
}

failures = c(missing_seed = 'No committed state',
  missing_commit = 'State commit disappeared',
  missing_archive = 'Durable state checksum mismatch',
  missing_sidecar = 'Durable state checksum mismatch',
  wrong_sha256 = 'Durable state checksum mismatch',
  incomplete_panel = 'Missing release file',
  conflicting_run_ids = 'setequal',
  analysis_version = 'State analysis version differs',
  inconsistent_panel = 'Historical panel row changed')
for (defect in names(failures)) {
  for (entry in c('preflight', 'full')) {
    test_that(paste(entry, 'rejects remote defect', defect,
        'without activating the checkout'), {
      local_dir(daily_test_root())
      rlang::local_bindings(CONALCO_DAILY_ROOT = 'data/conalco/daily',
        .env = environment(actions_preflight))
      cloud = actions_test_cloud_checkout()
      state = cloud$state
      dav = actions_test_bad_cloud(state, defect)
      before = state$objects
      state$collector_calls = 0L
      local_envvar(c(GITHUB_ACTIONS = 'true', GITHUB_REF = 'refs/heads/main'))
      rlang::local_bindings(actions_dav = \() dav,
        run_conalco_daily = \(...) {
          state$collector_calls = state$collector_calls + 1L
          stop('Unexpected collector call')
        }, .env = environment(actions_preflight))
      if (entry == 'preflight')
        expect_error(actions_preflight(online = T), failures[[defect]])
      else expect_error(actions_full_run(), failures[[defect]])
      expect_identical(state$collector_calls, 0L)
      expect_identical(state$objects, before)
      expect_identical(lapply(vapply(cloud$checkout, `[[`, '', 'path'),
        refresh_file_record), cloud$checkout)
      expect_false(paste0(actions_state_base(), '/writer_lock') %in%
        state$directories)
    })
  }
}

test_that('explicit offline history checks still reject invalid local paths', {
  local_dir(daily_test_root())
  rlang::local_bindings(CONALCO_DAILY_ROOT = 'data/conalco/daily',
    .env = environment(actions_preflight))
  cloud = actions_test_cloud_checkout()
  rlang::local_bindings(actions_dav = \() cloud$state$dav,
    .env = environment(actions_preflight))
  expect_error(actions_preflight(), 'Missing release file')
  expect_null(actions_preflight(check_local_history = F))
  expect_length(cloud$state$calls, 0L)
})

test_that('both cloud entries retain the technical reference guard', {
  local_dir(daily_test_root())
  rlang::local_bindings(CONALCO_DAILY_ROOT = 'data/conalco/daily',
    .env = environment(actions_preflight))
  cloud = actions_test_cloud_checkout()
  writeLines('damaged reference', 'config/conalco_actions_reference.tar.gz')
  local_envvar(c(GITHUB_ACTIONS = 'true', GITHUB_REF = 'refs/heads/main'))
  rlang::local_bindings(actions_dav = \() cloud$state$dav,
    .env = environment(actions_preflight))
  expect_error(actions_preflight(online = T), 'Archive SHA-256 mismatch')
  expect_error(actions_full_run(), 'Archive SHA-256 mismatch')
  expect_length(cloud$state$calls, 0L)
})


test_that('ambiguous PUT is retained and unconfirmed cleanup fails closed', {
  local = tempfile()
  writeLines('synthetic archive', local)
  target = 'APM/conalco/raw/2026/11/run.tar.gz'
  state = actions_fake_dav()
  ambiguous = \(method, path, input = NULL, output = NULL,
      destination = NULL) {
    result = state$dav(method, path, input, output, destination)
    if (method == 'PUT') stop('Ambiguous PUT timeout')
    result
  }
  expect_error(actions_dav_publish(ambiguous, local, target), 'Ambiguous PUT')
  expect_null(state$objects[[target]])
  expect_length(state$objects, 1L)
  expect_false(any(vapply(state$calls, \(x) x[[1]] == 'DELETE', F)))
  expect_error(actions_dav_publish(state$dav, local, target),
    'Previous raw upload may be pending')
  expect_identical(sum(vapply(state$calls, \(x) x[[1]] == 'PUT', F)), 1L)

  for (removed in c(T, F)) {
    state = actions_fake_dav()
    concurrent = \(method, path, input = NULL, output = NULL,
        destination = NULL) {
      if (method == 'MOVE') {
        state$objects[[destination]] = state$objects[[path]]
        return(412L)
      }
      if (method == 'DELETE' && !removed) return(204L)
      state$dav(method, path, input, output, destination)
    }
    if (removed) {
      expect_identical(actions_dav_publish(concurrent, local, target),
        'verified')
      expect_length(state$objects, 1L)
    } else {
      expect_error(actions_dav_publish(concurrent, local, target),
        'still present after DELETE')
    }
  }
})
