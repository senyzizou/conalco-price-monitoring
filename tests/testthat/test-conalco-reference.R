# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  CONALCO PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261126

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) OFFLINE PRIVATE REFERENCE BOOTSTRAP
# <-----------------------------------------------------------------------------

source(file.path(APM_ROOT, 'R/conalco_actions.R'), local = F)

test_that('verified reference inputs are temporary and integrity gates fail', {
  root = tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = T), add = T)
  local_dir(root)
  dir.create('config')
  dir.create('outer/config', recursive = T)
  files = c('config/conalco_actions_reference.tar.gz',
    'config/conalco_actions_reference.tar.gz.json',
    'config/conalco_actions.json')
  nested = 'data/raw/conalco/synthetic.txt'
  inputs = file.path('outer', files)
  writeLines('synthetic', inputs[1])
  write_json(list(files = list(list(path = nested))), inputs[2],
    auto_unbox = T)
  writeLines('{}', inputs[3])
  archive = file.path(root, 'source.tar.gz')
  outer = normalizePath('outer')
  with_dir(outer, tar(archive, files, compression = 'gzip',
    tar = Sys.which('tar'), extra_flags = '--format=ustar'))
  sidecar = paste0(archive, '.json')
  run = 'catalog_refresh_20261126T010203_000000000000'
  remote = paste0('APM/conalco/raw/2026/11/', run, '.tar.gz')
  hashes = setNames(vapply(inputs, archive_sha256, '', USE.NAMES = F), files)
  receipt = list(run_id = run, archive = list(sha256 = archive_sha256(archive)),
    files = lapply(files, \(path) list(path = path, sha256 = hashes[[path]])))
  write_json(receipt, sidecar, auto_unbox = T)
  pins = list(archive_sha256 = archive_sha256(archive),
    sidecar_sha256 = archive_sha256(sidecar), members = as.list(hashes))
  dav = \(method, path, output) {
    stopifnot(method == 'GET')
    source = if (identical(path, remote)) archive else
      if (identical(path, paste0(remote, '.json'))) sidecar else NULL
    if (is.null(source)) return(404L)
    stopifnot(file.copy(source, output))
    200L
  }
  expect_identical(actions_with_reference(\() {
    stopifnot(all(file.exists(files)))
    dir.create(dirname(nested), recursive = T)
    writeLines('extracted reference', nested)
    'ready'
  }, dav, remote, pins), 'ready')
  expect_false(any(file.exists(c(files, nested))))
  expect_error(actions_with_reference(\() stop('Pipeline error'),
    dav, remote, pins), 'Pipeline error')
  expect_false(any(file.exists(files)))
  expect_error(actions_with_reference(\() stop('Must not run'),
    \(...) 404L, remote, pins), 'sidecar missing')

  corrupt_member = pins
  corrupt_member$members[[files[1]]] = paste(rep('0', 64), collapse = '')
  expect_error(actions_with_reference(\() stop('Must not run'),
    dav, remote, corrupt_member), 'member missing or altered')
  missing_archive = \(method, path, output) {
    if (identical(path, remote)) return(404L)
    dav(method, path, output)
  }
  expect_error(actions_with_reference(\() stop('Must not run'),
    missing_archive, remote, pins), 'source archive missing or altered')
  altered_archive = \(method, path, output) {
    if (identical(path, remote)) {
      writeLines('tampered archive', output)
      return(200L)
    }
    dav(method, path, output)
  }
  expect_error(actions_with_reference(\() stop('Must not run'),
    altered_archive, remote, pins), 'source archive missing or altered')
  writeLines('existing file', files[1])
  expect_error(actions_with_reference(\() stop('Must not run'),
    dav, remote, pins), 'staging paths already exist')
  expect_identical(readLines(files[1]), 'existing file')
  unlink(files[1])
  writeLines('bad', sidecar)
  expect_error(actions_with_reference(\() stop('Must not run'),
    dav, remote, pins), 'sidecar missing or altered')
  expect_false(any(file.exists(files)))
})
