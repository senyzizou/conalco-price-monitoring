# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261026

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) RUNNER GUARDS AND EXISTING PANEL
# <-----------------------------------------------------------------------------

actions_secrets = \(get = Sys.getenv) {
  names = c('UKEBOX_WEBDAV_URL', 'UKEBOX_USERNAME', 'UKEBOX_APP_PASSWORD')
  values = vapply(names, get, '')
  if (any(!nzchar(trimws(values))))
    stop('Missing required secret: ', paste(names[!nzchar(trimws(values))],
      collapse = ', '), call. = FALSE)
  if (!identical(unname(sub('/+$', '', values[1])),
      'https://box.uke.de/remote.php/webdav'))
    stop('Unexpected WebDAV endpoint', call. = FALSE)
  unname(as.list(values))
}

actions_history = \() {
  if (conalco_storage_mode() == 'versioned') {
    root = conalco_versioned_root()
    conalco_versioned_validate(root)
    files = list.files(root, recursive = TRUE, full.names = TRUE)
    files = files[!grepl('/transactions/', files)]
    return(list(records = conalco_versioned_records(root),
      files = lapply(files, refresh_file_record)))
  }
  records = private_check_panel()
  paths = sort(list.files(file.path(CONALCO_DAILY_ROOT, 'runs'),
    recursive = TRUE, full.names = TRUE))
  list(records = records, files = lapply(paths, refresh_file_record))
}

actions_history_unchanged = \(history) {
  for (entry in history$files) {
    if (!file.exists(entry$path) || !identical(
        archive_sha256(entry$path), entry$sha256))
      stop('Historical run changed: ', entry$path)
  }
  invisible(TRUE)
}

actions_day_exists = \(records, date) {
  any(vapply(records, \(record) identical(record$date, date), logical(1)))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) BOUNDED WEBDAV; NO RESPONSE BODY OR CREDENTIALS IN DIAGNOSTICS
# <-----------------------------------------------------------------------------

actions_phase = \(phase, code) {
  started = proc.time()[['elapsed']]
  completed = F
  cat('[Conalco] ', phase, ': start\n', sep = '')
  flush.console()
  on.exit({
    cat(sprintf('[Conalco] %s: %s (%.2f s)\n', phase,
      if (completed) 'ok' else 'failed',
      proc.time()[['elapsed']] - started))
    flush.console()
  }, add = T)
  result = force(code)
  completed = T
  result
}

actions_dav = \(secrets = actions_secrets()) {
  base = sub('/+$', '', secrets[[1]])
  \(method, path, input = NULL, output = NULL, destination = NULL) {
    archive_safe_paths(path)
    raw_put = method == 'PUT' && grepl(
      paste0('^APM/conalco/raw/[0-9]{4}/[0-9]{2}/',
        '[^/]+[.]tar[.]gz[.]pending-[A-Za-z0-9_-]+$'),
      path)
    limit = if (raw_put) 900L else 300L
    req = request(paste0(base, '/', path)) %>%
      req_auth_basic(secrets[[2]], secrets[[3]]) %>%
      req_method(method) %>% req_timeout(limit) %>%
      req_options(followlocation = FALSE, connecttimeout = 30) %>%
      req_error(is_error = \(response) FALSE)
    if (method == 'PROPFIND') req = req_headers(req, Depth = '1')
    if (!is.null(input)) req = req_body_file(req, input)
    if (!is.null(destination)) {
      archive_safe_paths(destination)
      req = req_headers(req, Destination = paste0(base, '/', destination),
        Overwrite = 'F')
    }
    # Only fixed method labels; never print URLs, paths or library errors.
    label = if (method %in% c('GET', 'PUT', 'DELETE', 'MOVE', 'MKCOL',
        'PROPFIND')) method else 'request'
    actions_phase(paste('WebDAV', label), {
      result = tryCatch(suppressWarnings(req_perform(req, path = output)),
        error = \(e) {
          timeout = inherits(e, 'curl_error_operation_timedout') ||
            inherits(e$parent, 'curl_error_operation_timedout')
          stop(if (timeout) sprintf('WebDAV request timed out (limit %d s)',
            limit) else sprintf(paste0('WebDAV transport failure ',
              '(connect limit 30 s; request limit %d s)'), limit), call. = F)
        })
      status = resp_status(result)
      if (!status %in% c(200L, 201L, 204L, 207L, 404L, 405L, 412L))
        stop('WebDAV HTTP ', status, ': ', label, call. = F)
      status
    })
  }
}

actions_dav_read = \(dav, path, output) {
  status = dav('GET', path, output = output)
  if (!status %in% c(200L, 404L)) stop('Unexpected WebDAV read status')
  status == 200L
}

actions_dav_publish = \(dav, local, remote) {
  scratch = tempfile(fileext = if (endsWith(remote, '.gz')) '.gz' else '.json')
  on.exit(unlink(scratch), add = TRUE)
  expected = archive_sha256(local)
  verify = \() {
    if (!actions_dav_read(dav, remote, scratch) ||
        !identical(archive_sha256(scratch), expected))
      stop('Conflicting or missing remote SHA-256: ', remote)
  }
  if (grepl('^APM/conalco/raw/.+[.]tar[.]gz$', remote)) {
    if (dav('PROPFIND', dirname(remote), output = scratch) != 207L)
      stop('Cannot inspect raw archive directory before publication')
    hrefs = xml_text(xml_find_all(read_xml(scratch),
      '//*[local-name()="href"]'))
    if (any(startsWith(basename(URLdecode(hrefs)),
        paste0(basename(remote), '.pending-'))))
      stop('Previous raw upload may be pending; inspect before recovery')
  }
  if (actions_dav_read(dav, remote, scratch)) {
    if (!identical(archive_sha256(scratch), expected))
      stop('Conflicting remote SHA-256: ', remote)
    return(invisible('identical'))
  }
  token = paste(Sys.getenv('GITHUB_RUN_ID', 'local'),
    Sys.getenv('GITHUB_RUN_ATTEMPT', '1'), Sys.getpid(), sep = '-')
  pending = paste0(remote, '.pending-', token)
  # A previous or still-processing PUT must be reconciled by an operator.
  if (actions_dav_read(dav, pending, scratch))
    stop('Temporary WebDAV object already exists; inspect before recovery')
  if (!dav('PUT', pending, input = local) %in% c(200L, 201L, 204L))
    stop('WebDAV temporary upload failed')
  if (!actions_dav_read(dav, pending, scratch) ||
      !identical(archive_sha256(scratch), expected))
    stop('Temporary upload SHA-256 mismatch')
  status = dav('MOVE', pending, destination = remote)
  if (!status %in% c(201L, 204L, 412L)) stop('WebDAV MOVE failed')
  # Also verify after MOVE, including a concurrent identical publication.
  verify()
  # PUT and MOVE both returned and the final SHA-256 matches. A 412 can
  # leave this attempt's verified temporary object beside the final object.
  if (actions_dav_read(dav, pending, scratch)) {
    if (status != 412L ||
        !identical(archive_sha256(scratch), expected))
      stop('Unexpected temporary WebDAV object; inspect before recovery')
    if (!dav('DELETE', pending) %in% c(200L, 204L))
      stop('Temporary WebDAV DELETE was not confirmed')
    if (actions_dav_read(dav, pending, scratch))
      stop('Temporary WebDAV object still present after DELETE')
  }
  invisible('verified')
}

actions_connection_test = \(dav) {
  local = tempfile()
  downloaded = tempfile()
  on.exit(unlink(c(local, downloaded)), add = TRUE)
  actions_mkdirs(dav, 'APM/conalco/connection_test')
  token = paste(Sys.getenv('GITHUB_RUN_ID', 'local'),
    Sys.getenv('GITHUB_RUN_ATTEMPT', '1'), Sys.getpid(), sep = '-')
  remote = paste0('APM/conalco/connection_test/preflight-', token, '.txt')
  writeLines('APM harmless connection test', local)
  if (actions_dav_read(dav, remote, downloaded))
    stop('Connection test object already exists')
  if (!dav('PUT', remote, input = local) %in% c(200L, 201L, 204L) ||
      !actions_dav_read(dav, remote, downloaded) ||
      !identical(archive_sha256(local), archive_sha256(downloaded)))
    stop('WebDAV connection test failed')
  if (!dav('DELETE', remote) %in% c(200L, 204L) ||
      actions_dav_read(dav, remote, downloaded))
    stop('WebDAV connection test cleanup not confirmed')
  invisible(TRUE)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) ARCHIVE SELECTION, SAFE RESTORE AND DURABLE RECEIPT
# <-----------------------------------------------------------------------------

actions_archive_paths = \(manifest_path, root = CONALCO_DAILY_ROOT) {
  manifest = require_complete_catalog(manifest_path)
  directory = run_dir(manifest$run_id)
  inventory = read.csv(manifest$proof$raw_inventory$path,
    colClasses = 'character')
  review = read_json(file.path(directory, 'access_review_pages.json'))
  metadata = unique(c(inventory$metadata_file,
    unlist(review[endsWith(names(review), '_metadata')], use.names = FALSE)))
  bodies = vapply(metadata, \(path) read_archive(path)$meta$raw_file, '')
  top = list.files(directory, full.names = TRUE, pattern = '[.](json|csv)$')
  top = top[!grepl('/[0-9]+_[a-z]+[.]json$', top)]
  source = list.files(file.path(directory, 'collection_source'),
    recursive = TRUE, full.names = TRUE)
  source = source[!dir.exists(source)]
  paths = unique(c(manifest_path, metadata, bodies, top, source,
    vapply(manifest$proof, `[[`, '', 'path'),
    manifest$scope_review$path,
    'renv.lock', 'R/storage_transport.R', 'R/conalco_actions.R',
    'config/conalco_actions.json',
    'config/conalco_actions_reference.tar.gz',
    'config/conalco_actions_reference.tar.gz.json',
    file.path(root, 'runs', manifest$run_id,
      c('manifest.json', 'observations.csv.gz'))))
  archive_safe_paths(paths)
  paths
}

actions_restore = \(archive, receipt, target) {
  target = normalizePath(target, mustWork = TRUE)
  entries = receipt$files
  paths = vapply(entries, `[[`, '', 'path')
  hashes = vapply(entries, `[[`, '', 'sha256')
  archive_safe_paths(paths)
  if (!grepl('^[0-9a-f]{64}$', receipt$archive$sha256) ||
      !identical(archive_sha256(archive), receipt$archive$sha256) ||
      any(!grepl('^[0-9a-f]{64}$', hashes)))
    stop('Archive SHA-256 mismatch')
  members = archive_members(archive, roots = character(), allowed_paths = paths)
  if (!setequal(members, paths)) stop('Archive inventory mismatch')
  release_guard_paths(target, paths)
  # Restore into a new scratch directory first; validate all hashes before copy.
  scratch = tempfile('actions_restore_')
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  untar(archive, exdir = scratch, tar = 'internal')
  for (i in seq_along(paths)) {
    restored = file.path(scratch, paths[i])
    if (!identical(archive_sha256(restored), hashes[i]))
      stop('Restored member SHA-256 mismatch')
    existing = file.path(target, paths[i])
    if (file.exists(existing) &&
        !identical(archive_sha256(existing), hashes[i]))
      stop('Conflicting restore target: ', paths[i])
  }
  for (path in paths) {
    destination = file.path(target, path)
    if (file.exists(destination)) next
    ensure_dir(dirname(destination))
    if (!file.copy(file.path(scratch, path), destination))
      stop('Cannot restore archive member')
  }
  invisible(paths)
}

actions_verify_archive = \(archive, receipt) {
  scratch = tempfile('actions_verify_')
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  actions_restore(archive, receipt, scratch)
  id = receipt$run_id
  run_dir(id)
  old = getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(scratch)
  manifest = require_complete_catalog(file.path(run_dir(id), 'manifest.json'))
  if (!identical(manifest$run_id, id)) stop('Archive Run-ID mismatch')
  if (!identical(basename(archive), paste0(id, '.tar.gz')))
    stop('Archive filename differs from Run-ID')
  for (entry in manifest$proof) {
    if (!identical(archive_sha256(entry$path), entry$sha256))
      stop('Archive completeness proof SHA-256 mismatch')
  }
  inventory = read.csv(manifest$proof$raw_inventory$path,
    colClasses = 'character')
  if (!nrow(inventory)) stop('Archive has no original responses')
  for (i in seq_len(nrow(inventory))) {
    original = read_archive(inventory$metadata_file[i])
    if (!identical(original$meta$run_id, id) || !identical(
        original$meta$raw_file, inventory$raw_file[i]) || !identical(
        original$meta$raw_sha256, inventory$raw_sha256[i]))
      stop('Archive original response linkage mismatch')
  }
  daily_root = if (is.null(receipt$daily_root)) CONALCO_DAILY_ROOT else
    receipt$daily_root
  archive_safe_paths(daily_root)
  record = read_json(file.path(daily_root, 'runs', id, 'manifest.json'))
  if (!identical(record$run_id, id) || !identical(
      record$source_manifest$sha256, archive_sha256(
        record$source_manifest$path))) stop('Archive source linkage mismatch')
  day = daily_read_csv(record$observations$path)
  if (!identical(archive_sha256(record$observations$path),
      record$observations$sha256) || !nrow(day) ||
      anyDuplicated(day[c('retailer', 'retailer_product_id', 'daily_date')]) ||
      anyNA(day[c('retailer', 'retailer_product_id', 'daily_date')]) ||
      !all(day$retailer == 'conalco') ||
      !all(day$run_id == id) || !all(day$daily_date == record$date) ||
      !all(berlin_date(day$observed_at) == record$date))
    stop('Archive observation linkage mismatch')
  invisible(record)
}

actions_upload_archive = \(archive, receipt, date, dav) {
  record = actions_verify_archive(archive, receipt)
  if (!identical(record$date, date)) stop('Archive date differs from target')
  if (!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', date)) stop('Invalid date')
  directory = paste('APM/conalco/raw', substr(date, 1, 4),
    substr(date, 6, 7), sep = '/')
  for (path in c(paste0('APM/conalco/raw/', substr(date, 1, 4)), directory)) {
    status = dav('MKCOL', path)
    if (!status %in% c(201L, 405L)) stop('Cannot create WebDAV directory')
  }
  remote = paste0(directory, '/', receipt$run_id, '.tar.gz')
  sidecar = paste0(archive, '.json')
  actions_dav_publish(dav, archive, remote)
  # A recovery keeps the original receipt bytes, making sidecars idempotent.
  actions_dav_publish(dav, sidecar, paste0(remote, '.json'))
  list(path = remote, sha256 = receipt$archive$sha256,
    manifest_sha256 = archive_sha256(sidecar), verified_at_utc = utc_now(),
    verification = 'downloaded SHA-256 before and after MOVE')
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) PREFLIGHT AND EXPLICIT FULL RUN; EXISTING SCIENTIFIC PIPELINE
# <-----------------------------------------------------------------------------

actions_with_reference = \(operation, dav = actions_dav(),
    remote = Sys.getenv('CONALCO_REFERENCE_ARCHIVE'),
    pins = read_json('config/conalco_reference_pins.json')) {
  pattern = paste0('^APM/conalco/raw/[0-9]{4}/[0-9]{2}/',
    'catalog_refresh_[0-9]{8}T[0-9]{6}_[0-9a-f]{12}[.]tar[.]gz$')
  if (!grepl(pattern, remote)) stop('Invalid private reference archive')
  files = c('config/conalco_actions_reference.tar.gz',
    'config/conalco_actions_reference.tar.gz.json',
    'config/conalco_actions.json')
  hashes = unlist(pins$members[files], use.names = F)
  if (!setequal(names(pins$members), files) ||
      length(hashes) != length(files) ||
      !all(grepl('^[a-f0-9]{64}$',
        c(pins$archive_sha256, pins$sidecar_sha256, hashes))))
    stop('Invalid technical reference pins')
  root = normalizePath(getwd())
  release_guard_paths(root, files)
  if (any(file.exists(files))) stop('Reference staging paths already exist')
  scratch = tempfile('reference_inputs_')
  if (!dir.create(scratch, mode = '0700'))
    stop('Cannot create private reference scratch directory')
  scratch = normalizePath(scratch, mustWork = T)
  on.exit(unlink(scratch, recursive = T), add = T)
  archive = file.path(scratch, 'source.tar.gz')
  sidecar = paste0(archive, '.json')
  if (!actions_dav_read(dav, paste0(remote, '.json'), sidecar) ||
      !identical(archive_sha256(sidecar), pins$sidecar_sha256))
    stop('Private reference source sidecar missing or altered')
  receipt = read_json(sidecar)
  if (!identical(receipt$archive$sha256, pins$archive_sha256) ||
      !identical(paste0(receipt$run_id, '.tar.gz'), basename(remote)))
    stop('Private reference source identity mismatch')
  inventory = vapply(receipt$files, `[[`, '', 'path')
  at = match(files, inventory)
  if (anyNA(at) || !identical(vapply(receipt$files[at], `[[`, '', 'sha256'),
      hashes))
    stop('Pinned reference member missing or altered in source sidecar')
  if (!actions_dav_read(dav, remote, archive) ||
      !identical(archive_sha256(archive), pins$archive_sha256))
    stop('Private reference source archive missing or altered')
  members = archive_members(archive, roots = character(),
    allowed_paths = inventory)
  if (!setequal(members, inventory))
    stop('Private reference source archive inventory differs')
  untar(archive, files = files, exdir = scratch, tar = 'internal')
  inputs = file.path(scratch, files)
  release_guard_paths(scratch, files)
  if (!all(file.exists(inputs)) ||
      !identical(vapply(inputs, archive_sha256, '', USE.NAMES = F), hashes))
    stop('Extracted private reference member missing or altered')
  nested = vapply(read_json(inputs[2])$files, `[[`, '', 'path')
  release_guard_paths(root, nested)
  if (any(!grepl('^data/(raw/conalco|processed/catalog_refresh)/', nested)) ||
      any(file.exists(nested))) stop('Reference restore paths conflict')
  # The established pipeline needs these paths in the ephemeral checkout.
  # Remove precisely those paths before the separate retain-attempt step.
  on.exit(unlink(file.path(root, c(files, nested))), add = T)
  if (!all(file.copy(inputs, files))) stop('Cannot stage private reference')
  operation()
}

actions_reference = \(target) {
  archive = 'config/conalco_actions_reference.tar.gz'
  receipt = read_json(paste0(archive, '.json'))
  actions_phase('reference.archive_validate',
    actions_restore(archive, receipt, target))
  old = getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(target)
  config = read_json('config/conalco_actions.json')
  path = tempfile('routes_', tmpdir = getwd())
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  actions_phase('reference.terms_validate',
    daily_terms_document(config$reviewed_terms_reference$path))
  actions_phase('reference.routes_validate',
    daily_prepare_listing_routes(config, path))
  invisible(TRUE)
}

actions_preflight = \(online = FALSE, check_local_history = TRUE) {
  # Cloud history is validated after restore, never against checkout paths.
  history = if (!online && check_local_history &&
      dir.exists(CONALCO_DAILY_ROOT))
    actions_phase('preflight.local_history', actions_history()) else NULL
  scratch = tempfile('actions_reference_')
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  ensure_dir(file.path(scratch, 'config'))
  file.copy('config/conalco_actions.json', file.path(scratch, 'config'))
  actions_phase('preflight.reference', actions_reference(scratch))
  stopifnot(is.function(run_conalco_daily), is.function(daily_archive_run))
  if (online) {
    dav = actions_dav()
    actions_phase('preflight.connection', actions_connection_test(dav))
    restored = actions_phase('preflight.state_restore',
      actions_state_restore(dav, scratch))
  } else if (!is.null(history))
    actions_phase('preflight.history_unchanged',
      actions_history_unchanged(history))
  cat('Preflight passed; no collection or research data writes.\n')
  invisible(if (online) restored else history)
}

actions_publish_receipt = \(id, durable) {
  if (conalco_storage_mode() == 'versioned') {
    root = conalco_versioned_root()
    history = dedup_history(root)
    dates = names(history)[vapply(history, \(r) r$run_id == id, logical(1))]
    stopifnot(length(dates) == 1L)
    return(conalco_record_raw_receipt(root, dates, durable))
  }
  path = file.path(CONALCO_DAILY_ROOT, 'runs', id, 'manifest.json')
  record = read_json(path)
  record$raw_archive = durable
  daily_replace(path, \(p) write_json(record, p, auto_unbox = TRUE,
    pretty = TRUE, null = 'null'))
  path = file.path(CONALCO_DAILY_ROOT, 'manifest.json')
  receipt = read_json(path)
  receipt$raw_archives = lapply(daily_run_records(CONALCO_DAILY_ROOT),
    \(record) list(run_id = record$run_id, archive = record$raw_archive))
  daily_replace(path, \(p) write_json(receipt, p, auto_unbox = TRUE,
    pretty = TRUE, null = 'null'))
}

actions_full_run = \(recovery = FALSE) {
  if (!identical(Sys.getenv('GITHUB_ACTIONS'), 'true') ||
      !identical(Sys.getenv('GITHUB_REF'), 'refs/heads/main'))
    stop('Full Run is restricted to an explicit GitHub main dispatch')
  actions_preflight(check_local_history = FALSE)
  dav = actions_dav()
  lock = actions_state_lock(dav)
  on.exit({
    if (!dav('DELETE', lock) %in% c(200L, 204L))
      warning('Remote writer lock needs manual recovery')
  }, add = TRUE)
  scratch = tempfile('runner_state_')
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  restored = actions_state_restore(dav, scratch)
  # Replace only the disposable runner copy after full remote validation.
  if (dir.exists(CONALCO_DAILY_ROOT))
    unlink(CONALCO_DAILY_ROOT, recursive = TRUE)
  ensure_dir(dirname(CONALCO_DAILY_ROOT))
  if (!file.rename(file.path(scratch, CONALCO_DAILY_ROOT),
      CONALCO_DAILY_ROOT)) stop('Cannot activate restored runner state')
  if (!is.null(restored$versioned_store)) {
    moved_bridge = file.path(CONALCO_DAILY_ROOT, 'versioned_bridge')
    if (identical(restored$versioned_store,
        file.path(scratch, moved_bridge)))
      restored$versioned_store = moved_bridge
    conalco_merge_store(restored$versioned_store, conalco_versioned_root())
  } else if (conalco_storage_mode() == 'versioned') {
    conalco_sync_legacy()
  }
  history = actions_history()
  if (conalco_storage_mode('ukebox_results') == 'incremental')
    history$versioned_checkpoint = restored$commit
  history$package_project = restored$package_project
  today = format(Sys.time(), '%Y-%m-%d', tz = 'Europe/Berlin')
  if (conalco_storage_mode() == 'versioned' ||
      conalco_storage_mode('ukebox_results') == 'incremental') {
    durable = vapply(restored$records, \(r) r$date, '')
    current = names(dedup_history(conalco_versioned_root()))
    if (today %in% current && !today %in% durable && !recovery)
      stop('Unarchived local day exists; use explicit recovery')
  }
  if (actions_day_exists(history$records, today) && !recovery) {
    cat('Complete Berlin day already present; no collection.\n')
    return(invisible(NULL))
  }
  dav = actions_dav()
  backup = ensure_dir('tmp/actions_archives')
  finish = \(manifest_path, backup_directory) {
    manifest = require_complete_catalog(manifest_path)
    id = manifest$run_id
    archive = file.path(backup_directory, paste0(id, '.tar.gz'))
    if (!recovery) {
      file.copy('tmp/actions_config.json',
        file.path(run_dir(id), 'actions_config.json'))
      archive_root = if (conalco_storage_mode() == 'versioned')
        conalco_raw_bridge(manifest_path) else CONALCO_DAILY_ROOT
      paths = actions_archive_paths(manifest_path, root = archive_root)
      daily_archive_run(manifest_path, backup_directory,
        root = archive_root, paths = paths)
    }
    receipt = read_json(paste0(archive, '.json'))
    record = actions_verify_archive(archive, receipt)
    rescue = ensure_dir('tmp/actions_recovery')
    for (source in c(archive, paste0(archive, '.json'))) {
      target = file.path(rescue, basename(source))
      if (file.exists(target)) {
        if (!identical(archive_sha256(source), archive_sha256(target)))
          stop('Conflicting validated recovery archive')
      } else if (!file.copy(source, target))
        stop('Cannot retain validated recovery archive')
    }
    durable = actions_upload_archive(archive, receipt, record$date, dav)
    actions_history_unchanged(history)
    actions_publish_receipt(id, durable)
    private_check_panel()
    actions_state_publish(dav, history, 'tmp/actions_state')
    cat('Complete panel committed to UKEbox:', id, '\n')
    writeLines(id, 'tmp/actions_run_id.txt')
    return(invisible(id))
  }
  if (recovery) {
    remote = Sys.getenv('RECOVERY_ARCHIVE')
    if (nzchar(remote)) {
      pattern = paste0('^APM/conalco/raw/[0-9]{4}/[0-9]{2}/',
        'catalog_refresh_[0-9]{8}T[0-9]{6}_[0-9a-f]{12}[.]tar[.]gz$')
      if (!grepl(pattern, remote)) stop('Invalid private recovery path')
      for (suffix in c('', '.json')) {
        target = file.path(backup, paste0(basename(remote), suffix))
        if (file.exists(target)) stop('Recovery target already exists')
        if (!actions_dav_read(dav, paste0(remote, suffix), target))
          stop('Private recovery archive or sidecar missing')
      }
    }
    archives = list.files(backup, '[.]tar[.]gz$', full.names = TRUE)
    if (length(archives) != 1L) stop('Expected one validated recovery archive')
    receipt = read_json(paste0(archives, '.json'))
    record = actions_verify_archive(archives, receipt)
    for (entry in record$code) {
      if (!identical(archive_sha256(entry$path), entry$sha256))
        stop('Recovery scientific code differs; explicit review required')
    }
    if (actions_day_exists(history$records, record$date)) {
      cat('Recovery date already committed; no publication.\n')
      return(invisible(NULL))
    }
    actions_restore(archives, receipt, getwd())
    append_conalco_daily(record$source_manifest$path)
    return(finish(record$source_manifest$path, backup))
  }
  actions_reference(getwd())
  config = read_json('config/conalco_actions.json')
  config$enabled = TRUE
  config$access_review_approved = TRUE
  config$collection_authorization = paste('Explicit GitHub Full Run dispatch',
    Sys.getenv('GITHUB_RUN_ID'), 'under user preparation request 2026-10-26')
  config$backup_directory = backup
  # Offline archive/restore checks passed; remote permissions just verified.
  config$backup_restore_verified = TRUE
  config$disabled_reason = NULL
  path = 'tmp/actions_config.json'
  write_json(config, path, auto_unbox = TRUE, pretty = TRUE, null = 'null')
  id = run_conalco_daily(manual = TRUE, config_file = path, finalize = finish)
  if (!file.exists('tmp/actions_run_id.txt'))
    stop('No complete archived run; publication blocked: ', id)
  invisible(id)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) OFFLINE REPRODUCTION FROM A PUBLISHED UKEBOX ARCHIVE
# <-----------------------------------------------------------------------------

actions_reproduce = \(archive) {
  archive = normalizePath(archive, mustWork = TRUE)
  receipt = read_json(paste0(archive, '.json'))
  id = receipt$run_id
  run_dir(id)
  published = read_json(file.path(CONALCO_DAILY_ROOT, 'runs', id,
      'manifest.json'))
  if (is.null(published$raw_archive) || !identical(
      archive_sha256(archive), published$raw_archive$sha256) ||
      !identical(archive_sha256(paste0(archive, '.json')),
        published$raw_archive$manifest_sha256))
    stop('Archive is not the published immutable source')
  actions_verify_archive(archive, receipt)
  scratch = tempfile('actions_reproduction_')
  dir.create(scratch)
  previous = getwd()
  on.exit({
    setwd(previous)
    unlink(scratch, recursive = TRUE)
  }, add = TRUE)
  actions_restore(archive, receipt, scratch)
  ensure_dir(file.path(scratch, 'renv'))
  if (!file.copy('renv/activate.R', file.path(scratch, 'renv')))
    stop('Cannot supply offline renv activation')
  setwd(scratch)
  snapshot = read_json(file.path(run_dir(id), 'collection_source',
      'source_snapshot.json'))
  paths = vapply(snapshot$files, `[[`, '', 'path')
  archive_safe_paths(paths)
  if (any(!grepl('^(R|scripts)/.*[.]R$', paths)))
    stop('Unexpected frozen source path')
  for (entry in snapshot$files) {
    bytes = charToRaw(entry$source)
    if (!identical(digest(bytes, algo = 'sha256', serialize = FALSE),
        entry$sha256)) stop('Frozen code SHA-256 mismatch')
    ensure_dir(dirname(entry$path))
    writeBin(bytes, entry$path)
  }
  # Existing scripts recalculate this day only, using its frozen R source.
  release_rscript('--vanilla', 'scripts/run_conalco_daily.R', '--rebuild')
  release_rscript('--vanilla', 'scripts/verify_daily.R',
    paste0('--run-id=', id), '--sources')
  cat('Published day reproduced from original responses:', id, '\n')
  invisible(TRUE)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) EXISTING ARCHIVE GUARDS; NO HISTORICAL SCRIPT ENTRY POINT
# <-----------------------------------------------------------------------------

# Copied from reproduce_from_archives.R, original 261021, adapted 261026.

archive_sha256 = \(path) digest(file = path, algo = "sha256")

archive_safe_paths = \(paths) {
  if (!length(paths) || anyNA(paths) ||
      any(!grepl("^[A-Za-z0-9_./-]+$", paths)) ||
      any(grepl("^/|/$|//|(^|/)[.]{1,2}(/|$)", paths)) ||
      anyDuplicated(tolower(paths))) stop("Unsafe or duplicate archive paths")
  invisible(paths)
}

# Only regular POSIX ustar files are needed by the frozen archives. Reject
# links, devices, directories and extension headers instead of interpreting
# alternate path overrides. Scan all headers before extracting anything.

# One shared parser validates both legacy and incremental archives.
archive_members = storage_archive_members

# Existing streaming check from private_reproduction.R, original 261026.
private_check_panel = \(root = getwd(), check_rows = TRUE) {
  if (exists('conalco_storage_mode', mode = 'function') &&
      conalco_storage_mode() == 'versioned') {
    store = conalco_versioned_root(project = root)
    if (file.exists(file.path(store, 'schema.json'))) {
      conalco_versioned_validate(store)
      return(invisible(conalco_versioned_records(store)))
    }
    if (missing(root)) stop('Primary versioned store is missing')
  }
  root = normalizePath(root, mustWork = TRUE)
  daily = file.path(root, CONALCO_DAILY_ROOT)
  manifest = read_json(file.path(daily, 'manifest.json'))
  if (!identical(manifest$analysis_version, CONALCO_DAILY_VERSION))
    stop('State analysis version differs; explicit migration required')
  records = lapply(sort(list.files(file.path(daily, 'runs'),
    '^manifest[.]json$', recursive = TRUE, full.names = TRUE)), read_json)
  if (!length(records) || !all(vapply(records, \(record)
        identical(record$analysis_version, CONALCO_DAILY_VERSION),
        logical(1)))) stop('Missing or incompatible daily run records')
  dates = vapply(records, `[[`, '', 'date')
  ids = vapply(records, `[[`, '', 'run_id')
  counts = vapply(records, `[[`, numeric(1), 'n_products')
  stopifnot(length(records) == manifest$n_runs, !anyDuplicated(dates),
    !anyDuplicated(ids), setequal(ids, unlist(manifest$run_ids)),
    sum(counts) == manifest$n_observations)
  entries = c(list(manifest$panel, manifest$statistics),
    lapply(records, `[[`, 'observations'))
  paths = vapply(entries, `[[`, '', 'path')
  archive_safe_paths(paths)
  release_guard_paths(root, paths)
  release_check_files(file.path(root, paths),
    file.size(file.path(root, paths)), vapply(entries, `[[`, '', 'sha256'))
  if (!check_rows) return(invisible(records))
  connection = gzfile(file.path(root, manifest$panel$path), 'rt')
  on.exit(close(connection))
  header = readLines(connection, n = 1L)
  columns = names(read.csv(text = header, nrows = 0L, check.names = FALSE))
  for (record in records[order(dates)]) {
    day = read.csv(file.path(root, record$observations$path),
      colClasses = 'character', na.strings = '', check.names = FALSE)
    keys = c('retailer', 'retailer_product_id', 'daily_date')
    stopifnot(nrow(day) == record$n_products, !anyNA(day[keys]),
      !anyDuplicated(day[keys]), all(nzchar(day$retailer_product_id)),
      all(day$retailer == 'conalco'), all(day$daily_date == record$date),
      all(day$run_id == record$run_id),
      all(berlin_date(day$observed_at) == record$date))
    day = day[order(day$retailer, day$retailer_product_id), ]
    for (column in setdiff(columns, names(day))) day[[column]] = NA_character_
    block = read.csv(connection, nrows = nrow(day), header = FALSE,
      col.names = columns, colClasses = 'character', na.strings = '',
      check.names = FALSE)
    expected = day[columns]
    rownames(expected) = rownames(block) = NULL
    if (!identical(block, expected)) stop('Historical panel row changed')
  }
  if (length(readLines(connection, n = 1L))) stop('Unexpected panel rows')
  cat('Panel matches validated run shards:', manifest$n_observations, '\n')
  invisible(records)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) IMMUTABLE UKEBOX STATE; A DATE COMMIT IS THE PUBLICATION POINT
# <-----------------------------------------------------------------------------

# Reuses the archive selection, checksum and WebDAV primitives above.
actions_state_base = \() {
  base = Sys.getenv('APM_STATE_BASE', 'APM/conalco/validation/state')
  archive_safe_paths(base)
  base
}

actions_mkdirs = \(dav, path) {
  parts = strsplit(path, '/', fixed = TRUE)[[1]]
  for (n in seq_along(parts)) {
    status = dav('MKCOL', paste(parts[seq_len(n)], collapse = '/'))
    if (!status %in% c(201L, 405L)) stop('Cannot create WebDAV directory')
  }
}

actions_state_list = \(dav) {
  scratch = tempfile(fileext = '.xml')
  on.exit(unlink(scratch), add = TRUE)
  directory = paste0(actions_state_base(), '/commits')
  status = dav('PROPFIND', directory, output = scratch)
  if (status == 404L) stop('No durable state; explicit seed upload required')
  if (status != 207L) stop('Cannot list durable state commits')
  doc = read_xml(scratch)
  hrefs = xml_text(xml_find_all(doc, '//*[local-name()="href"]'))
  names = basename(vapply(hrefs, URLdecode, ''))
  sort(unique(names[grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}[.]json$', names)]))
}

actions_state_restore = \(dav, target) {
  if (conalco_storage_mode('ukebox_results') == 'incremental')
    return(conalco_increment_restore(dav, target))
  names = actions_state_list(dav)
  if (!length(names)) stop('No committed state; explicit seed required')
  commit_path = paste0(actions_state_base(), '/commits/', tail(names, 1L))
  commit_file = tempfile(fileext = '.json')
  archive = tempfile(fileext = '.tar.gz')
  sidecar = paste0(archive, '.json')
  on.exit(unlink(c(commit_file, archive, sidecar)), add = TRUE)
  if (!actions_dav_read(dav, commit_path, commit_file))
    stop('State commit disappeared')
  commit = read_json(commit_file)
  expected = paste0(actions_state_base(), '/objects/', commit$sha256,
    '.tar.gz')
  if (!identical(commit$archive, expected) ||
      !identical(paste0(commit$date, '.json'), tail(names, 1L)) ||
      !identical(commit$daily_root, CONALCO_DAILY_ROOT))
    stop('Invalid durable state commit')
  if (!actions_dav_read(dav, expected, archive) ||
      !actions_dav_read(dav, paste0(expected, '.json'), sidecar) ||
      !identical(archive_sha256(archive), commit$sha256) ||
      !identical(archive_sha256(sidecar), commit$receipt_sha256))
    stop('Durable state checksum mismatch')
  receipt = read_json(sidecar)
  paths = vapply(receipt$files, `[[`, '', 'path')
  if (any(!startsWith(paths, paste0(CONALCO_DAILY_ROOT, '/'))))
    stop('State archive contains non-panel paths')
  actions_restore(archive, receipt, target)
  records = private_check_panel(target)
  if (!identical(sort(vapply(records, `[[`, '', 'date')), unlist(commit$dates)))
    stop('State date inventory differs')
  bridge = file.path(target, CONALCO_DAILY_ROOT, 'versioned_bridge')
  if (file.exists(file.path(bridge, 'schema.json'))) {
    checked = conalco_versioned_validate(bridge)
    stopifnot(identical(checked$dates, sort(vapply(records, `[[`, '', 'date'))))
    return(list(commit = commit, records = records, versioned_store = bridge))
  }
  list(commit = commit, records = records)
}

actions_state_pack = \(directory) {
  records = private_check_panel()
  paths = sort(list.files(CONALCO_DAILY_ROOT, recursive = TRUE,
    full.names = TRUE))
  paths = paths[!dir.exists(paths)]
  archive_safe_paths(paths)
  if (any(nzchar(Sys.readlink(paths)))) stop('Symlink in state')
  ensure_dir(directory)
  pending = tempfile('state_', tmpdir = directory, fileext = '.tar.gz')
  tar(pending, files = paths, compression = 'gzip', tar = Sys.which('tar'),
    extra_flags = '--format=ustar')
  hash = archive_sha256(pending)
  archive = file.path(directory, paste0(hash, '.tar.gz'))
  if (!file.rename(pending, archive)) stop('Cannot retain state archive')
  receipt = list(archive = refresh_file_record(archive),
    files = lapply(paths, refresh_file_record))
  sidecar = paste0(archive, '.json')
  write_json(receipt, sidecar, auto_unbox = TRUE, pretty = TRUE)
  scratch = tempfile('state_verify_')
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  actions_restore(archive, receipt, scratch)
  private_check_panel(scratch)
  dates = sort(vapply(records, `[[`, '', 'date'))
  commit = list(date = tail(dates, 1L), daily_root = CONALCO_DAILY_ROOT,
    dates = dates, archive = paste0(actions_state_base(), '/objects/',
      basename(archive)), sha256 = hash,
    receipt_sha256 = archive_sha256(sidecar))
  commit_file = file.path(directory, paste0(commit$date, '.json'))
  write_json(commit, commit_file, auto_unbox = TRUE, pretty = TRUE)
  list(archive = archive, sidecar = sidecar, commit = commit,
    commit_file = commit_file)
}

actions_state_publish = \(dav, history, directory) {
  if (conalco_storage_mode('ukebox_results') == 'incremental') {
    actions_history_unchanged(history)
    root = if (conalco_storage_mode() == 'versioned')
      conalco_versioned_root() else conalco_sync_legacy()
    return(conalco_publish_increment(root, directory,
      previous = history$versioned_checkpoint,
      publish = \(source, path) actions_dav_publish(dav, source, path),
      make_directory = \(path) actions_mkdirs(dav, path),
      project = if (is.null(history$versioned_checkpoint)) getwd() else
        history$package_project))
  }
  if (conalco_storage_mode() == 'versioned')
    return(conalco_legacy_state_publish(dav, history, directory))
  actions_history_unchanged(history)
  packed = actions_state_pack(directory)
  actions_mkdirs(dav, paste0(actions_state_base(), '/objects'))
  actions_mkdirs(dav, paste0(actions_state_base(), '/commits'))
  actions_dav_publish(dav, packed$archive, packed$commit$archive)
  actions_dav_publish(dav, packed$sidecar,
    paste0(packed$commit$archive, '.json'))
  # Never overwrite a date: a conflicting writer must stop here.
  actions_dav_publish(dav, packed$commit_file,
    paste0(actions_state_base(), '/commits/', basename(packed$commit_file)))
  invisible(packed$commit)
}

actions_state_lock = \(dav) {
  actions_mkdirs(dav, actions_state_base())
  path = paste0(actions_state_base(), '/writer_lock')
  if (dav('MKCOL', path) != 201L)
    stop('Remote writer lock exists; inspect Actions before manual recovery')
  path
}

actions_retain_attempt = \(dav = actions_dav()) {
  # Runs only in the ephemeral runner; credentials are never archive members.
  paths = list.files(c('logs/conalco_daily', 'data/raw/conalco',
      'data/processed/catalog_refresh', 'tmp/actions_recovery'),
    recursive = TRUE, full.names = TRUE, all.files = TRUE)
  paths = paths[!dir.exists(paths) & !grepl('/[.][^/]+', paths)]
  marker = 'tmp/actions_run_id.txt'
  if (file.exists(marker)) {
    id = readLines(marker, warn = FALSE)
    stopifnot(length(id) == 1L)
    sidecar = file.path('tmp/actions_recovery', paste0(id, '.tar.gz.json'))
    archived = read_json(sidecar)
    used = vapply(archived$files, `[[`, '', 'path')
    rescue = list.files('tmp/actions_recovery', full.names = TRUE)
    # Complete original responses already have a verified durable archive.
    paths = setdiff(paths, c(used, rescue))
    paths = paths[!grepl('/parsed_pages/', paths, fixed = TRUE)]
  }
  if (!length(paths)) return(invisible(NULL))
  archive_safe_paths(paths)
  if (any(nzchar(Sys.readlink(paths)))) stop('Symlink in technical attempt')
  directory = ensure_dir('tmp/actions_attempt')
  name = paste(Sys.getenv('GITHUB_RUN_ID', 'local'),
    Sys.getenv('GITHUB_RUN_ATTEMPT', '1'), sep = '-')
  archive = file.path(directory, paste0(name, '.tar.gz'))
  tar(archive, files = paths, compression = 'gzip', tar = Sys.which('tar'),
    extra_flags = '--format=ustar')
  receipt = list(archive = refresh_file_record(archive),
    files = lapply(paths, refresh_file_record), status = 'technical_attempt')
  sidecar = paste0(archive, '.json')
  write_json(receipt, sidecar, auto_unbox = TRUE, pretty = TRUE)
  actions_mkdirs(dav, 'APM/conalco/attempts')
  remote = paste0('APM/conalco/attempts/', basename(archive))
  actions_dav_publish(dav, archive, remote)
  actions_dav_publish(dav, sidecar, paste0(remote, '.json'))
  cat('Technical attempt retained in UKEbox:', remote, '\n')
  invisible(remote)
}

actions_prepare_seed = \(source_root = CONALCO_DAILY_ROOT,
  target_root = 'data/conalco/daily', output = 'tmp/actions_state_seed') {
  source_root = normalizePath(source_root, mustWork = TRUE)
  output = ensure_dir(output)
  output = normalizePath(output)
  before = lapply(list.files(source_root, recursive = TRUE, full.names = TRUE),
    refresh_file_record)
  scratch = tempfile('seed_')
  dir.create(scratch)
  previous = getwd()
  on.exit({
    setwd(previous)
    unlink(scratch, recursive = TRUE)
  }, add = TRUE)
  destination = file.path(scratch, target_root)
  ensure_dir(dirname(destination))
  if (!file.copy(source_root, dirname(destination), recursive = TRUE))
    stop('Cannot copy existing state')
  # file.copy retains the source basename; rename inside the scratch workspace.
  copied = file.path(dirname(destination), basename(source_root))
  if (copied != destination && !file.rename(copied, destination))
    stop('Cannot map state root')
  for (path in list.files(destination, '^manifest[.]json$',
      recursive = TRUE, full.names = TRUE)) {
    record = read_json(path)
    if (!is.null(record$observations)) record$observations$path =
      file.path(target_root, 'runs', record$run_id, 'observations.csv.gz')
    if (!is.null(record$panel)) record$panel$path =
      file.path(target_root, 'panel.csv.gz')
    if (!is.null(record$statistics)) record$statistics$path =
      file.path(target_root, 'statistics.csv')
    write_json(record, path, auto_unbox = TRUE, pretty = TRUE, null = 'null')
  }
  setwd(scratch)
  scope = new.env(parent = environment(actions_state_pack))
  scope$CONALCO_DAILY_ROOT = target_root
  pack = actions_state_pack
  check = private_check_panel
  environment(pack) = environment(check) = scope
  scope$private_check_panel = check
  packed = pack(output)
  for (entry in before) {
    if (!identical(archive_sha256(entry$path), entry$sha256))
      stop('Source history changed during seed preparation')
  }
  cat('Prepared seed only; no upload:', output, '\n')
  invisible(packed)
}

actions_check_deployment = \() {
  entries = read_json('config/conalco_actions_files.json')$files
  paths = vapply(entries, `[[`, '', 'path')
  archive_safe_paths(paths)
  release_guard_paths(getwd(), paths)
  for (entry in entries) {
    if (!file.exists(entry$path) || !identical(
        archive_sha256(entry$path), entry$sha256))
      stop('Missing or outdated Actions deployment file: ', entry$path)
  }
  invisible(TRUE)
}


# Origin: conalco_actions.R, 261026; consolidated 261030.
# 8) RESTORE A DURABLE STATE; PRESERVE IMMUTABLE OBSERVATIONS
# <-----------------------------------------------------------------------------

actions_publication_paths = \(paths) {
  archive_safe_paths(paths)
  daily = paste0('^data/conalco/daily/(panel[.]csv[.]gz|statistics[.]csv|',
    'manifest[.]json|runs/[A-Za-z0-9_]+/',
    '(observations[.]csv[.]gz|manifest[.]json))$')
  extra = c('.gitattributes', 'docs/files.csv',
    'docs/conalco_publication.json', 'config/conalco_lfs_usage.json')
  if (any(!grepl(daily, paths) & !paths %in% extra))
    stop('Unexpected publication file')
  invisible(paths)
}

actions_publication_index = \(root, files) {
  inventory = read.csv('docs/files.csv', colClasses = 'character',
    na.strings = '', check.names = FALSE)
  for (path in files) {
    row = match(path, inventory$path)
    if (is.na(row)) {
      entry = inventory[which(inventory$hash_policy == 'sha256')[1], ]
      entry$path = path
      entry$archive_members = '0'
      entry$role = 'validated_results_or_publication_metadata'
      entry$publication_status = 'private_repository_publication_approved'
      inventory = rbind(inventory, entry)
      row = nrow(inventory)
    }
    inventory$bytes[row] = as.character(file.size(file.path(root, path)))
    inventory$sha256[row] = archive_sha256(file.path(root, path))
    inventory$publication_status[row] =
      'private_repository_publication_approved'
  }
  inventory = inventory[order(inventory$path), ]
  ensure_dir(file.path(root, 'docs'))
  write.csv(inventory, file.path(root, 'docs/files.csv'),
    row.names = FALSE, na = '')
}

actions_lfs_pointer = \(lines) {
  if (length(lines) != 3L ||
      lines[1] != 'version https://git-lfs.github.com/spec/v1' ||
      !grepl('^oid sha256:[0-9a-f]{64}$', lines[2]) ||
      !grepl('^size [0-9]+$', lines[3])) stop('Invalid LFS pointer')
  list(oid = sub('^oid sha256:', '', lines[2]),
    bytes = as.numeric(sub('^size ', '', lines[3])))
}

actions_publication_sizes = \(files, panel, size = file.size) {
  bytes = size(files)
  if (anyNA(bytes) || any(!is.finite(bytes) | bytes <= 0))
    stop('Missing or invalid publication file size')
  if (sum(files == panel) != 1L || bytes[files == panel] > 2000000000)
    stop('Git LFS Free per-file limit exceeded')
  if (any(bytes[files != panel] > 100 * 1024^2))
    stop('Normal Git file exceeds 100 MiB')
  invisible(TRUE)
}

actions_prepare_publication = \(dav = actions_dav()) {
  if (conalco_storage_mode('github_publication') == 'versioned')
    return(conalco_actions_prepare_versioned(dav))
  if (!identical(CONALCO_DAILY_ROOT, 'data/conalco/daily'))
    stop('Publication requires the repository daily path')
  output = 'tmp/actions_publication'
  release_guard_paths(getwd(), output)
  if (dir.exists(output)) unlink(output, recursive = TRUE)
  payload = ensure_dir(file.path(output, 'payload'))
  restored = actions_state_restore(dav, payload)
  if (conalco_storage_mode('ukebox_results') == 'incremental')
    restored = conalco_legacy_publication_view(dav, payload, restored)
  records = restored$records
  ids = vapply(records, `[[`, '', 'run_id')
  latest = records[[match(restored$commit$date,
    vapply(records, `[[`, '', 'date'))]]
  raw = latest$raw_archive
  expected_raw = paste0('APM/conalco/raw/', substr(latest$date, 1, 4), '/',
    substr(latest$date, 6, 7), '/', latest$run_id, '.tar.gz')
  if (is.null(raw) || !identical(raw$path, expected_raw) ||
      !grepl('^[0-9a-f]{64}$', raw$sha256) ||
      !grepl('^[0-9a-f]{64}$', raw$manifest_sha256))
    stop('Latest state lacks a durable raw archive receipt')
  repairs = list()
  exports = list()
  previous_audit = 'docs/conalco_publication.json'
  if (file.exists(previous_audit)) {
    previous = read_json(previous_audit)
    repairs = previous$repairs
    exports = previous$historical_exports
  }
  if (is.null(repairs)) repairs = list()
  if (is.null(exports)) exports = list()
  local_paths = list.files(file.path(CONALCO_DAILY_ROOT, 'runs'),
    recursive = TRUE, full.names = TRUE, all.files = TRUE)
  local_paths = local_paths[!dir.exists(local_paths)]
  if (length(local_paths)) {
    actions_publication_paths(local_paths)
    release_guard_paths(getwd(), local_paths)
  }
  local_manifests = local_paths[basename(local_paths) == 'manifest.json']
  for (path in local_manifests) {
    id = basename(dirname(path))
    old = read_json(path)
    if (!identical(old$run_id, id) || !id %in% ids)
      stop('Historical Run-ID missing or conflicting in durable state: ', id)
    remote = records[[match(id, ids)]]
    observation = file.path(dirname(path), 'observations.csv.gz')
    if (!file.exists(observation) || !identical(
        archive_sha256(observation), old$observations$sha256) ||
        !identical(old$observations$sha256, remote$observations$sha256))
      stop('Historical observations conflict: ', id)
    if (!is.null(old$source_export_manifest)) {
      wrapper = c('source_export_manifest', 'external_transfer_approved',
        'source_dependencies', 'staging_code', 'analysis_code_provenance')
      outer = old[setdiff(names(old), wrapper)]
      inner = old$source_export_manifest
      inner$observations$path = outer$observations$path
      review = inner$conflict_review
      if (!is.null(review) && identical(review$path,
          'config/release_2026-10-16/le_coq_conflicts.csv') &&
          identical(outer$conflict_review$path,
            'config/le_coq_conflicts.csv') &&
          identical(review$sha256, outer$conflict_review$sha256)) {
        inner$conflict_review$path = outer$conflict_review$path
        repairs[[paste(path, 'conflict_review')]] = list(path = path,
          field = 'conflict_review.path', from = outer$conflict_review$path,
          to = review$path)
      }
      if (is.null(outer$code) && identical(old$analysis_code_provenance,
          'source_export_manifest.code')) inner$code = NULL
      if (!identical(outer, inner))
        stop('Historical export wrapper contradicts original manifest: ', id)
      exports[[id]] = list(path = path, sha256 = archive_sha256(path),
        source = rawToChar(readBin(path, 'raw', file.size(path))))
      old = old$source_export_manifest
    }
    canonical = file.path(CONALCO_DAILY_ROOT, 'runs', id,
      'observations.csv.gz')
    legacy = file.path('data/processed/exports/conalco_daily', 'runs', id,
      'observations.csv.gz')
    if (!old$observations$path %in% c(canonical, legacy))
      stop('Unexpected historical observation path: ', id)
    if (old$observations$path != canonical) repairs[[path]] = list(
      path = path, field = 'observations.path', from = old$observations$path,
      to = canonical)
    old$observations$path = canonical
    if (!is.null(old$raw_archive) &&
        !identical(old$raw_archive, remote$raw_archive))
      stop('Historical raw archive receipt conflict: ', id)
    old$raw_archive = remote$raw_archive = NULL
    if (!identical(old, remote)) stop('Historical manifest conflict: ', id)
  }
  observations = local_paths[basename(local_paths) == 'observations.csv.gz']
  if (any(!file.path(dirname(observations), 'manifest.json') %in%
      local_manifests)) stop('Historical observations lack a manifest')
  top = file.path(CONALCO_DAILY_ROOT, 'manifest.json')
  if (file.exists(top)) {
    old = read_json(top)
    for (field in c('panel', 'statistics')) {
      canonical = file.path(CONALCO_DAILY_ROOT,
        if (field == 'panel') 'panel.csv.gz' else 'statistics.csv')
      legacy = sub('data/conalco/daily',
        'data/processed/exports/conalco_daily', canonical, fixed = TRUE)
      if (!old[[field]]$path %in% c(canonical, legacy))
        stop('Unexpected historical aggregate path')
      if (old[[field]]$path != canonical) repairs[[paste(top, field)]] = list(
        path = top, field = paste0(field, '.path'),
        from = old[[field]]$path, to = canonical)
    }
  }
  files = sort(list.files(file.path(payload, CONALCO_DAILY_ROOT),
    recursive = TRUE, full.names = TRUE))
  files = substring(files, nchar(payload) + 2L)
  actions_publication_paths(files)
  panel = file.path(CONALCO_DAILY_ROOT, 'panel.csv.gz')
  oid = archive_sha256(file.path(payload, panel))
  bytes = file.size(file.path(payload, panel))
  attributes = if (file.exists('.gitattributes'))
    readLines('.gitattributes', warn = FALSE) else character()
  rule = readLines('config/conalco_panel.gitattributes', warn = FALSE)
  stopifnot(length(rule) == 1L)
  if (any(grepl('filter=lfs', attributes, fixed = TRUE) &
      !attributes %in% rule))
    stop('Only the daily panel may be configured for LFS by this publisher')
  ledger_path = 'config/conalco_lfs_usage.json'
  if (rule %in% attributes && !file.exists(ledger_path))
    stop('LFS usage ledger missing; inspect account usage before recovery')
  ledger = if (file.exists(ledger_path)) read_json(ledger_path) else
    list(version = 1L, objects = list())
  if (!identical(ledger$version, 1L)) stop('Unknown LFS usage ledger version')
  objects = ledger$objects
  known = vapply(objects, `[[`, '', 'oid')
  sizes = vapply(objects, `[[`, numeric(1), 'bytes')
  if (anyDuplicated(known) || any(!grepl('^[0-9a-f]{64}$', known)) ||
      any(!is.finite(sizes) | sizes <= 0 | sizes > 2000000000))
    stop('Invalid LFS usage ledger')
  if (oid %in% known) {
    if (sizes[match(oid, known)] != bytes) stop('LFS object size conflict')
  } else ledger$objects[[length(objects) + 1L]] = list(oid = oid,
    bytes = bytes, first_state_date = restored$commit$date)
  ledger$managed_storage_bytes = sum(vapply(ledger$objects, `[[`,
    numeric(1), 'bytes'))
  ledger$workflow_lfs_download_bytes = 0
  ledger$measurement = paste('Lower bound for committed panel objects;',
    'account usage also includes other repositories and orphan uploads')
  ensure_dir(file.path(payload, 'config'))
  ensure_dir(file.path(payload, 'docs'))
  writeLines(unique(c(attributes, rule)),
    file.path(payload, '.gitattributes'))
  write_json(ledger, file.path(payload, ledger_path), auto_unbox = TRUE,
    pretty = TRUE, null = 'null')
  if (length(repairs)) repairs = repairs[order(names(repairs))]
  if (length(exports)) exports = exports[order(names(exports))]
  state_base = if (conalco_storage_mode('ukebox_results') == 'incremental')
    conalco_increment_base() else actions_state_base()
  audit = list(state_commit = paste0(state_base, '/commits/',
      restored$commit$date, '.json'), state = restored$commit,
    repairs = repairs, historical_exports = exports)
  write_json(audit, file.path(payload, previous_audit), auto_unbox = TRUE,
    pretty = TRUE, null = 'null')
  files = c(files, '.gitattributes', ledger_path, previous_audit)
  actions_publication_index(payload, files)
  files = c(files, 'docs/files.csv')
  actions_publication_paths(files)
  actions_publication_sizes(file.path(payload, files),
    file.path(payload, panel))
  private_check_panel(payload)
  release_guard_paths(getwd(), files)
  plan = list(date = restored$commit$date, panel = panel, oid = oid,
    bytes = bytes, files = lapply(files, \(path) list(path = path,
      sha256 = archive_sha256(file.path(payload, path)))),
    checkout = lapply(files, \(path) list(path = path,
      sha256 = if (file.exists(path)) archive_sha256(path) else NULL)))
  write_json(plan, file.path(output, 'plan.json'), auto_unbox = TRUE,
    pretty = TRUE, null = 'null')
  cat('Validated UKEbox state prepared for GitHub:', plan$date,
    '; days:', length(records), '; observations:',
    sum(vapply(records, `[[`, numeric(1), 'n_products')),
    '; panel bytes:', bytes, '; managed LFS bytes:',
    ledger$managed_storage_bytes, '\n')
  invisible(plan)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 9) EXPLICIT COST GATE AND ONE ATOMIC REPOSITORY COMMIT
# <-----------------------------------------------------------------------------

actions_lfs_budget = \(managed_bytes, get = Sys.getenv) {
  if (!identical(get('APM_LFS_ZERO_BUDGET_CONFIRMED'), 'true'))
    stop('LFS PUSH BLOCKED: verify the account LFS zero-dollar stop budget')
  other = get('APM_LFS_OTHER_STORAGE_BYTES')
  if (!grepl('^[0-9]+$', other))
    stop('LFS PUSH BLOCKED: declare verified other account storage in bytes')
  total = managed_bytes + as.numeric(other)
  if (!is.finite(total) || total > 9 * 1024^3)
    stop('LFS PUSH BLOCKED: conservative 9 GiB storage ceiling reached')
  invisible(total)
}

actions_publication_git = \(args, allowed = 0L) {
  output = tempfile('publication_git_')
  on.exit(unlink(output), add = TRUE)
  status = system2('git', shQuote(args), stdout = output, stderr = output)
  if (!status %in% allowed) stop('Git publication failed: ', args[1],
    ' (exit ', status, '); UKEbox unchanged; retry mode=publish')
  list(status = status, output = readLines(output, warn = FALSE))
}

actions_commit_publication = \(git = actions_publication_git) {
  if (!identical(Sys.getenv('GITHUB_ACTIONS'), 'true') ||
      !identical(Sys.getenv('GITHUB_REF'), 'refs/heads/main'))
    stop('Publication commits are restricted to GitHub main')
  if (conalco_storage_mode('github_publication') == 'versioned') {
    if (!identical(Sys.getenv('GITHUB_REPOSITORY'),
        'senyzizou/conalco-price-monitoring'))
      stop('Public publication requires the canonical repository')
    plan = read_json('tmp/actions_publication/versioned_plan.json')
    return(conalco_commit_publication(plan, git,
      mode = Sys.getenv('APM_PUBLICATION_MODE'),
      event = Sys.getenv('GITHUB_EVENT_NAME')))
  }
  if (!identical(Sys.getenv('APM_PRIVATE_REPOSITORY'), 'true'))
    stop('Legacy LFS publication requires a private repository')
  output = 'tmp/actions_publication'
  payload = file.path(output, 'payload')
  plan = read_json(file.path(output, 'plan.json'))
  ledger = read_json(file.path(payload, 'config/conalco_lfs_usage.json'))
  actions_lfs_budget(ledger$managed_storage_bytes)
  files = vapply(plan$files, `[[`, '', 'path')
  actions_publication_paths(files)
  release_guard_paths(getwd(), c(output, files))
  for (entry in plan$files) {
    if (!identical(archive_sha256(file.path(payload, entry$path)),
        entry$sha256)) stop('Publication payload changed after validation')
  }
  for (entry in plan$checkout) {
    actual = if (file.exists(entry$path)) archive_sha256(entry$path) else NULL
    if (!identical(actual, entry$sha256))
      stop('Checkout changed during publication; prepare again')
  }
  private_check_panel(payload)
  if (!identical(archive_sha256(file.path(payload, plan$panel)), plan$oid) ||
      file.size(file.path(payload, plan$panel)) != plan$bytes)
    stop('LFS plan differs from validated panel')
  base = git(c('rev-parse', 'HEAD'))$output
  git(c('fetch', '--no-tags', 'origin', 'main'))
  if (!identical(git(c('rev-parse', 'origin/main'))$output, base))
    stop('main changed; rerun mode=publish on current main, no merge or force')
  if (length(git(c('diff', '--cached', '--name-only'))$output))
    stop('Unexpected pre-existing staged changes')
  git(c('lfs', 'version'))
  git(c('lfs', 'install', '--local'))
  for (path in files) {
    ensure_dir(dirname(path))
    if (!file.copy(file.path(payload, path), path, overwrite = TRUE))
      stop('Cannot activate publication file')
  }
  git(c('add', '--', files))
  staged = git(c('diff', '--cached', '--name-only'))$output
  if (any(!staged %in% files)) stop('Unexpected file in publication commit')
  if (!length(staged)) {
    cat('GitHub already contains this validated state; no commit.\n')
    return(invisible('identical'))
  }
  pointer_size = as.numeric(git(c('cat-file', '-s',
      paste0(':', plan$panel)))$output)
  if (length(pointer_size) != 1L || !is.finite(pointer_size) ||
      pointer_size > 1024) stop('Panel was not staged as an LFS pointer')
  pointer = actions_lfs_pointer(git(c('show', paste0(':', plan$panel)))$output)
  if (pointer$oid != plan$oid || pointer$bytes != plan$bytes)
    stop('Staged LFS pointer differs from validated panel')
  git(c('commit', '-m', paste('Conalco validated daily state', plan$date)))
  # Normal fast-forward push; the installed LFS hook uploads the panel first.
  git(c('push', 'origin', 'HEAD:refs/heads/main'))
  cat('GitHub publication complete:', plan$date, '\n')
  invisible('published')
}

# Explicit import of a user-provided durable legacy snapshot; no transport.
conalco_import_state = \(commit_file, archive, sidecar, output,
    required_through, expected_run = NULL, expected_observations = NULL) {
  stopifnot(!file.exists(output))
  commit = read_json(commit_file)
  receipt = read_json(sidecar)
  conalco_safe_paths(commit$daily_root)
  if (commit$date < required_through)
    stop('Latest real daily state is required through ', required_through)
  stopifnot(dedup_hash(archive) == commit$sha256,
    dedup_hash(sidecar) == commit$receipt_sha256,
    receipt$archive$sha256 == commit$sha256,
    identical(basename(commit$archive), paste0(commit$sha256, '.tar.gz')))
  paths = vapply(receipt$files, \(entry) entry$path, '')
  stopifnot(all(startsWith(paths, paste0(commit$daily_root, '/'))))
  pending = tempfile('state_import_', tmpdir = dirname(output))
  dir.create(pending)
  on.exit(unlink(pending, recursive = TRUE), add = TRUE)
  actions_restore(archive, receipt, pending)
  scope = new.env(parent = environment(private_check_panel))
  scope$CONALCO_DAILY_ROOT = commit$daily_root
  check = private_check_panel
  environment(check) = scope
  records = check(pending)
  dates = sort(vapply(records, \(record) record$date, ''))
  stopifnot(identical(dates, sort(unlist(commit$dates))),
    tail(dates, 1L) == commit$date)
  latest = records[[which.max(vapply(records,
    \(record) as.numeric(as.Date(record$date)), numeric(1)))]]
  observations = sum(vapply(records, \(record) record$n_products, numeric(1)))
  if (!is.null(expected_run)) stopifnot(latest$run_id == expected_run)
  if (!is.null(expected_observations))
    stopifnot(observations == expected_observations)
  canonical = file.path(pending, 'canonical')
  conalco_migrate_legacy(file.path(pending, commit$daily_root), canonical)
  panel = file.path(pending, 'verified_panel.csv.gz')
  result = export_conalco_panel(dates[1], tail(dates, 1L), panel, canonical)
  legacy = read_json(file.path(pending, commit$daily_root, 'manifest.json'))
  stopifnot(result$sha256 == legacy$panel$sha256)
  dedup_json(list(kind = 'explicit_local_state_import',
    required_through = required_through, latest_date = commit$date,
    latest_run_id = latest$run_id, observations = observations,
    commit_sha256 = dedup_hash(commit_file),
    archive_sha256 = dedup_hash(archive), sidecar_sha256 = dedup_hash(sidecar),
    panel_sha256 = result$sha256, activated = FALSE),
    file.path(pending, 'import_receipt.json'))
  stopifnot(file.rename(pending, output))
  invisible(output)
}
