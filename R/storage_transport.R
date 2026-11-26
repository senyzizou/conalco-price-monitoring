# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261029

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) COMPLETE LOCAL PACKAGES; NO REMOTE OPERATIONS
# <-----------------------------------------------------------------------------

conalco_safe_paths = \(paths) {
  if (anyNA(paths) || any(!nzchar(paths)) || anyDuplicated(paths) ||
      any(grepl('^/|(^|/)[.][.](/|$)', paths)) ||
      any(grepl('\\', paths, fixed = TRUE))) stop('Unsafe package path')
  invisible(paths)
}

conalco_file_records = \(root, paths) {
  conalco_safe_paths(paths)
  files = file.path(root, paths)
  stopifnot(all(file.exists(files)), !any(dir.exists(files)),
    !any(nzchar(Sys.readlink(files))))
  data.frame(path = paths, bytes = file.size(files),
    sha256 = unname(vapply(files, dedup_hash, '')), stringsAsFactors = FALSE)
}

conalco_verify_files = \(root, records) {
  conalco_safe_paths(records$path)
  for (i in seq_len(nrow(records))) {
    p = file.path(root, records$path[i])
    if (!file.exists(p) || dir.exists(p) || nzchar(Sys.readlink(p)) ||
        file.size(p) != records$bytes[i] ||
        dedup_hash(p) != records$sha256[i])
      stop('Missing or damaged dependency: ', records$path[i])
  }
  invisible(TRUE)
}

conalco_copy_files = \(source, target, paths) {
  conalco_safe_paths(paths)
  for (path in paths) {
    destination = file.path(target, path)
    if (file.exists(destination)) {
      stopifnot(dedup_hash(destination) == dedup_hash(file.path(source, path)))
      next
    }
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(source, path), destination))
  }
  invisible(target)
}

conalco_package = \(root, output, to = '9999-12-31',
    project = getwd(), statistics = NULL) {
  stopifnot(is.character(project), length(project) == 1L,
    dir.exists(project))
  if (dir.exists(output)) stop('Package output already exists')
  history = dedup_history(root)
  dates = names(history)[names(history) <= to]
  if (!length(dates)) stop('No committed dates')
  dedup_restore_rows(root, dates[1], tail(dates, 1L))
  dir.create(output, recursive = TRUE)
  output = normalizePath(output, mustWork = TRUE)
  prefix = conalco_storage_config()$versioned_root
  files = c('schema.json', unlist(lapply(dates, \(date) {
    p = file.path('commits', date)
    file.path(p, list.files(file.path(root, p)))
  }), use.names = FALSE))
  bootstrap = file.path(root, 'bootstrap.json')
  if (file.exists(bootstrap)) {
    b = jsonlite::read_json(bootstrap, simplifyVector = TRUE)
    if (all(unlist(b$dates, use.names = FALSE) %in% dates))
      files = c(files, 'bootstrap.json')
  }
  for (date in dates) {
    receipt = file.path('raw_receipts', paste0(date, '.json'))
    if (file.exists(file.path(root, receipt))) files = c(files, receipt)
    snapshot = file.path('statistics_snapshots', date)
    if (dir.exists(file.path(root, snapshot))) {
      files = c(files, file.path(snapshot,
        list.files(file.path(root, snapshot))))
    }
  }
  conalco_copy_files(root, file.path(output, prefix), files)
  code = c('R/storage.R', 'R/storage_transport.R', 'R/conalco_daily.R',
    'config/conalco_versioned_schema.json', 'renv.lock')
  conalco_copy_files(project, output, code)
  if (is.null(statistics))
    statistics = conalco_versioned_statistics(root, dates)
  if (!is.null(statistics)) {
    path = file.path(output, prefix, 'statistics.csv')
    if (is.character(statistics) && length(statistics) == 1L) {
      stopifnot(file.copy(statistics, path))
    } else write.csv(statistics, path, row.names = FALSE, na = '')
    stopifnot(identical(dedup_read(path),
      conalco_versioned_statistics(root, dates)))
  }
  relative = list.files(output, recursive = TRUE, all.files = TRUE,
    no.. = TRUE, full.names = FALSE)
  relative = relative[!dir.exists(file.path(output, relative))]
  manifest = list(format = 'conalco-versioned-package-1',
    date = tail(dates, 1L), dates = dates, store_root = prefix,
    observations = sum(vapply(history[dates], \(r) r$n_products, integer(1))),
    files = conalco_file_records(output, sort(relative)))
  dedup_json(manifest, file.path(output, 'package_manifest.json'))
  conalco_verify_package(output)
  invisible(manifest)
}

conalco_verify_package = \(root) {
  manifest = jsonlite::read_json(file.path(root, 'package_manifest.json'),
    simplifyVector = TRUE)
  stopifnot(manifest$format == 'conalco-versioned-package-1')
  conalco_safe_paths(manifest$store_root)
  conalco_verify_files(root, manifest$files)
  store = file.path(root, manifest$store_root)
  checked = conalco_versioned_validate(store)
  stopifnot(identical(checked$dates, manifest$dates),
    checked$observations == manifest$observations)
  invisible(manifest)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) INITIAL BACKUP AND IMMUTABLE DAILY INCREMENTS
# <-----------------------------------------------------------------------------

conalco_pack_increment = \(root, output, date,
    previous = NULL, project = getwd(), statistics = NULL) {
  if (dir.exists(output)) stop('Archive output already exists')
  history = dedup_history(root)
  stopifnot(date %in% names(history))
  dates = names(history)[names(history) <= date]
  if (is.null(previous)) {
    previous_dates = character()
  } else {
    previous_dates = unlist(previous$dates, use.names = FALSE)
    stopifnot(length(previous_dates) < length(dates),
      identical(previous_dates, head(dates, length(previous_dates))))
  }
  dir.create(output, recursive = TRUE)
  output = normalizePath(output, mustWork = TRUE)
  package = file.path(output, 'assembled')
  conalco_package(root, package, to = date, project = project,
    statistics = statistics)
  manifest = jsonlite::read_json(file.path(package, 'package_manifest.json'),
    simplifyVector = TRUE)
  files = manifest$files$path
  if (length(previous_dates)) {
    old = previous$state_files
    if (!is.data.frame(old)) old = do.call(rbind, lapply(old, as.data.frame))
    at = match(old$path, manifest$files$path)
    stopifnot(!anyNA(at))
    stopifnot(all(manifest$files$sha256[at] == old$sha256))
    files = setdiff(files, old$path)
  }
  aggregate_stat = file.path(manifest$store_root, 'statistics.csv')
  stat = files[files == aggregate_stat]
  for (path in stat) {
    dated = sub('statistics[.]csv$', paste0('statistics_', date, '.csv'), path)
    stopifnot(file.copy(file.path(package, path), file.path(package, dated)))
    files = c(setdiff(files, path), dated)
  }
  stat_files = files[grepl('/statistics_[0-9-]+[.]csv$', files)]
  checkpoint = list(format = 'conalco-result-increment-1', date = date,
    dates = dates, store_root = manifest$store_root,
    observations = manifest$observations,
    state_files = manifest$files[manifest$files$path != aggregate_stat, ],
    statistics_files = if (length(stat_files))
      conalco_file_records(package, stat_files) else NULL,
    previous_archive_sha256 = if (is.null(previous)) 'GENESIS' else
      previous$archive_sha256,
    dependencies = if (is.null(previous)) list() else c(previous$dependencies,
      list(list(archive_sha256 = previous$archive_sha256,
        receipt_sha256 = previous$receipt_sha256))))
  # The current complete manifest travels as an immutable dated checkpoint.
  path = file.path('checkpoints', paste0(date, '.json'))
  dir.create(file.path(package, 'checkpoints'))
  dedup_json(checkpoint, file.path(package, path))
  files = c(files, path)
  archive = file.path(output, paste0(date, '.tar.gz'))
  packed = storage_pack_files(package, sort(files), archive)
  receipt = list(date = date, archive_sha256 = packed$archive_sha256,
    files = packed$files)
  sidecar = file.path(output, paste0(date, '.json'))
  dedup_json(receipt, sidecar)
  checkpoint$archive_sha256 = receipt$archive_sha256
  checkpoint$receipt_sha256 = dedup_hash(sidecar)
  checkpoint$archive = basename(archive)
  checkpoint$receipt = basename(sidecar)
  list(archive = archive, sidecar = sidecar, checkpoint = checkpoint)
}

conalco_restore_increments = \(archives, receipts, output, expected = NULL) {
  stopifnot(length(archives) == length(receipts), length(archives) > 0L)
  if (dir.exists(output)) stop('Restore output already exists')
  pending = tempfile('restore_', tmpdir = dirname(output))
  dir.create(pending)
  on.exit(unlink(pending, recursive = TRUE), add = TRUE)
  previous = 'GENESIS'
  for (i in seq_along(archives)) {
    receipt = jsonlite::read_json(receipts[i], simplifyVector = TRUE)
    stopifnot(dedup_hash(archives[i]) == receipt$archive_sha256)
    paths = receipt$files$path
    conalco_safe_paths(paths)
    # Existing ustar parser rejects links, traversal and undeclared members.
    members = storage_archive_members(archives[i], roots = character(),
      allowed_paths = paths)
    stopifnot(setequal(members, paths),
      !any(file.exists(file.path(pending, paths))))
    untar(archives[i], exdir = pending, tar = 'internal')
    conalco_verify_files(pending, receipt$files)
    checkpoint = jsonlite::read_json(file.path(pending, 'checkpoints',
      paste0(receipt$date, '.json')), simplifyVector = TRUE)
    stopifnot(checkpoint$format == 'conalco-result-increment-1')
    conalco_safe_paths(checkpoint$store_root)
    stopifnot(checkpoint$previous_archive_sha256 == previous)
    conalco_verify_files(pending, checkpoint$state_files)
    if (!is.null(checkpoint$statistics_files))
      conalco_verify_files(pending, checkpoint$statistics_files)
    if (i > 1L) {
      dependencies = checkpoint$dependencies
      stopifnot(nrow(dependencies) == i - 1L,
        identical(unname(vapply(archives[seq_len(i - 1L)], dedup_hash, '')),
          dependencies$archive_sha256),
        identical(unname(vapply(receipts[seq_len(i - 1L)], dedup_hash, '')),
          dependencies$receipt_sha256))
    }
    previous = receipt$archive_sha256
    store = file.path(pending, checkpoint$store_root)
    checked = conalco_versioned_validate(store)
    stopifnot(identical(checked$dates, checkpoint$dates),
      checked$observations == checkpoint$observations)
  }
  if (!is.null(expected)) {
    stopifnot(identical(checkpoint$dates, unlist(expected$dates,
      use.names = FALSE)),
      identical(previous, expected$archive_sha256),
      dedup_hash(tail(receipts, 1L)) == expected$receipt_sha256)
  }
  # A failed restore never becomes the requested visible destination.
  if (!file.rename(pending, output)) stop('Cannot activate restored state')
  invisible(checkpoint)
}

# Ustar parser from conalco_actions.R; original 261021, adapted 261026.
storage_archive_members = \(path, roots, allowed_paths = NULL) {
  connection = if (endsWith(path, '.gz')) gzfile(path, 'rb') else
    xzfile(path, 'rb')
  on.exit(close(connection))
  field = \(bytes) {
    end = match(as.raw(0), bytes, nomatch = length(bytes) + 1L)
    if (end <= length(bytes) && any(bytes[end:length(bytes)] != as.raw(0)))
      stop("Invalid tar text field")
    rawToChar(bytes[seq_len(end - 1L)])
  }
  octal = \(bytes) {
    end = match(TRUE, !bytes %in% as.raw(48:55),
      nomatch = length(bytes) + 1L)
    if (end <= length(bytes) &&
        any(!bytes[end:length(bytes)] %in% as.raw(c(0, 32))))
      stop("Invalid tar octal field")
    value = rawToChar(bytes[seq_len(end - 1L)])
    if (!grepl("^[0-7]+$", value)) stop("Invalid tar octal field")
    value = strtoi(value, base = 8L)
    if (is.na(value) || value < 0) stop("Unsupported tar size or checksum")
    value
  }
  members = character()
  repeat {
    header = readBin(connection, "raw", 512L)
    if (length(header) != 512L) stop("Truncated tar header")
    if (all(header == as.raw(0))) {
      end = readBin(connection, "raw", 512L)
      if (length(end) != 512L || any(end != as.raw(0)))
        stop("Invalid tar end marker")
      repeat {
        tail = readBin(connection, "raw", 1048576L)
        if (any(tail != as.raw(0))) stop("Unexpected trailing tar content")
        if (!length(tail)) break
      }
      break
    }
    expected = octal(header[149:156])
    checked = header
    checked[149:156] = as.raw(32)
    if (sum(as.integer(checked)) != expected) stop("Invalid tar checksum")
    magic = c(charToRaw("ustar"), as.raw(0), charToRaw("00"))
    if (!identical(header[258:265], magic))
      stop("Unsupported tar format")
    if (!header[157] %in% as.raw(c(0, 48)) ||
        any(header[158:257] != as.raw(0))) stop("Non-regular tar entry")
    name = field(header[1:100])
    prefix = field(header[346:500])
    if (nzchar(prefix)) name = paste(prefix, name, sep = "/")
    conalco_safe_paths(name)
    if (is.null(allowed_paths)) {
      if (!any(startsWith(name, paste0(roots, "/"))))
        stop("Unexpected archive destination: ", name)
    } else if (!name %in% allowed_paths) {
      stop("Unexpected archive destination: ", name)
    }
    members = c(members, name)
    size = octal(header[125:136])
    remaining = ceiling(size / 512) * 512
    while (remaining > 0) {
      count = min(remaining, 1048576L)
      body = readBin(connection, "raw", count)
      if (length(body) != count) stop("Truncated tar body")
      remaining = remaining - count
    }
  }
  conalco_safe_paths(members)
  listed = untar(path, list = TRUE, tar = "internal")
  if (!identical(listed, members)) stop("Tar path interpretation mismatch")
  members
}



# Origin: storage_transport.R, 261029; consolidated 261030.
# 3) EXPLICIT MANUAL PUBLICATION; TRANSPORT IS INJECTABLE
# <-----------------------------------------------------------------------------

conalco_manual_gate = \(mode, event, offline = FALSE) {
  scheduled = identical(event, 'schedule') &&
    identical(Sys.getenv('GITHUB_ACTIONS'), 'true') &&
    identical(Sys.getenv('GITHUB_REF'), 'refs/heads/main') &&
    identical(Sys.getenv('GITHUB_REPOSITORY'),
      'senyzizou/conalco-price-monitoring')
  permitted = identical(event, 'workflow_dispatch') || scheduled ||
    (identical(event, 'local_test') && offline)
  if (mode != 'publish' || !permitted)
    stop('Versioned publication requires explicit manual mode=publish')
  invisible(T)
}

conalco_prepare_publication = \(root, output, mode, event, offline = FALSE) {
  conalco_manual_gate(mode, event, offline)
  manifest = conalco_package(root, output)
  files = c(manifest$files$path, 'package_manifest.json')
  conalco_publication_security(output, files)
  # Existing Git data, LFS pointers, attributes and usage ledgers stay intact.
  stopifnot(!any(files %in% c('.gitattributes',
    'config/conalco_lfs_usage.json')),
    !any(grepl('panel[.]csv[.]gz$', files)))
  list(mode = mode, event = event, date = manifest$date, output = output,
    package_sha256 = dedup_hash(file.path(output, 'package_manifest.json')),
    files = conalco_file_records(output, files))
}

conalco_commit_publication = \(plan, git, mode, event, offline = FALSE) {
  conalco_manual_gate(mode, event, offline)
  if (!is.data.frame(plan$files))
    plan$files = do.call(rbind, lapply(plan$files, as.data.frame))
  conalco_verify_package(plan$output)
  stopifnot(dedup_hash(file.path(plan$output, 'package_manifest.json')) ==
    plan$package_sha256)
  conalco_verify_files(plan$output, plan$files)
  files = plan$files$path
  conalco_publication_security(plan$output, files)
  manifest = jsonlite::read_json(file.path(plan$output,
    'package_manifest.json'), simplifyVector = TRUE)
  if (file.exists(file.path(conalco_versioned_root(), 'schema.json'))) {
    existing_dates = names(dedup_history(conalco_versioned_root()))
    if (any(!existing_dates %in% manifest$dates))
      stop('Publication source omits existing repository history')
  }
  if (any(file.exists(files))) {
    existing = files[file.exists(files)]
    at = match(existing, files)
    # Only a derived package/statistics manifest can change on publication.
    mutable = existing %in% c(file.path(manifest$store_root,
      'statistics.csv'), 'package_manifest.json')
    stopifnot(all(vapply(existing[!mutable], dedup_hash, '') ==
      plan$files$sha256[at[!mutable]]))
  }
  base = git(c('rev-parse', 'HEAD'))$output
  git(c('fetch', '--no-tags', 'origin', 'main'))
  stopifnot(identical(git(c('rev-parse', 'origin/main'))$output, base))
  if (length(git(c('diff', '--cached', '--name-only'))$output))
    stop('Unexpected staged changes')
  attributes = git(c('check-attr', 'filter', '--', files))$output
  allowed = c(paste0(files, ': filter: unspecified'),
    paste0(files, ': filter: unset'))
  if (length(attributes) != length(files) || any(!attributes %in% allowed))
    stop('Every new payload path must be verified without a Git filter')
  for (path in files) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(plan$output, path), path,
      overwrite = path %in% c(file.path(manifest$store_root,
        'statistics.csv'), 'package_manifest.json')) ||
      dedup_hash(path) == dedup_hash(file.path(plan$output, path)))
  }
  git(c('add', '--', files))
  staged = git(c('diff', '--cached', '--name-only'))$output
  if (any(!staged %in% files)) stop('Unexpected staged path')
  if (!length(staged)) return(invisible('identical'))
  git(c('commit', '-m', paste('Conalco versioned state', plan$date)))
  git(c('push', 'origin', 'HEAD:refs/heads/main'))
  invisible('published')
}

conalco_publication_security = \(root, paths) {
  conalco_safe_paths(paths)
  prefix = conalco_storage_config()$versioned_root
  code = c('R/storage.R', 'R/storage_transport.R', 'R/conalco_daily.R',
    'config/conalco_versioned_schema.json', 'renv.lock',
    'package_manifest.json')
  dated = '[0-9]{4}-[0-9]{2}-[0-9]{2}'
  names = c('manifest[.]json', 'origin_receipt[.]json',
    '(observations|product_versions|product_changes)[.]csv[.]gz',
    'statistics[.]csv')
  pattern = paste0('^(schema[.]json|bootstrap[.]json|statistics[.]csv|',
    'commits/', dated, '/(', paste(names, collapse = '|'), ')|',
    'raw_receipts/', dated, '[.]json|statistics_snapshots/', dated,
    '/(manifest[.]json|statistics[.]csv))$')
  relative = substring(paths, nchar(prefix) + 2L)
  allowed = paths %in% code | (startsWith(paths, paste0(prefix, '/')) &
    grepl(pattern, relative))
  if (any(!allowed)) stop('Unexpected public publication path')
  if (any(file.size(file.path(root, paths)) > 100 * 1024^2))
    stop('Public Git file exceeds 100 MiB')
  for (path in paths) {
    connection = if (endsWith(path, '.gz'))
      gzfile(file.path(root, path), 'rt') else
      file(file.path(root, path), 'rt')
    text = paste(readLines(connection, warn = F), collapse = '\n')
    close(connection)
    patterns = c(
      '\\bgh[pousr]_[A-Za-z0-9]{20,}',
      '\\bgithub_pat_[A-Za-z0-9_]{30,}',
      '\\bsk-(proj-)?[A-Za-z0-9_-]{32,}',
      '\\b(AKIA|ASIA)[A-Z0-9]{16}\\b',
      'https?://[^ /:]+:[^ /@]+@',
      paste0('-----BEGIN (RSA |EC |OPENSSH )?', 'PRIVATE KEY-----'),
      'session-[A-Za-z0-9_-]*=[A-Za-z0-9._~-]{12,}',
      paste0('/', 'Users/', '[^/[:space:]]+/'),
      paste0('"(password|app_password|access_token|refresh_token|',
        'client_secret)"[[:space:]]*:[[:space:]]*"[^"[:space:]]+"'))
    if (any(vapply(patterns, \(pattern)
        grepl(pattern, text, perl = T), logical(1))))
      stop('Sensitive content in public publication file: ', path)
  }
  invisible(T)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) OFFLINE-TESTABLE INCREMENTAL RESULT ARCHIVES
# <-----------------------------------------------------------------------------

conalco_increment_base = \() conalco_storage_config()$ukebox_base

conalco_publish_increment = \(root, directory, previous, publish,
    make_directory = \(path) invisible(path), project = getwd()) {
  dates = names(dedup_history(root))
  date = tail(dates, 1L)
  if (!is.null(previous) && previous$date == date) {
    stopifnot(identical(unlist(previous$dates, use.names = FALSE), dates))
    records = previous$state_files
    if (!is.data.frame(records))
      records = do.call(rbind, lapply(records, as.data.frame))
    path = file.path(conalco_storage_config()$versioned_root,
      'commits', date, 'manifest.json')
    at = match(path, records$path)
    stopifnot(!is.na(at), dedup_hash(file.path(root, 'commits', date,
      'manifest.json')) == records$sha256[at])
    return(invisible(previous))
  }
  if (dir.exists(directory)) {
    directory = tempfile('retry_', tmpdir = directory)
  }
  code = c('R/storage.R', 'R/storage_transport.R', 'R/conalco_daily.R',
    'config/conalco_versioned_schema.json', 'renv.lock')
  if (!is.null(previous)) {
    records = previous$state_files
    if (!is.data.frame(records))
      records = do.call(rbind, lapply(records, as.data.frame))
    selected = records[match(code, records$path), ]
    stopifnot(!anyNA(selected$path))
    conalco_verify_files(project, selected)
  }
  packed = conalco_pack_increment(root, directory, date, previous,
    project = project)
  checkpoint = packed$checkpoint
  base = conalco_increment_base()
  object = paste0(base, '/objects/', checkpoint$archive_sha256, '.tar.gz')
  sidecar = paste0(object, '.json')
  make_directory(paste0(base, '/objects'))
  make_directory(paste0(base, '/commits'))
  publish(packed$archive, object)
  publish(packed$sidecar, sidecar)
  checkpoint$object_path = object
  checkpoint$receipt_path = sidecar
  previous_objects = if (is.null(previous)) list() else previous$objects
  checkpoint$objects = c(previous_objects, list(list(path = object,
    sha256 = checkpoint$archive_sha256, receipt = sidecar,
    receipt_sha256 = checkpoint$receipt_sha256)))
  commit_file = file.path(directory, paste0(date, '_commit.json'))
  dedup_json(checkpoint, commit_file)
  # The final pointer is published last; incomplete uploads are invisible.
  publish(commit_file, paste0(base, '/commits/', date, '.json'))
  invisible(checkpoint)
}

conalco_fetch_increment = \(commit, target, fetch) {
  scratch = tempfile('increment_fetch_', tmpdir = dirname(target))
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  archives = character(length(commit$objects))
  receipts = character(length(commit$objects))
  for (i in seq_along(commit$objects)) {
    entry = commit$objects[[i]]
    expected = paste0(conalco_increment_base(), '/objects/', entry$sha256,
      '.tar.gz')
    stopifnot(entry$path == expected,
      entry$receipt == paste0(expected, '.json'))
    archives[i] = file.path(scratch, paste0(i, '.tar.gz'))
    receipts[i] = file.path(scratch, paste0(i, '.json'))
    stopifnot(fetch(entry$path, archives[i]),
      fetch(entry$receipt, receipts[i]),
      dedup_hash(archives[i]) == entry$sha256,
      dedup_hash(receipts[i]) == entry$receipt_sha256)
  }
  conalco_restore_increments(archives, receipts, target, expected = commit)
}

conalco_increment_restore = \(dav, target) {
  scope = new.env(parent = environment(actions_state_list))
  scope$actions_state_base = conalco_increment_base
  list_commits = actions_state_list
  environment(list_commits) = scope
  dates = actions_phase('state.list_commits', list_commits(dav))
  if (!length(dates)) stop('Explicit incremental archive seed required')
  path = paste0(conalco_increment_base(), '/commits/', tail(dates, 1L))
  commit_file = tempfile('increment_commit_')
  on.exit(unlink(commit_file), add = TRUE)
  actions_phase('state.read_commit',
    stopifnot(actions_dav_read(dav, path, commit_file)))
  commit = jsonlite::read_json(commit_file)
  destination = file.path(target, 'versioned_restore')
  actions_phase('state.increment_download_validate',
    conalco_fetch_increment(commit, destination,
      \(path, output) actions_dav_read(dav, path, output)))
  store = file.path(destination, commit$store_root)
  actions_phase('state.legacy_view_validate',
    conalco_legacy_view(store, file.path(target, 'legacy_view'),
      daily_root = CONALCO_DAILY_ROOT))
  actions_phase('state.copy_validated_view',
    conalco_copy_files(file.path(target, 'legacy_view'), target,
      list.files(file.path(target, 'legacy_view'), recursive = T)))
  paths = list.files(file.path(target, CONALCO_DAILY_ROOT, 'runs'),
    '^manifest[.]json$', recursive = TRUE, full.names = TRUE)
  list(commit = commit, records = lapply(paths, read_json),
    versioned_store = store, package_project = destination)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) LEGACY RAW ARCHIVES AND CONTROLLED RESULT-BACKEND TRANSITION
# <-----------------------------------------------------------------------------

conalco_raw_bridge = \(manifest_path) {
  manifest = require_complete_catalog(manifest_path)
  root = conalco_versioned_root()
  date = names(dedup_history(root))[vapply(dedup_history(root),
    \(r) r$run_id == manifest$run_id, logical(1))]
  directory = file.path('tmp', 'conalco_raw_bridge', manifest$run_id)
  if (!dir.exists(directory)) {
    conalco_legacy_view(root, directory, dates = date, panel = FALSE,
      daily_root = CONALCO_DAILY_ROOT)
  }
  daily = file.path(directory, CONALCO_DAILY_ROOT)
  path = file.path(daily, 'runs', manifest$run_id, 'manifest.json')
  record = read_json(path)
  record$observations$path = file.path(daily, 'runs', manifest$run_id,
    'observations.csv.gz')
  write_json(record, path, auto_unbox = TRUE, pretty = TRUE, na = 'null')
  daily
}

conalco_legacy_state_publish = \(dav, history, directory) {
  actions_history_unchanged(history)
  workspace = tempfile('legacy_state_', tmpdir = dirname(directory))
  conalco_legacy_view(conalco_versioned_root(), workspace,
    daily_root = CONALCO_DAILY_ROOT)
  conalco_attach_legacy_bridge(conalco_versioned_root(), workspace)
  output = file.path(normalizePath(dirname(directory)), basename(directory))
  old = getwd()
  old_option = getOption('apm.conalco.storage')
  on.exit({
    setwd(old)
    options(apm.conalco.storage = old_option)
  }, add = TRUE)
  options(apm.conalco.storage = list(local_storage = 'legacy',
    github_publication = 'legacy', ukebox_results = 'legacy'))
  setwd(workspace)
  actions_state_publish(dav, actions_history(), output)
}

conalco_attach_legacy_bridge = \(root, workspace) {
  conalco_check_bootstrap(root)
  target = file.path(workspace, CONALCO_DAILY_ROOT, 'versioned_bridge')
  files = list.files(root, recursive = TRUE)
  files = files[!grepl('^transactions/|^[.]writer_lock/', files)]
  conalco_copy_files(root, target, files)
  conalco_versioned_validate(target)
  invisible(target)
}

conalco_merge_store = \(source, target) {
  conalco_versioned_validate(source)
  if (!dir.exists(target)) {
    dir.create(target, recursive = TRUE)
    conalco_copy_files(source, target, list.files(source, recursive = TRUE))
    dir.create(file.path(target, 'transactions'), showWarnings = FALSE)
    return(invisible(target))
  }
  stopifnot(dedup_hash(file.path(source, 'schema.json')) ==
    dedup_hash(file.path(target, 'schema.json')))
  old = dedup_history(target)
  new = dedup_history(source)
  common = min(length(old), length(new))
  stopifnot(identical(head(names(old), common), head(names(new), common)))
  for (date in head(names(old), common)) {
    stopifnot(dedup_hash(file.path(source, 'commits', date, 'manifest.json')) ==
      dedup_hash(file.path(target, 'commits', date, 'manifest.json')))
  }
  for (date in setdiff(names(new), names(old))) {
    pending = tempfile('restored_', tmpdir = file.path(target, 'transactions'))
    dir.create(pending)
    origin = file.path(source, 'commits', date)
    conalco_copy_files(origin, pending, list.files(origin))
    if (!file.rename(pending, file.path(target, 'commits', date)))
      stop('Cannot activate restored commit')
  }
  snapshots = list.files(file.path(source, 'statistics_snapshots'),
    recursive = TRUE)
  if (length(snapshots)) {
    conalco_copy_files(source, target,
      file.path('statistics_snapshots', snapshots))
  }
  if (file.exists(file.path(source, 'bootstrap.json')) &&
      !file.exists(file.path(target, 'bootstrap.json'))) {
    conalco_copy_files(source, target, 'bootstrap.json')
  }
  receipts = list.files(file.path(source, 'raw_receipts'), full.names = TRUE)
  for (path in receipts) {
    conalco_record_raw_receipt(target, sub('[.]json$', '', basename(path)),
      jsonlite::read_json(path))
  }
  invisible(target)
}

conalco_actions_prepare_versioned = \(dav) {
  mode = Sys.getenv('APM_PUBLICATION_MODE')
  event = Sys.getenv('GITHUB_EVENT_NAME')
  conalco_manual_gate(mode, event)
  dir.create('tmp', showWarnings = FALSE)
  scratch = tempfile('versioned_publication_', tmpdir = 'tmp')
  dir.create(scratch)
  restored = actions_state_restore(dav, scratch)
  if (!is.null(restored$versioned_store)) {
    root = restored$versioned_store
  } else {
    root = file.path(scratch, 'canonical')
    existing = conalco_versioned_root()
    if (dir.exists(existing)) conalco_merge_store(existing, root)
    conalco_migrate_legacy(file.path(scratch, CONALCO_DAILY_ROOT), root)
  }
  records = conalco_versioned_records(root)
  latest = tail(records, 1L)[[1]]
  if (is.null(latest$raw_archive) ||
      !grepl('^[0-9a-f]{64}$', latest$raw_archive$sha256) ||
      !grepl('^[0-9a-f]{64}$', latest$raw_archive$manifest_sha256))
    stop('Latest run needs a verified durable raw archive receipt')
  payload = file.path(scratch, 'payload')
  plan = conalco_prepare_publication(root, payload, mode, event)
  directory = 'tmp/actions_publication'
  dir.create(directory, showWarnings = FALSE)
  daily_replace(file.path(directory, 'versioned_plan.json'), \(path) {
    jsonlite::write_json(plan, path, auto_unbox = TRUE, pretty = TRUE,
      na = 'null')
  })
  invisible(plan)
}

conalco_legacy_publication_view = \(dav, target, restored) {
  # Legacy publication must retain original published shard bytes.
  stopifnot(normalizePath(target) != normalizePath(getwd()))
  root = restored$versioned_store
  history = dedup_history(root)
  records = vector('list', length(history))
  for (i in seq_along(history)) {
    date = names(history)[i]
    id = history[[i]]$run_id
    canonical = file.path(CONALCO_DAILY_ROOT, 'runs', id)
    original_manifest = file.path(canonical, 'manifest.json')
    original_shard = file.path(canonical, 'observations.csv.gz')
    if (file.exists(original_manifest) && file.exists(original_shard)) {
      record = read_json(original_manifest)
      stopifnot(dedup_hash(original_shard) == record$observations$sha256)
      source = original_shard
      if (!is.null(record$source_export_manifest))
        record = record$source_export_manifest
    } else {
      entry = conalco_versioned_records(root)[[i]]
      raw = entry$raw_archive
      if (is.null(raw)) stop('Legacy publication needs original shard: ', id)
      directory = tempfile('legacy_raw_')
      dir.create(directory)
      archive = file.path(directory, paste0(id, '.tar.gz'))
      sidecar = paste0(archive, '.json')
      stopifnot(actions_dav_read(dav, raw$path, archive),
        actions_dav_read(dav, paste0(raw$path, '.json'), sidecar),
        dedup_hash(archive) == raw$sha256,
        dedup_hash(sidecar) == raw$manifest_sha256)
      receipt = read_json(sidecar)
      record = actions_verify_archive(archive, receipt)
      extraction = file.path(directory, 'extracted')
      dir.create(extraction)
      actions_restore(archive, receipt, extraction)
      conalco_safe_paths(record$observations$path)
      source = file.path(extraction, record$observations$path)
    }
    x = dedup_read(source)[dedup_schema(root)$columns]
    x = x[order(x$retailer, x$retailer_product_id), ]
    rownames(x) = NULL
    stopifnot(identical(x, dedup_restore_rows(root, date, date)))
    destination = file.path(target, canonical)
    stopifnot(file.copy(source, file.path(destination, 'observations.csv.gz'),
      overwrite = TRUE))
    record$observations = list(path = file.path(canonical,
      'observations.csv.gz'), sha256 = dedup_hash(source))
    if (is.null(record$raw_archive))
      record$raw_archive = conalco_versioned_records(root)[[i]]$raw_archive
    jsonlite::write_json(record, file.path(destination, 'manifest.json'),
      auto_unbox = TRUE, pretty = TRUE, null = 'null', na = 'null')
    records[[i]] = record
  }
  restored$records = records
  restored
}

# Compatibility name for already prepared offline callers.
conalco_archive_members = storage_archive_members

# Shared offline archive primitive; payload selection stays in adapters.
storage_pack_files = \(root, paths, archive) {
  conalco_safe_paths(paths)
  stopifnot(!file.exists(archive))
  records = conalco_file_records(root, paths)
  archive = file.path(normalizePath(dirname(archive)), basename(archive))
  old = getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)
  tar(archive, files = sort(paths), compression = 'gzip',
    tar = Sys.which('tar'), extra_flags = '--format=ustar')
  list(archive_sha256 = dedup_hash(archive), files = records)
}

storage_restore_files = \(archive, receipt, output) {
  stopifnot(!file.exists(output), dedup_hash(archive) == receipt$archive_sha256)
  records = receipt$files
  if (!is.data.frame(records))
    records = do.call(rbind, lapply(records, as.data.frame))
  paths = records$path
  storage_archive_members(archive, roots = character(), allowed_paths = paths)
  pending = tempfile('restore_files_', tmpdir = dirname(output))
  dir.create(pending)
  on.exit(unlink(pending, recursive = TRUE), add = TRUE)
  untar(archive, exdir = pending, tar = 'internal')
  conalco_verify_files(pending, records)
  stopifnot(file.rename(pending, output))
  invisible(output)
}

# Existing release safety helpers; original 261026.
release_guard_paths = \(root, paths) {
  archive_safe_paths(paths)
  for (relative in paths) {
    path = file.path(root, relative)
    repeat {
      link = Sys.readlink(path)
      if (!is.na(link) && nzchar(link)) stop("Symlink in release input")
      parent = dirname(path)
      if (identical(parent, path)) break
      path = parent
    }
  }
  invisible(paths)
}

release_check_files = \(paths, bytes, hashes) {
  if (any(!file.exists(paths) | dir.exists(paths)))
    stop("Missing release file")
  if (anyNA(bytes) || any(file.size(paths) != bytes))
    stop("Release file size mismatch")
  for (i in seq_along(paths)) {
    if (!identical(archive_sha256(paths[i]), hashes[i]))
      stop("Release SHA-256 mismatch: ", basename(paths[i]))
  }
  invisible(paths)
}

release_rscript = \(...) {
  status = system2(file.path(R.home("bin"), "Rscript"), shQuote(c(...)),
    env = c("RENV_CONFIG_AUTOLOADER_ENABLED=false", paste0("R_LIBS=",
        shQuote(paste(.libPaths(), collapse = .Platform$path.sep)))))
  if (status != 0) stop("Offline replay failed (exit ", status, ")")
}

