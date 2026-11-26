# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261007

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) STORAGE
# <-----------------------------------------------------------------------------

run_dir = \(run_id, base_dir = "data/raw/conalco") {
  if (length(run_id) != 1L || is.na(run_id) || !grepl("^[A-Za-z0-9_-]+$",
      run_id))
    stop("Invalid run_id")
  file.path(base_dir, run_id)
}

observation_path = \(run_id, parser_version = PARSER_VERSION) {
  file.path("data/processed/runs", run_id, paste0("observations_",
      parser_version, ".csv"))
}

collect_observations = \(parser_version = PARSER_VERSION, exclude_run = NULL) {
  paths = list.files("data/processed/runs", pattern = paste0(
      "^observations_", gsub("[.]",
        "\\.", parser_version), "[.]csv$"), recursive = TRUE, full.names = TRUE)
  if (!length(paths))
    return(data.frame())
  tables = lapply(paths, \(path) {
      id = basename(dirname(path))
      manifest_path = file.path(run_dir(id), "manifest.json")
      # A CSV published just before an unexpected interruption is not a
      # finalized
      # run. Keep it for recovery, but never silently call it an analysis
      # source.
      if (!file.exists(manifest_path))
        return(NULL)
      manifest = read_json(manifest_path, simplifyVector = FALSE)
      if (!identical(manifest$purpose, "pilot") || isTRUE(manifest$aborted))
        return(NULL)
      if (identical(manifest$parser_version, parser_version)) {
        expected = scalar(manifest$observation_sha256)
        if (is.na(expected) || !identical(digest(file = path, algo =
                "sha256"),
            expected))
          stop("Observation hash mismatch: ", path)
      }
      else {
        sidecar = paste0(path, ".json")
        if (!file.exists(sidecar))
          stop("Missing derivation provenance: ", path)
        derivation = read_json(sidecar, simplifyVector = TRUE)
        if (!identical(digest(file = path, algo = "sha256"),
            derivation$observation_sha256))
          stop("Derivation hash mismatch: ", path)
      }
      read_observations(path)
  })
  tables = Filter(Negate(is.null), tables)
  if (!length(tables))
    return(data.frame())
  observations = do.call(rbind, tables)
  if (!is.null(exclude_run))
  observations = observations[!observations$run_id %in% exclude_run, ,
    drop = FALSE]
  observations[order(observations$observed_at, observations$run_id,
      observations$requested_product_id), ,
    drop = FALSE]
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) IDEMPOTENT OBSERVATION STORAGE
# <-----------------------------------------------------------------------------

save_run_observations = \(x, run_id, parser_version = PARSER_VERSION) {
  if (nrow(x) && any(x$run_id != run_id))
    stop("Observation/run identity mismatch")
  path = observation_path(run_id, parser_version)
  if (file.exists(path)) {
    old = read_observations(path)
    old$validation_flags[is.na(old$validation_flags)] = ""
    x$validation_flags[is.na(x$validation_flags)] = ""
    if (!isTRUE(all.equal(old, x, tolerance = 1e-10, check.attributes = FALSE)))
      stop(paste0("Different observations under existing run/",
          "parser key; refusing overwrite"))
    return(list(path = path, written = FALSE))
  }
  write_csv_once(x, path)
  write_json_once(list(run_id = run_id, parser_version = parser_version,
      derived_at_utc = utc_now(),
      observation_sha256 = digest(file = path, algo = "sha256"),
      n_observations = nrow(x)),
    paste0(path, ".json"))
  list(path = path, written = TRUE)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) HISTORICAL PILOT PANEL
# <-----------------------------------------------------------------------------

build_analysis_export = \(parser_version = PARSER_VERSION) {
  observations = collect_observations(parser_version)
  if (!nrow(observations))
    stop("No immutable run observations to export")
  key = paste(observations$run_id, observations$requested_product_id,
    observations$parser_version, sep = "|")
  if (anyDuplicated(key))
    stop("Duplicate run/product/parser key in source observations")
  status = setNames(vapply(unique(observations$run_id), \(id) scalar(
        read_json(file.path(run_dir(id),
            "manifest.json"))$status), character(1)), unique(
      observations$run_id))
  observations$run_status = unname(status[observations$run_id])
  content_hash = substr(digest(observations, algo = "sha256"), 1, 16)
  path = file.path("data/processed/exports", paste0("panel_", parser_version,
      "_", content_hash,
      ".csv"))
  if (!file.exists(path))
    write_csv_once(observations, path)
  list(path = path, n_observations = nrow(observations), n_runs = length(
      unique(observations$run_id)))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) STORED-RUN PARSING AND REPLAY
# <-----------------------------------------------------------------------------

reparse_run = \(run_id) {
  manifest = read_json(file.path(run_dir(run_id), "manifest.json"),
    simplifyVector = FALSE)
  if (!identical(manifest$purpose, "pilot"))
    stop("Not a completed pilot manifest")
  path = observation_path(run_id)
  if (file.exists(path))
    return(list(path = path, written = FALSE, reason =
          "already parsed with this parser version"))
  rows = lapply(manifest$product_responses, \(metadata) parse_conalco(
      metadata$metadata_file,
      scalar(metadata$retailer_product_id), scalar(metadata$beverage_group)))
  observations = do.call(rbind, rows)
  previous = collect_observations(exclude_run = run_id)
  if (nrow(previous))
  previous = previous[previous$observed_at < manifest$started_at_utc, ,
    drop = FALSE]
  observations = validate_observations(observations, previous = previous,
    planned = manifest$n_planned_products)
  # New parser versions create new derivations; original run manifests never
  # change.
  saved = save_run_observations(observations, run_id)
  saved
}

replay_run = \(run_id) {
  manifest = read_json(file.path(run_dir(run_id), "manifest.json"),
    simplifyVector = FALSE)
  if (!identical(manifest$purpose, "pilot") || isTRUE(manifest$aborted))
    stop("Not a finalized pilot run")
  if (!identical(manifest$parser_version, PARSER_VERSION))
    stop("Replay equality requires the original parser version")
  rows = lapply(manifest$product_responses, \(metadata) parse_conalco(
      metadata$metadata_file,
      scalar(metadata$retailer_product_id), scalar(metadata$beverage_group)))
  observations = do.call(rbind, rows)
  previous = collect_observations(exclude_run = run_id)
  if (nrow(previous))
  previous = previous[previous$observed_at < manifest$started_at_utc, ,
    drop = FALSE]
  observations = validate_observations(observations, previous = previous,
    planned = manifest$n_planned_products)
  stored = read_observations(manifest$observation_file)
  stored$validation_flags[is.na(stored$validation_flags)] = ""
  comparison = all.equal(observations, stored, tolerance = 1e-10,
    check.attributes = FALSE)
  if (!isTRUE(comparison))
    stop("Offline replay differs: ", paste(comparison, collapse = "; "))
  list(run_id = run_id, n_observations = nrow(observations),
    identical_with_numeric_tolerance = TRUE,
    original_csv_sha256 = digest(file = manifest$observation_file, algo =
        "sha256"))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) ARCHIVE
# <-----------------------------------------------------------------------------

# Merged on 2026-10-19 from R/archive.R (file created 2026-10-07); content
# unchanged
archive_response = \(response, run_id, request_type, request_index,
  product_id = NA_character_, base_dir = "data/raw/conalco",
  retailer = "conalco") {
  meta = response[setdiff(names(response), "body")]
  meta$run_id = run_id
  meta$retailer = retailer
  meta$request_type = request_type
  meta$retailer_product_id = product_id
  meta$scraper_version = SCRAPER_VERSION
  stem = file.path(base_dir, run_id, sprintf("%04d_%s", request_index,
      request_type))
  meta$raw_file = NA_character_
  meta$raw_sha256 = NA_character_
  if (!is.null(response$body)) {
    meta$raw_file = paste0(stem, ".body")
    meta$raw_sha256 = digest(response$body, algo = "sha256", serialize = FALSE)
    write_once(meta$raw_file, \(p) writeBin(response$body, p))
  }
  meta$metadata_file = paste0(stem, ".json")
  write_json_once(meta, meta$metadata_file)
  meta
}
read_archive = \(metadata_file) {
  meta = read_json(metadata_file, simplifyVector = TRUE)
  if (is.null(meta$raw_file) || is.na(meta$raw_file) ||
      !file.exists(meta$raw_file)) stop("No archived response body")
  bytes = readBin(meta$raw_file, "raw", n = file.info(meta$raw_file)$size)
  if (!identical(digest(bytes, algo = "sha256", serialize = FALSE),
      meta$raw_sha256)) stop("Archive hash mismatch")
  list(meta = meta, body = bytes)
}


# Origin: storage.R, 261029; consolidated 261030.
# 6) VERSIONED STORAGE; PROTOTYPE ORIGIN 261029
# <-----------------------------------------------------------------------------

# CSV logic reused from the local feasibility study, originally 261029.
dedup_dependencies = \() {
  for (package in c('jsonlite', 'digest')) {
    if (!requireNamespace(package, quietly = TRUE)) stop('Missing: ', package)
  }
  invisible(TRUE)
}

dedup_read = \(path) {
  connection = gzfile(path, 'rb')
  on.exit(close(connection))
  chunks = list()
  repeat {
    bytes = readBin(connection, 'raw', n = 8L * 1024L^2)
    if (!length(bytes)) break
    chunks[[length(chunks) + 1L]] = bytes
  }
  text = rawToChar(do.call(c, chunks))
  Encoding(text) = 'UTF-8'
  lines = strsplit(text, '\n', fixed = TRUE)[[1]]
  quotes = lengths(regmatches(lines, gregexpr('"', lines, fixed = TRUE,
    useBytes = TRUE)))
  ends = which(cumsum(quotes) %% 2L == 0L)
  stopifnot(length(ends) > 0L, tail(ends, 1L) == length(lines))
  starts = c(1L, head(ends, -1L) + 1L)
  records = vapply(seq_along(ends), \(i) {
    paste(lines[starts[i]:ends[i]], collapse = '\n')
  }, '')
  marker = '__APM_UNQUOTED_MISSING__'
  while (any(grepl(marker, records, fixed = TRUE))) marker = paste0(marker, '_')
  pattern = paste0('"(?:[^"]++|"")*"(*SKIP)(*F)|',
    '(?<=,)(?=,|\\r?$)|^(?=,|\\r?$)')
  records = gsub(pattern, marker, records, perl = TRUE, useBytes = TRUE)
  read.csv(text = records, colClasses = 'character', na.strings = marker,
    check.names = FALSE, strip.white = FALSE, comment.char = '',
    blank.lines.skip = FALSE, fill = FALSE, encoding = 'UTF-8')
}

dedup_write = \(x, path, level = 6L) {
  if (file.exists(path)) stop('Refusing overwrite: ', path)
  connection = gzfile(path, 'wt', compression = level)
  on.exit(close(connection))
  write.table(x, connection, sep = ',', row.names = FALSE, na = '',
    qmethod = 'double', fileEncoding = 'UTF-8')
  invisible(path)
}

dedup_json = \(x, path) {
  if (file.exists(path)) stop('Refusing overwrite: ', path)
  jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE, na = 'null',
    null = 'null')
}

dedup_hash = \(path) digest::digest(file = path, algo = 'sha256')

dedup_content = \(values) {
  encoded = ifelse(is.na(values), 'N',
    paste0('V', nchar(values, type = 'bytes'), ':', values))
  digest::digest(paste0(encoded, collapse = ''), algo = 'sha256',
    serialize = FALSE)
}

dedup_version_id = \(product, previous, content) {
  dedup_content(c(product, previous, content))
}

dedup_change_type = \(old, new, previously_known = TRUE) {
  if (is.na(old) && !is.na(new) && new == '') return('missing_to_empty')
  if (!is.na(old) && old == '' && is.na(new)) return('empty_to_missing')
  if (!is.na(old) && old == '' && !is.na(new) && new != '')
    return('empty_to_known')
  if (!is.na(old) && old != '' && !is.na(new) && new == '')
    return('known_to_empty')
  if (is.na(old) && !is.na(new)) {
    return(if (previously_known) 'missing_to_known' else
      'first_known_after_missing')
  }
  if (!is.na(old) && is.na(new)) return('known_to_missing')
  'observed_value_change'
}

dedup_schema = \(root) {
  jsonlite::read_json(file.path(root, 'schema.json'), simplifyVector = TRUE)
}

dedup_init = \(root, columns, tracked, core, retailer = 'conalco',
    usage_mode = 'derived') {
  dedup_dependencies()
  if (dir.exists(root)) stop('Store already exists')
  stopifnot(!anyDuplicated(columns), all(c(tracked, core) %in% columns))
  dir.create(root, recursive = TRUE)
  dir.create(file.path(root, 'commits'))
  dir.create(file.path(root, 'transactions'))
  stopifnot(length(retailer) == 1L, !is.na(retailer), nzchar(retailer),
    usage_mode %in% c('derived', 'stored'))
  schema = list(format_version = 'apm-versioned-2.0',
    retailer = retailer, usage_mode = usage_mode,
    columns = columns, tracked = tracked, moved = setdiff(tracked, core),
    observation_columns = setdiff(columns, setdiff(tracked, core)),
    core = core, import = 'base R; quote-aware empty fields; character; UTF-8',
    compression_level = 6L,
    versions = 'Immutable episodes; usage endpoints are derived views',
    source_check = 'Unknown unless independently proven; never inferred')
  dedup_json(schema, file.path(root, 'schema.json'))
  invisible(root)
}

dedup_validate_commit = \(path, schema_hash, previous_hash) {
  manifest = jsonlite::read_json(file.path(path, 'manifest.json'),
    simplifyVector = TRUE)
  stopifnot(identical(manifest$schema_sha256, schema_hash),
    identical(manifest$previous_commit_sha256, previous_hash))
  files = manifest$files
  required = c('observations.csv.gz', 'product_versions.csv.gz',
    'product_changes.csv.gz', 'origin_receipt.json')
  if (is.null(manifest$usage_mode) || manifest$usage_mode != 'derived')
    required = c(required, 'version_usage.csv.gz')
  stopifnot(!anyDuplicated(files$file), all(required %in% files$file),
    all(files$file %in% c('observations.csv.gz', 'product_versions.csv.gz',
      'product_changes.csv.gz', 'version_usage.csv.gz', 'origin_receipt.json',
      'statistics.csv')),
    !any(grepl('[/\\]', files$file)))
  for (i in seq_len(nrow(files))) {
    p = file.path(path, files$file[i])
    if (!file.exists(p) || file.size(p) != files$bytes[i] ||
        dedup_hash(p) != files$sha256[i]) stop('Corrupt or missing: ', p)
  }
  manifest
}

dedup_history = \(root) {
  dirs = sort(list.dirs(file.path(root, 'commits'), recursive = FALSE,
    full.names = TRUE))
  schema_hash = dedup_hash(file.path(root, 'schema.json'))
  previous = 'GENESIS'
  result = vector('list', length(dirs))
  for (i in seq_along(dirs)) {
    result[[i]] = dedup_validate_commit(dirs[i], schema_hash, previous)
    stopifnot(basename(dirs[i]) == result[[i]]$date)
    previous = dedup_hash(file.path(dirs[i], 'manifest.json'))
  }
  names(result) = basename(dirs)
  result
}

dedup_table = \(root, filename, dates = names(dedup_history(root))) {
  if (!length(dates)) return(NULL)
  result = do.call(rbind, lapply(dates, \(date) {
    path = file.path(root, 'commits', date, filename)
    if (filename == 'version_usage.csv.gz' &&
        identical(dedup_schema(root)$usage_mode, 'derived')) {
      obs = dedup_read(file.path(dirname(path), 'observations.csv.gz'))
      new = dedup_read(file.path(dirname(path), 'product_versions.csv.gz'))
      storage_usage(obs, new$product_version_id)
    } else dedup_read(path)
  }))
  rownames(result) = NULL
  result
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) CHRONOLOGICAL, IDEMPOTENT AND ATOMIC DAILY COMMIT
# <-----------------------------------------------------------------------------

dedup_ingest = \(root, shard, receipt, fail_at = NULL, statistics = NULL,
    statistics_origin = NULL) {
  dedup_dependencies()
  lock = file.path(root, '.writer_lock')
  if (!dir.create(lock, showWarnings = FALSE)) stop('Writer lock exists')
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  dedup_json(list(pid = Sys.getpid(), acquired_at_utc = format(Sys.time(),
    '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')), file.path(lock, 'owner.json'))
  history = dedup_history(root)
  s = dedup_schema(root)
  meta = jsonlite::read_json(receipt, simplifyVector = TRUE)
  meta$raw_archive = NULL
  meta$created_at_utc = NULL
  meta$observations$path = 'observations.csv.gz'
  receipt_hash = dedup_content(jsonlite::toJSON(meta, auto_unbox = TRUE,
    null = 'null'))
  input_hash = dedup_hash(shard)
  stopifnot(identical(input_hash, meta$observations$sha256))
  date = meta$date
  run_id = meta$run_id
  if (!is.null(statistics)) {
    statistics = as.data.frame(lapply(statistics, as.character),
      stringsAsFactors = FALSE)
    stopifnot(nrow(statistics) > 0L, all(statistics$date == date),
      all(statistics$run_id == run_id))
  }
  if (date %in% names(history)) {
    prior = history[[date]]
    if (prior$input_sha256 != input_hash || prior$run_id != run_id ||
        prior$receipt_sha256 != receipt_hash)
      stop('Conflicting input for committed date')
    stat = file.path(root, 'commits', date, 'statistics.csv')
    if (!is.null(statistics) && file.exists(stat))
      stopifnot(identical(statistics, dedup_read(stat)))
    return(invisible('already_committed'))
  }
  if (!grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', date) ||
      is.na(as.Date(date))) stop('Invalid commit date')
  if (length(history) && date <= tail(names(history), 1L))
    stop('Chronological append required')
  x = dedup_read(shard)
  stopifnot(setequal(names(x), s$columns), !anyDuplicated(names(x)))
  x = x[s$columns]
  x = x[order(x$retailer, x$retailer_product_id), ]
  rownames(x) = NULL
  keys = c('retailer', 'retailer_product_id', 'product_key',
    'observation_key', 'daily_date', 'run_id', 'observed_at')
  stopifnot(all(vapply(x[keys], \(v) !anyNA(v) && all(nzchar(v)),
    logical(1))), all(x$retailer ==
      if (is.null(s$retailer)) 'conalco' else s$retailer),
    identical(x$product_key, paste(x$retailer, x$retailer_product_id,
      sep = ':')))
  if (length(history)) {
    if (run_id %in% vapply(history, \(r) r$run_id, ''))
      stop('Run-ID is already committed on another date')
    types = setNames(rep('NULL', length(s$observation_columns) + 1L),
      c(s$observation_columns, 'product_version_id'))
    types['observation_key'] = 'character'
    old_keys = unlist(lapply(names(history), \(date) {
      path = file.path(root, 'commits', date, 'observations.csv.gz')
      read.csv(path, colClasses = types, check.names = FALSE)$observation_key
    }), use.names = FALSE)
    if (any(x$observation_key %in% old_keys))
      stop('Observation key already exists in historical data')
  }
  stopifnot(nrow(x) == meta$n_products, all(x$daily_date == date),
    all(x$run_id == run_id), !anyNA(x$product_key),
    !anyNA(x$observation_key), !anyDuplicated(x$product_key),
    !anyDuplicated(x$observation_key))
  versions = dedup_table(root, 'product_versions.csv.gz', names(history))
  previous_versions = if (is.null(versions)) NULL else
    versions[!duplicated(versions$product_key, fromLast = TRUE), ]
  old_at = if (is.null(previous_versions)) rep(NA_integer_, nrow(x)) else
    match(x$product_key, previous_versions$product_key)
  hashes = vapply(seq_len(nrow(x)), \(i) {
    dedup_content(unlist(x[i, s$tracked], use.names = FALSE))
  }, character(1))
  old_hashes = if (is.null(previous_versions)) rep(NA_character_, nrow(x)) else
    previous_versions$content_sha256[old_at]
  changed = is.na(old_at) | hashes != old_hashes
  ids = if (is.null(previous_versions)) rep(NA_character_, nrow(x)) else
    previous_versions$product_version_id[old_at]
  episodes = if (is.null(previous_versions)) rep(0L, nrow(x)) else
    as.integer(previous_versions$episode[old_at])
  episodes[is.na(episodes)] = 0L
  for (i in which(changed)) {
    prior = if (is.na(ids[i])) 'GENESIS' else ids[i]
    ids[i] = dedup_version_id(x$product_key[i], prior, hashes[i])
  }
  new = x[which(changed), unique(c('retailer', 'retailer_product_id',
    'product_key', s$tracked)), drop = FALSE]
  new$product_version_id = ids[changed]
  new$content_sha256 = hashes[changed]
  new$episode = as.character(episodes[changed] + 1L)
  new$first_observed_date = x$daily_date[changed]
  new$first_observed_at = x$observed_at[changed]
  new$creation_run_id = x$run_id[changed]
  new$creation_source_metadata = ifelse(
    x$detail_coverage_method[changed] == 'individual_product_page',
    x$detail_metadata_file[changed], x$listing_metadata_file[changed])
  new$creation_raw_sha256 = x$raw_sha256[changed]
  rownames(new) = NULL
  change_rows = list()
  n_change = 0L
  for (i in which(changed)) {
    initial = is.na(old_at[i])
    old = if (initial) rep(NA_character_, length(s$tracked)) else
      unlist(previous_versions[old_at[i], s$tracked], use.names = FALSE)
    current = unlist(x[i, s$tracked], use.names = FALSE)
    diff = if (initial) 0L else which(
      is.na(old) != is.na(current) |
        (!is.na(old) & !is.na(current) & old != current))
    for (j in diff) {
      n_change = n_change + 1L
      seen = if (initial) FALSE else any(!is.na(
        versions[[s$tracked[j]]][versions$product_key == x$product_key[i]]))
      kind = if (initial) 'initial_product' else
        dedup_change_type(old[j], current[j], previously_known = seen)
      change_rows[[n_change]] = data.frame(
        retailer = x$retailer[i], retailer_product_id =
          x$retailer_product_id[i], product_key = x$product_key[i],
        date = date, observed_at = x$observed_at[i], run_id = run_id,
        field = if (initial) '__initial_product__' else s$tracked[j],
        old_version_id = if (initial)
          NA_character_ else previous_versions$product_version_id[old_at[i]],
        new_version_id = ids[i], change_type = kind,
        source_metadata = ifelse(
          x$detail_coverage_method[i] == 'individual_product_page',
          x$detail_metadata_file[i], x$listing_metadata_file[i]),
        raw_sha256 = x$raw_sha256[i], stringsAsFactors = FALSE)
    }
  }
  change_columns = c('retailer', 'retailer_product_id', 'product_key',
    'date', 'observed_at', 'run_id', 'field', 'old_version_id',
    'new_version_id', 'change_type', 'source_metadata', 'raw_sha256')
  changes = if (length(change_rows)) do.call(rbind, change_rows) else
    as.data.frame(setNames(rep(list(character()), length(change_columns)),
      change_columns))
  obs = x[s$observation_columns]
  obs$product_version_id = ids
  dir.create(file.path(root, 'transactions'), showWarnings = FALSE)
  pending = tempfile('pending_', tmpdir = file.path(root, 'transactions'))
  dir.create(pending)
  dedup_write(new, file.path(pending, 'product_versions.csv.gz'))
  if (identical(fail_at, 'after_versions')) stop('Injected interruption')
  dedup_write(obs, file.path(pending, 'observations.csv.gz'))
  dedup_write(changes, file.path(pending, 'product_changes.csv.gz'))
  if (!identical(s$usage_mode, 'derived'))
    dedup_write(storage_usage(obs, new$product_version_id),
      file.path(pending, 'version_usage.csv.gz'))
  stopifnot(file.copy(receipt, file.path(pending, 'origin_receipt.json')))
  if (!is.null(statistics)) {
    write.csv(statistics, file.path(pending, 'statistics.csv'),
      row.names = FALSE, na = '')
  }
  files = sort(list.files(pending))
  records = data.frame(file = files, bytes = file.size(file.path(pending,
    files)), sha256 = vapply(file.path(pending, files), dedup_hash, ''))
  rownames(records) = NULL
  previous = if (!length(history)) 'GENESIS' else dedup_hash(file.path(
    root, 'commits', tail(names(history), 1L), 'manifest.json'))
  manifest = list(date = date, run_id = run_id, n_products = nrow(x),
    new_products = sum(is.na(old_at)), new_versions = nrow(new),
    changed_products = sum(changed & !is.na(old_at)),
    usage_mode = if (is.null(s$usage_mode)) 'stored' else s$usage_mode,
    input_sha256 = input_hash, receipt_sha256 = receipt_hash,
    source_receipt = meta,
    source_manifest_sha256 = meta$source_manifest$sha256,
    statistics_origin = statistics_origin,
    schema_sha256 = dedup_hash(file.path(root, 'schema.json')),
    previous_commit_sha256 = previous, files = records)
  dedup_json(manifest, file.path(pending, 'manifest.json'))
  dedup_validate_commit(pending, manifest$schema_sha256, previous)
  available = c(if (!is.null(versions)) versions$product_version_id,
    new$product_version_id)
  stopifnot(!anyDuplicated(available), all(ids %in% available))
  if (identical(fail_at, 'before_commit')) stop('Injected interruption')
  target = file.path(root, 'commits', date)
  if (dir.exists(target) || !file.rename(pending, target))
    stop('Atomic commit failed; pending transaction retained')
  invisible('committed')
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 8) HISTORICAL EXPORT AND DERIVED VERSION ENDPOINTS
# <-----------------------------------------------------------------------------

dedup_restore_rows = \(root, from, to) {
  history = dedup_history(root)
  dates = names(history)
  bounds = c(from, to)
  stopifnot(length(bounds) == 2L, !anyNA(as.Date(bounds)),
    identical(format(as.Date(bounds), '%Y-%m-%d'), bounds), from <= to)
  selected = dates[dates >= from & dates <= to]
  if (!length(selected)) stop('No available observation dates in range')
  s = dedup_schema(root)
  versions = dedup_table(root, 'product_versions.csv.gz', dates[dates <= to])
  stopifnot(!anyDuplicated(versions$product_version_id))
  restored = lapply(selected, \(date) {
    obs = dedup_read(file.path(root, 'commits', date, 'observations.csv.gz'))
    at = match(obs$product_version_id, versions$product_version_id)
    if (anyNA(at)) stop('Missing product version')
    stopifnot(identical(obs$product_key, versions$product_key[at]))
    overlap = intersect(s$tracked, s$observation_columns)
    for (field in overlap) {
      stopifnot(identical(obs[[field]], versions[[field]][at]))
    }
    for (field in s$moved) obs[[field]] = versions[[field]][at]
    obs[s$columns]
  })
  x = do.call(rbind, restored)
  rownames(x) = NULL
  stopifnot(!anyDuplicated(x$observation_key),
    !anyDuplicated(x[c('retailer', 'retailer_product_id', 'daily_date')]))
  x
}

dedup_panel_values = \(x) {
  # Match daily_read_csv(na.strings = '') only in the legacy export view.
  for (field in names(x)) {
    x[[field]][!is.na(x[[field]]) & x[[field]] == ''] = NA_character_
  }
  x
}

export_conalco_panel = \(from, to, output,
    root = conalco_versioned_root(),
    preserve_shard_empty_strings = FALSE) {
  storage_export_panel(root, from, to, output,
    preserve_empty = preserve_shard_empty_strings)
}

storage_export_panel = \(root, from, to, output, preserve_empty = TRUE) {
  x = dedup_restore_rows(root, from, to)
  if (!preserve_empty) x = dedup_panel_values(x)
  dedup_write(x, output)
  invisible(list(rows = nrow(x), days = length(unique(x$daily_date)),
    sha256 = dedup_hash(output), bytes = file.size(output)))
}

dedup_version_view = \(root, to = '9999-12-31') {
  dates = names(dedup_history(root))
  dates = dates[dates <= to]
  versions = dedup_table(root, 'product_versions.csv.gz', dates)
  usage = dedup_table(root, 'version_usage.csv.gz', dates)
  groups = split(seq_len(nrow(usage)), usage$product_version_id)
  at = groups[versions$product_version_id]
  versions$last_observed_date = vapply(at, \(i) {
    max(usage$daily_date[i])
  }, '')
  versions$last_observed_at = vapply(at, \(i) max(usage$observed_at[i]), '')
  versions$last_source_checked_at = NA_character_
  versions$source_check_status = 'not_independently_verified'
  versions
}

dedup_change_values = \(root, change) {
  stopifnot(nrow(change) == 1L, change$field != '__initial_product__')
  versions = dedup_table(root, 'product_versions.csv.gz')
  stopifnot(change$field %in% dedup_schema(root)$tracked)
  old = match(change$old_version_id, versions$product_version_id)
  new = match(change$new_version_id, versions$product_version_id)
  stopifnot(!is.na(new))
  list(old_value = versions[[change$field]][old],
    new_value = versions[[change$field]][new])
}

# Shared normalized observation contract; adapters provide actual field schemas.
storage_usage = \(obs, new_ids) {
  usage = obs[c('product_key', 'observation_key', 'daily_date', 'observed_at',
    'run_id', 'detail_coverage_method', 'listing_metadata_file',
    'detail_metadata_file', 'raw_sha256')]
  usage$product_version_id = obs$product_version_id
  usage$source_checked_at = NA_character_
  usage$source_check_status = 'not_independently_verified'
  usage$content_changed = ifelse(obs$product_version_id %in% new_ids,
    'TRUE', 'FALSE')
  usage$source_metadata = ifelse(
    obs$detail_coverage_method == 'individual_product_page',
    obs$detail_metadata_file, obs$listing_metadata_file)
  usage$source_claims_current_run = ifelse(is.na(usage$source_metadata),
    'unknown', ifelse(vapply(seq_len(nrow(usage)), \(i) {
      grepl(obs$run_id[i], usage$source_metadata[i], fixed = TRUE)
    }, logical(1)), 'TRUE', 'FALSE'))
  usage
}

# An adapter must supply canonical keys, provenance and its own tracked fields.
storage_profile = \(retailer, columns, tracked, core) {
  required = c('retailer', 'retailer_product_id', 'product_key',
    'observation_key', 'daily_date', 'run_id', 'observed_at',
    'detail_coverage_method', 'listing_metadata_file',
    'detail_metadata_file', 'raw_sha256')
  stopifnot(is.character(retailer), length(retailer) == 1L,
    !is.na(retailer), nzchar(retailer),
    all(required %in% setdiff(columns, setdiff(tracked, core))),
    all(c(tracked, core) %in% columns), !anyDuplicated(columns))
  list(retailer = retailer, columns = columns, tracked = tracked, core = core)
}

storage_init = \(root, profile, usage_mode = 'derived') {
  profile = do.call(storage_profile, profile)
  dedup_init(root, profile$columns, profile$tracked, profile$core,
    retailer = profile$retailer, usage_mode = usage_mode)
}
