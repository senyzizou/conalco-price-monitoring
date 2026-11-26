# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261007

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) UTILS
# <-----------------------------------------------------------------------------

SCRAPER_VERSION = "0.1.0"
PARSER_VERSION = "conalco-0.1.0"

utc_now = \() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")

new_run_id = \(prefix = "pilot") {
  entropy = paste(Sys.time(), Sys.getpid(), runif(1), sep = "|")
  paste0(prefix, "_", format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "_",
    substr(digest(entropy,
        algo = "sha256"), 1, 12))
}

scalar = \(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L)
    return(default)
  value = as.character(x[[1L]])
  if (is.na(value) || !nzchar(trimws(value)))
    default
  else trimws(value)
}

clean_text = \(x) {
  x = gsub("[  ]", " ", x)
  trimws(gsub("\\s+", " ", x, perl = T))
}

berlin_date = \(x) {
  time = as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  format(time, "%Y-%m-%d", tz = "Europe/Berlin")
}

ensure_dir = \(path) {
  if (!dir.exists(path) && !dir.create(path, recursive = T))
    stop("Cannot create: ", path)
  invisible(path)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) IMMUTABLE PUBLICATION
# <-----------------------------------------------------------------------------

write_once = \(path, writer) {
  ensure_dir(dirname(path))
  if (file.exists(path))
    stop("Refusing to overwrite immutable file: ", path)
  pending_path = tempfile(pattern = ".pending_", tmpdir = dirname(path))
  on.exit(unlink(pending_path), add = T)
  writer(pending_path)
  if (file.exists(path) || !file.rename(pending_path, path))
    stop("Cannot publish: ", path)
  invisible(path)
}

write_json_once = \(x, path) write_once(path, \(p) {
    write_json(x, p, auto_unbox = T, pretty = T, na = "null", null = "null")
})

write_csv_once = \(x, path) write_once(path, \(p) {
    write.csv(x, p, row.names = F, na = "", fileEncoding = "UTF-8")
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) OBSERVATION TYPES
# <-----------------------------------------------------------------------------

read_observations = \(path) {
  # Identifiers (including GTINs) must never be guessed as numeric by read.csv.
  x = read.csv(path, colClasses = "character", na.strings = "", check.names = F)
  numeric_fields = intersect(names(x), c("volume_l", "pack_count",
      "unit_volume_l",
      "abv", "alcohol_content", "volume_l_displayed", "pack_count_declared",
      "unit_volume_l_declared",
      "total_volume_l_declared", "price_current", "price_reference",
      "price_per_l_displayed",
      "price_per_l", "stock_quantity", "deposit", "price_regular",
      "response_status"))
  for (n in numeric_fields) x[[n]] = as.numeric(x[[n]])
  if ("promo" %in% names(x))
  x$promo = as.logical(x$promo)
  x
}

log_event = \(path, event, level = "info", ...) {
  ensure_dir(dirname(path))
  record = c(list(logged_at_utc = utc_now(), level = level, event = event),
    list(...))
  cat(toJSON(record, auto_unbox = T, na = "null", null = "null"), "\n",
    file = path, append = T, sep = "")
  invisible(record)
}

add_flag = \(existing, flag) {
  old = if (is.na(existing) || !nzchar(existing))
    character()
  else strsplit(existing, ";", fixed = T)[[1]]
  paste(unique(c(old, flag)), collapse = ";")
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) SOURCE PROVENANCE
# <-----------------------------------------------------------------------------

snapshot_source = \(directory) {
  files = list.files(c("R", "scripts"), pattern = "[.]R$", recursive = T,
    full.names = T)
  records = lapply(files, \(source_path) list(path = source_path, sha256 =
      digest(file = source_path,
        algo = "sha256"), source = rawToChar(readBin(source_path, "raw",
          file.info(source_path)$size))))
  path = file.path(directory, "source_snapshot.json")
  write_json_once(list(captured_at_utc = utc_now(), files = records), path)
  list(path = path, sha256 = digest(file = path, algo = "sha256"))
}
