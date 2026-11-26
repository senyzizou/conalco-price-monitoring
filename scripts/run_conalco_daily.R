# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261022

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

script_args = commandArgs(trailingOnly = F)
script_file = sub('^--file=', '', script_args[grepl('^--file=',
    script_args)][1])
source(file.path(dirname(script_file), 'bootstrap.R'))
load_project(pack = 'ps')
for (module in c('catalog.R', 'derivation.R', 'price_analysis.R',
    'catalog_refresh.R', 'refresh_validation.R', 'snapshot_analysis.R',
    'conalco_daily.R')) source(file.path('R', module))

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) LIVE COLLECTION OR EXPLICIT OFFLINE MAINTENANCE
# <-----------------------------------------------------------------------------

args = commandArgs(trailingOnly = T)
if (!length(args) || identical(args, '--manual')) {
  if (conalco_storage_mode() == 'versioned' &&
      !dir.exists(conalco_versioned_root()))
    stop('Explicit initial migration required before collection')
  run_conalco_daily(manual = identical(args, '--manual'))
} else if (length(args) == 1L && startsWith(args, '--append-run=')) {
  id = sub('^--append-run=', '', args)
  review = if (id == 'catalog_refresh_20261016T161846_574254837b68')
    'config/le_coq_conflicts.csv' else NULL
  append_conalco_daily(file.path(run_dir(id), 'manifest.json'),
    conflict_review = review)
} else if (identical(args, '--rebuild')) {
  if (conalco_storage_mode() == 'versioned') {
    conalco_versioned_validate(conalco_versioned_root())
  } else {
    lock = file.path(CONALCO_DAILY_ROOT, '.write_lock')
    daily_lock(lock)
    tryCatch(daily_rebuild_views(CONALCO_DAILY_ROOT),
      finally = unlink(lock, recursive = T))
  }
} else if (length(args) == 1L && startsWith(args, '--recover-lock=')) {
  kind = sub('^--recover-lock=', '', args)
  stopifnot(kind %in% c('collector', 'panel'))
  lock = if (kind == 'collector') 'logs/conalco_daily/.collector_lock' else
    if (conalco_storage_mode() == 'versioned')
      file.path(conalco_versioned_root(), '.writer_lock') else
        file.path(CONALCO_DAILY_ROOT, '.write_lock')
  owner = read_json(file.path(lock, 'owner.json'))
  if (owner$pid %in% ps_pids()) stop('Lock owner still exists; do not remove')
  unlink(lock, recursive = T)
  cat('Removed stale lock; original run and observations retained\n')
} else if (identical(args, '--write-launch-agent')) {
  # StartCalendarInterval uses the Mac's system timezone, not an env TZ value.
  if (!endsWith(Sys.readlink('/etc/localtime'), '/Europe/Berlin'))
    stop('Set macOS system timezone to Europe/Berlin before installation')
  root = normalizePath(getwd())
  logs = ensure_dir(file.path(root, 'logs/conalco_daily'))
  document = xml_new_root('plist', version = '1.0')
  dict = xml_add_child(document, 'dict')
  add = \(key, value, type = 'string', parent = dict) {
    xml_add_child(parent, 'key', key)
    xml_add_child(parent, type, as.character(value))
  }
  add('Label', 'de.apm.conalco.daily')
  xml_add_child(dict, 'key', 'ProgramArguments')
  arguments = xml_add_child(dict, 'array')
  for (value in c('/usr/bin/caffeinate', '-i',
      file.path(R.home('bin'), 'Rscript'), '--vanilla',
      file.path(root, 'scripts/run_conalco_daily.R')))
    xml_add_child(arguments, 'string', value)
  add('WorkingDirectory', root)
  xml_add_child(dict, 'key', 'StartCalendarInterval')
  calendar = xml_add_child(dict, 'dict')
  add('Hour', 9, 'integer', calendar)
  add('Minute', 0, 'integer', calendar)
  xml_add_child(dict, 'key', 'RunAtLoad')
  xml_add_child(dict, 'false')
  xml_add_child(dict, 'key', 'KeepAlive')
  xml_add_child(dict, 'false')
  add('StandardOutPath', file.path(logs, 'launchd.out.log'))
  add('StandardErrorPath', file.path(logs, 'launchd.error.log'))
  path = file.path(ensure_dir('tmp'), 'de.apm.conalco.daily.plist')
  write_xml(document, path)
  cat('Prepared only; not installed:', normalizePath(path), '\n')
} else stop(paste0('Use --manual, --append-run=ID, --rebuild, ',
    '--recover-lock=collector|panel or --write-launch-agent'))
