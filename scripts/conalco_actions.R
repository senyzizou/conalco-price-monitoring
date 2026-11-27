# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261026

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

script_args = commandArgs(trailingOnly = FALSE)
script_file = sub('^--file=', '', script_args[grepl('^--file=',
  script_args)][1])
source(file.path(dirname(script_file), 'bootstrap.R'))
load_project(pack = c('ps', 'testthat', 'withr'))
for (module in c('catalog.R', 'derivation.R', 'price_analysis.R',
    'catalog_refresh.R', 'refresh_validation.R', 'snapshot_analysis.R',
    'conalco_daily.R', 'conalco_actions.R'))
  source(file.path('R', module))
# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) EXPLICIT OPERATIONS; NO DEFAULT LIVE ENTRY POINT
# <-----------------------------------------------------------------------------

# The runner uses the existing repository layout; the Mac default stays local.
CONALCO_DAILY_ROOT = Sys.getenv('APM_DAILY_ROOT', CONALCO_DAILY_ROOT)
args = commandArgs(trailingOnly = TRUE)
actions_check_deployment()
privileged = length(args) == 1L && args %in% c('--preflight', '--full',
  '--recover', '--retain-attempt')
if (privileged && (!identical(Sys.getenv('GITHUB_ACTIONS'), 'true') ||
    !identical(Sys.getenv('GITHUB_REPOSITORY'),
      'senyzizou/conalco-price-monitoring') ||
    !identical(Sys.getenv('GITHUB_REF'), 'refs/heads/main') ||
    !Sys.getenv('GITHUB_EVENT_NAME') %in% c('workflow_dispatch', 'schedule')))
  stop('Privileged operations require the canonical GitHub main workflow')
if (length(args) == 1L && args %in% c('--full', '--recover'))
  options(apm.conalco.phase_log = file.path(getwd(), 'logs/conalco_daily',
    'runner_phases.jsonl'))
if (identical(args, '--offline')) {
  actions_preflight()
} else if (identical(args, '--preflight')) {
  actions_with_reference(\() actions_preflight(online = T))
} else if (identical(args, '--full')) {
  actions_phase('full.execution',
    actions_with_reference(\() actions_full_run()))
} else if (identical(args, '--retain-attempt')) {
  actions_retain_attempt()
} else if (identical(args, '--prepare-seed')) {
  actions_prepare_seed()
} else if (identical(args, '--pack-state')) {
  actions_state_pack('tmp/actions_state_seed')
} else if (identical(args, '--recover')) {
  actions_phase('recovery.execution',
    actions_with_reference(\() actions_full_run(recovery = T)))
} else if (length(args) == 1L &&
    args %in% c('--tests-fast', '--tests', '--tests-extended')) {
  started = proc.time()[['elapsed']]
  extended = identical(args, '--tests-extended')
  filter = if (extended) '^conalco-startup$' else paste0(
    '^(conalco-(daily|actions|pagination|reference|',
    'public-security|export|versioned)',
    '|consolidation)$')
  result = with_envvar(c(APM_TEST_SCOPE = 'actions-fast'),
    test_dir('tests/testthat',
      filter = filter,
      reporter = 'summary', stop_on_failure = TRUE, load_package = 'none'))
  counts = as.data.frame(result)
  cat('Tests:', nrow(counts), '; passed expectations:', sum(counts$passed),
    '; failed:', sum(counts$failed), '; skipped:', sum(counts$skipped), '\n')
  cat(if (extended) 'Extended' else 'Fast', 'suite elapsed seconds:',
    proc.time()[['elapsed']] - started, '\n')
  details = as.data.frame(result)
  write.csv(details[c('file', 'test', 'real', 'passed', 'failed', 'skipped')],
    file.path(ensure_dir('tmp'), if (extended) 'tests_extended_timings.csv' else
      'tests_fast_timings.csv'), row.names = F)
} else if (length(args) == 1L && startsWith(args, '--reproduce=')) {
  actions_reproduce(sub('^--reproduce=', '', args))
} else if (length(args) == 3L && args[1] == '--migrate') {
  conalco_migrate_legacy(args[2], args[3])
} else if (length(args) %in% c(5L, 6L) && args[1] == '--export') {
  preserve = length(args) == 6L && args[6] == '--preserve-empty'
  if (length(args) == 6L && !preserve) stop('Unknown export option')
  export_conalco_panel(args[2], args[3], args[4], root = args[5],
    preserve_shard_empty_strings = preserve)
} else if (length(args) == 5L && args[1] == '--period') {
  conalco_export_versioned_period(args[2], args[3], args[4], args[5])
} else if (length(args) == 3L && args[1] == '--package') {
  conalco_package(args[2], args[3])
} else if (length(args) %in% c(3L, 4L) && args[1] == '--archive') {
  previous = if (length(args) == 4L) jsonlite::read_json(args[4]) else NULL
  date = tail(names(dedup_history(args[2])), 1L)
  packed = conalco_pack_increment(args[2], args[3], date, previous)
  dedup_json(packed$checkpoint, file.path(args[3], 'checkpoint.json'))
} else if (length(args) == 4L && args[1] == '--restore') {
  checkpoint = jsonlite::read_json(args[2])
  archives = list.files(args[3], '[.]tar[.]gz$', recursive = TRUE,
    full.names = TRUE)
  receipts = list.files(args[3], '[.]json$', recursive = TRUE,
    full.names = TRUE)
  archive_hashes = vapply(archives, dedup_hash, '')
  receipt_hashes = vapply(receipts, dedup_hash, '')
  wanted = c(lapply(checkpoint$dependencies, \(r) r$archive_sha256),
    list(checkpoint$archive_sha256))
  wanted_receipts = c(lapply(checkpoint$dependencies,
    \(r) r$receipt_sha256), list(checkpoint$receipt_sha256))
  at = match(unlist(wanted), archive_hashes)
  rt = match(unlist(wanted_receipts), receipt_hashes)
  stopifnot(!anyNA(at), !anyNA(rt))
  conalco_restore_increments(archives[at], receipts[rt], args[4], checkpoint)
} else if (length(args) == 3L && args[1] == '--legacy-view') {
  conalco_legacy_view(args[2], args[3])
} else if (length(args) == 2L && args[1] == '--validate') {
  print(conalco_versioned_validate(args[2]))
} else if (length(args) == 6L && args[1] == '--import-state') {
  conalco_import_state(args[2], args[3], args[4], args[5], args[6])
} else stop('Use --migrate, --export, --period, --package, --archive, ',
  '--restore, --legacy-view, --validate, --import-state or --offline, ',
  '--preflight, --full, --recover, --tests-fast ',
  '(alias --tests), --tests-extended, ',
  '--prepare-seed, ',
  '--pack-state, --retain-attempt ',
  'or --reproduce=ARCHIVE')
