# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261016

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------
rm(list = ls())

pack =
  c("digest", "dplyr", "httr2", "jsonlite", "xml2")

# Project root, renv library, package check and shared modules
# (scripts/bootstrap.R)
script_args = commandArgs(trailingOnly = F)
script_file = sub('^--file=', '', script_args[grepl('^--file=',
      script_args)][1])
source(file.path(dirname(script_file), 'bootstrap.R'))
load_project(pack = pack)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) EXPLICIT AUTHORIZED REFRESH; NEVER INVOKED BY THE DAILY RUNNER
# <-----------------------------------------------------------------------------

for (module in c('catalog.R', 'derivation.R', 'catalog_refresh.R',
    'refresh_validation.R'))
  source(file.path('R', module), keep.source = T)
args = commandArgs(trailingOnly = T)
if (length(args) != 1L || !startsWith(args, '--run-id='))
  stop(paste0("Usage: Rscript scripts/refresh_conalco.R ",
      "--run-id=EXISTING_AUTHORIZED_RUN"))
ensure_dir('logs/conalco_refresh_2026-10-16')
writeLines(as.character(Sys.getpid()),
  'logs/conalco_refresh_2026-10-16/collection.pid')
id = sub('^--run-id=', '', args)
state = tryCatch(run_conalco_refresh(id), error = \(e) {
  if (file.exists(file.path(run_dir(id), 'checkpoint.rds')))
    refresh_store_partial(id, conditionMessage(e))
  stop(e)
})
if (identical(state$stage, 'partial'))
  cat('Partial observations:', refresh_store_partial(id), '\n')
