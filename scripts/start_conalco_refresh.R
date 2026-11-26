# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261019

# <=============================================================================
# <=============================================================================
# <=============================================================================

# Usage: Rscript scripts/start_conalco_refresh.R --run-id=<existing authorized
# refresh run>
# Network access: this starts live collection; run only after explicit approval.

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

# 1) START AN AUTHORIZED CONALCO REFRESH DETACHED FROM THE TERMINAL
# <-----------------------------------------------------------------------------

for (module in c('catalog.R', 'derivation.R', 'catalog_refresh.R'))
  source(file.path('R', module), keep.source = T)
args = commandArgs(trailingOnly = T)
if (length(args) != 1L || !startsWith(args, '--run-id='))
  stop(paste0("Usage: Rscript scripts/start_conalco_refresh.R ",
      "--run-id=EXISTING_AUTHORIZED_RUN"))
pid = refresh_start_detached(sub('^--run-id=', '', args))
cat('Detached R collector PID', pid,
  '; log: logs/conalco_refresh_2026-10-16/collection.log\n')
