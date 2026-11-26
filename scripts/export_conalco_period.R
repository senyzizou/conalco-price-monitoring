# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261029

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

script_args = commandArgs(trailingOnly = F)
script_file = sub('^--file=', '', script_args[grepl('^--file=',
  script_args)][1])
source(file.path(dirname(script_file), 'bootstrap.R'))
load_project()
for (module in c('catalog.R', 'derivation.R', 'price_analysis.R',
    'catalog_refresh.R', 'refresh_validation.R', 'snapshot_analysis.R',
    'conalco_daily.R', 'conalco_daily.R')) source(file.path('R', module))

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) EXPLICIT LOCAL EXPORT; NO COLLECTION OR REMOTE ACCESS
# <-----------------------------------------------------------------------------

args = commandArgs(trailingOnly = T)
if (!length(args) %in% c(3L, 4L)) stop(
  'Use: Rscript --vanilla scripts/export_conalco_period.R ',
  'FROM TO NEW_OUTPUT_DIRECTORY [DAILY_ROOT]')
receipt = export_conalco_period(args[1], args[2], args[3],
  root = if (length(args) == 4L) args[4] else
    if (conalco_storage_mode() == 'versioned') conalco_versioned_root() else
      CONALCO_DAILY_ROOT)
cat('Exported', receipt$n_runs, 'daily runs and', receipt$n_observations,
  'observations to', dirname(receipt$panel$path), '\n')
