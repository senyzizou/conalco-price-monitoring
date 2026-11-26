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

# Compatibility entry point; the shared dispatcher preserves existing arguments.
script_args = commandArgs(trailingOnly = FALSE)
script_file = sub('^--file=', '', script_args[grepl('^--file=',
  script_args)][1])
offline_args = commandArgs(trailingOnly = TRUE)
allowed = c('--migrate', '--export', '--period', '--package', '--archive',
  '--restore', '--legacy-view', '--validate', '--import-state')
if (!length(offline_args) || !offline_args[1] %in% allowed)
  stop('Offline-only compatibility entry; choose a storage command')
source(file.path(dirname(script_file), 'conalco_actions.R'))
