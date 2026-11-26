# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) ISOLATED CONALCO SCRIPT STARTUPS
# <-----------------------------------------------------------------------------
# Origin: test-consolidation.R, 261030; separated 261031.
# These Conalco entry-point startup cases run in the extended preflight.

test_that(
  'Conalco entry points load from their actual paths without network', {
  tools = c('scripts/conalco_actions.R', 'scripts/refresh_conalco.R',
    'scripts/start_conalco_refresh.R', 'scripts/run_conalco_daily.R',
    'scripts/export_conalco_period.R', 'scripts/verify_daily.R')
  for (tool in tools) {
    started = proc.time()[['elapsed']]
    cat("STARTUP", tool, "\n")
    result = callr::r(\(root, tool) {
      script = file.path(root, tool)
      mocks = new.env()
      mocks$commandArgs = \(trailingOnly = F) {
        if (trailingOnly) character() else paste0('--file=', script)
      }
      attach(mocks, name = 'APM_STARTUP_TEST')
      setwd(tempdir())
      expr = parse(script)
      boundary = which(vapply(expr, \(x) is.call(x) &&
        identical(x[[1]], as.name('load_project')), logical(1)))[1]
      stopifnot(!is.na(boundary))
      for (x in expr[seq_len(boundary)]) eval(x, envir = .GlobalEnv)
      list(root = normalizePath(getwd()), shared =
        exists('storage_init', mode = 'function'))
    }, args = list(root = APM_ROOT, tool = tool), libpath = .libPaths(),
      system_profile = F, user_profile = F)
    cat('STARTUP_SECONDS', tool, proc.time()[['elapsed']] - started, '\n')
    expect_identical(result$root, APM_ROOT, info = tool)
    expect_true(result$shared, info = tool)
  }
})
