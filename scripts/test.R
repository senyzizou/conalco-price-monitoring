# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261007

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

pack =
  c("digest", "dplyr", "httr2", "jsonlite", "testthat", "xml2")

# Project root, renv library, package check and shared modules
# (scripts/bootstrap.R)
script_args = commandArgs(trailingOnly = F)
script_file = sub("^--file=", "", script_args[grepl("^--file=",
      script_args)][1])
source(file.path(dirname(script_file), "bootstrap.R"))
load_project(pack = pack)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) TEST
# <-----------------------------------------------------------------------------

result = test_dir("tests/testthat", reporter = "summary",
  stop_on_failure = FALSE)
counts = as.data.frame(result)
cat("Tests:", nrow(counts), "; passed expectations:", sum(counts$passed),
  "; failed:", sum(counts$failed), "; errors:", sum(counts$error),
  "; skipped:", sum(counts$skipped), "\n")
if (any(counts$failed > 0L | counts$error)) stop("Offline test failures")
