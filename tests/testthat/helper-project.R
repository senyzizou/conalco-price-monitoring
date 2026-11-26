# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261007

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) HELPER PROJECT
# <-----------------------------------------------------------------------------

APM_ROOT = normalizePath("../..")

# Test runs attach the project and test packages here; scripts attach them via
# load_project().
pack = c("digest", "dplyr", "httr2", "jsonlite", "xml2", "testthat", "withr")
invisible(lapply(pack, library, character.only = T))

for (f in c("utils.R", "classification.R", "http.R", "retailers/conalco.R",
    "validate.R", "storage.R", "storage_transport.R")) source(file.path(APM_ROOT, "R",
      f), local = F)

local_project = \() local_dir(APM_ROOT, .local_envir = parent.frame())

source(file.path(APM_ROOT, 'tests/helpers/versioned_fixtures.R'), local = TRUE)
