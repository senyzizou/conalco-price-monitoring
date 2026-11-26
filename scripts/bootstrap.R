# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261007

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) BOOTSTRAP
# <-----------------------------------------------------------------------------

project_root_from_script = \(script_file = NULL) {
  if (is.null(script_file)) {
    script_args = commandArgs(trailingOnly = F)
    script_file = sub("^--file=", "", script_args[grepl("^--file=",
          script_args)][1])
  }
  if (length(script_file) != 1L || is.na(script_file))
    stop("Cannot determine the project root without a script path")
  root = normalizePath(dirname(script_file), mustWork = T)
  repeat {
    if (all(file.exists(file.path(root, c("renv.lock", "R/utils.R",
              "scripts/bootstrap.R")))))
      return(root)
    parent = dirname(root)
    if (identical(parent, root))
      stop("Cannot find the Alcohol Price Monitoring project root")
    root = parent
  }
}

# Packages used unqualified by the shared modules; scripts may add further
# packages.
PROJECT_PACKAGES = c("digest", "dplyr", "httr2", "jsonlite", "xml2")

load_project = \(root = project_root_from_script(), pack = PROJECT_PACKAGES) {
  setwd(root)
  if (!any(grepl(paste0(root, "/renv/library"), .libPaths(), fixed = T))) {
    # .Renviron holds machine-specific renv paths and is not part of the
    # repository.
    if (file.exists(".Renviron")) readRenviron(".Renviron")
    source("renv/activate.R")
  }
  # Packages come from renv.lock; restore them instead of installing unpinned
  # versions
  pack = unique(c(PROJECT_PACKAGES, pack))
  installed_pack = pack %in% rownames(installed.packages())
  if (any(installed_pack == F)) {
    stop("Missing packages: ", paste(pack[!installed_pack], collapse =
          ", "), ". Run renv::restore().")
  }

  invisible(lapply(pack, library, character.only = T))
  for (module in c("utils.R", "classification.R", "http.R",
      "retailers/conalco.R",
      "validate.R", "quality.R", "storage.R")) source(file.path(
        "R", module),
      local = .GlobalEnv)
  source(file.path('R', 'storage_transport.R'), local = .GlobalEnv)
  invisible(root)
}
