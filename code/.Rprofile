# The scripts run from code/, but renv's project is the repository root, and R
# reads .Rprofile only from the working directory. This points renv at the root
# so the pinned library from renv.lock is used here too.
local({
  root <- normalizePath("..", mustWork = TRUE)
  activate <- file.path(root, "renv", "activate.R")
  if (file.exists(activate)) {
    Sys.setenv(RENV_PROJECT = root)
    source(activate)
  }
})
