# config.R
# Central configuration for paths, dates, tickers, and model settings.

load_required_packages <- function(attach = TRUE) {
  pkgs <- c("quantmod", "tidyverse", "lubridate", "broom")

  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(
      paste0(
        "Missing required packages: ",
        paste(missing_pkgs, collapse = ", "),
        ". Please install them with install.packages()."
      )
    )
  }

  if (isTRUE(attach)) {
    invisible(lapply(pkgs, library, character.only = TRUE))
  }

load_required_packages <- function() {
  pkgs <- c("quantmod", "tidyverse", "lubridate", "broom")
  # Skeleton only: package checks/loading can be expanded later.
  invisible(pkgs)
}

get_config <- function() {
  list(
    market = "HK",
    tickers = c(
      # e.g. "0005.HK", "0700.HK"
    ),
    start_date = as.Date("2015-01-01"),
    end_date = Sys.Date(),
    forecast_horizon = 21,
    train_ratio = 0.7,
    paths = list(
      raw_data = "data/raw",
      processed_data = "data/processed",
      output = "data/output"
    )
  )
}

initialize_project_dirs <- function(config) {
  dir_paths <- unlist(config$paths, use.names = FALSE)

  for (dir_path in dir_paths) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
  }

  invisible(dir_paths)
}
