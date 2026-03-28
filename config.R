# config.R
# Central configuration for paths, dates, tickers, and model settings.

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
