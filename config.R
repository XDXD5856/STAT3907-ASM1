# config.R
# Central configuration for the HK stock selection pipeline.

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

  invisible(pkgs)
}

get_config <- function() {
  list(
    investment_hkd = 1000000,
    # HKEX official securities list endpoint (can be changed if HKEX updates URL)
    hkex_securities_list_url = "https://www.hkex.com.hk/eng/services/trading/securities/securitieslists/ListOfSecurities.csv",
    start_date = as.Date("2015-01-01"),
    end_date = Sys.Date(),
    target_horizon = 21,
    min_history_days = 252 * 3,
    max_missing_ratio = 0.2,
    min_avg_daily_volume = 100000,
    train_ratio = 0.7,
    max_predictors = 15,
    candidate_predictors = NULL,
    paths = list(
      raw_data = "data/raw",
      processed_data = "data/processed",
      output = "data/output"
    ),
    files = list(
      universe = "data/output/universe_qualified.csv",
      raw_panel = "data/raw/raw_panel.csv",
      model_panel = "data/processed/model_panel.csv",
      all_models = "data/output/all_models.csv",
      predictions = "data/output/predictions_latest.csv",
      ranking = "data/output/ranked_stocks.csv",
      top_pick = "data/output/top_pick.csv"
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
