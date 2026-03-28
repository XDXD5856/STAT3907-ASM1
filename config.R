# config.R
# Central configuration for HK stock-selection pipeline

load_required_packages <- function(attach = TRUE) {
  pkgs <- c("quantmod", "tidyverse", "lubridate", "broom", "zoo")
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(paste0("Missing packages: ", paste(missing_pkgs, collapse = ", ")))
  }

  if (isTRUE(attach)) invisible(lapply(pkgs, library, character.only = TRUE))
  invisible(pkgs)
}

get_config <- function() {
  list(
    investment_hkd = 1000000,
    hkex_securities_list_url = "https://www.hkex.com.hk/eng/services/trading/securities/securitieslists/ListOfSecurities.csv",
    start_date = as.Date("2015-01-01"),
    end_date = Sys.Date(),
    target_horizon = 21,
    min_history_days = 252 * 3,
    max_missing_ratio = 0.20,
    min_avg_daily_volume = 100000,
    train_ratio = 0.7,
    max_predictors = 15,
    candidate_predictors = NULL,
    top_n_chart = 20,
    paths = list(
      logs = "logs",
      outputs_root = "outputs",
      stage1 = "outputs/stage1_universe",
      stage2 = "outputs/stage2_raw_data",
      stage3 = "outputs/stage3_features",
      stage4 = "outputs/stage4_model_search",
      stage5 = "outputs/stage5_predictions",
      stage6 = "outputs/stage6_reports"
    ),
    logs = list(
      master = "logs/pipeline_master.log",
      stage1 = "logs/stage1_universe.log",
      stage2 = "logs/stage2_raw_data.log",
      stage3 = "logs/stage3_features.log",
      stage4 = "logs/stage4_model_search.log",
      stage5 = "logs/stage5_predictions.log",
      stage6 = "logs/stage6_reports.log"
    ),
    files = list(
      stage1_universe = "outputs/stage1_universe/qualified_universe.csv",
      stage1_summary = "outputs/stage1_universe/universe_summary.txt",

      stage2_raw_panel = "outputs/stage2_raw_data/raw_panel.csv",
      stage2_failed = "outputs/stage2_raw_data/failed_stocks.csv",
      stage2_download_summary = "outputs/stage2_raw_data/download_summary.csv",

      stage3_model_panel = "outputs/stage3_features/model_panel.csv",
      stage3_feature_summary = "outputs/stage3_features/feature_summary.csv",

      stage4_all_models = "outputs/stage4_model_search/all_models.csv",
      stage4_best_model_summary = "outputs/stage4_model_search/best_model_summary.txt",
      stage4_best_predictors = "outputs/stage4_model_search/best_predictors.csv",
      stage4_split_info = "outputs/stage4_model_search/split_info.csv",
      stage4_pred_vs_actual = "outputs/stage4_model_search/best_model_pred_vs_actual.csv",

      stage5_predictions_latest = "outputs/stage5_predictions/predictions_latest.csv",
      stage5_latest_obs = "outputs/stage5_predictions/latest_observations_used.csv",

      stage6_ranked = "outputs/stage6_reports/ranked_stocks.csv",
      stage6_top_pick = "outputs/stage6_reports/top_pick.csv",
      stage6_final_report = "outputs/stage6_reports/final_report.txt"
    )
  )
}
