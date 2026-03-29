# config.R
# Central configuration for assignment-style HK stock selection pipeline

load_required_packages <- function(attach = TRUE) {
  pkgs <- c("quantmod", "tidyverse", "lubridate", "broom", "zoo")
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) stop(paste0("Missing packages: ", paste(missing_pkgs, collapse = ", ")))
  if (isTRUE(attach)) {
    invisible(lapply(pkgs, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE))))
  }
  invisible(pkgs)
}

get_config <- function() {
  codes <- c(
    1,2,3,5,6,12,16,19,27,66,83,135,144,151,175,267,285,288,291,293,
    300,316,322,386,388,669,688,700,728,762,788,836,857,883,939,941,
    966,981,992,998,1024,1088,1093,1109,1113,1171,1177,1211,1299,1308,
    1336,1378,1398,1810,1929,2015,2020,2313,2318,2388,2601,2628,2888,
    3328,3690,3968,3988,6030,6690,6862,9618,9888,9987,9988,9992
  )

  list(
    stock_codes = codes,
    start_date = as.Date("2015-01-01"),
    end_date = Sys.Date(),
    target_horizon = 21,
    target_col = "target_log10_return",
    min_history_days = 252 * 3,
    max_missing_ratio = 0.20,
    min_avg_daily_volume = 100000,
    train_ratio = 0.7,
    valid_ratio = 0.15,
    max_predictors = 5,
    stage4_k_values = c(1, 2, 3, 4),
    stage4_ic_drop_tolerance = 0.02,
    stage4_max_per_group = 2,
    stage4_rmse_marginal_threshold = 0.01,
    candidate_predictors = NULL,
    verbose_console = TRUE,
    resume_run = TRUE,
    run_id = NULL,
    start_new_run = FALSE,
    refresh_features = FALSE,
    refresh_download = FALSE,
    refresh_model_search = FALSE,
    stage4_progress_every = 50,
    stage4_partial_save_every = 50,
    stage4_checkpoint_buffer_size = 20,
    stage4_use_rolling_window = FALSE,
    stage4_rolling_window_size = 252,
    stage4_include_time_index = FALSE,
    investment_hkd = 1000000,
    top_n_chart = 20,
    shared_data_path = "data",
    run_output_path = "results",
    paths = list(
      data_raw = "data/raw",
      data_processed = "data/processed",
      results = "results",
      report = "report",
      submission = "submission",
      logs = "logs"
    ),
    logs = list(
      master = "logs/pipeline_master.log",
      stage12 = "logs/stage1_2_data_download.log",
      stage3 = "logs/stage3_features.log",
      stage4 = "logs/stage4_model_search.log",
      stage5 = "logs/stage5_predictions.log",
      stage6 = "logs/stage6_report.log"
    ),
    files = list(
      stage12_universe = "data/raw/universe_tickers.csv",
      stage12_raw_panel = "data/raw/hk_stock_price_data.csv",
      stage12_failed = "data/raw/failed_stocks.csv",
      stage12_ticker_dir = "data/raw/tickers",

      stage3_ticker_dir = "data/processed/tickers_features",
      stage3_model_panel = "data/processed/feature_panel.csv",
      stage3_summary = "data/processed/feature_summary.csv",
      stage3_plot_target = "results/stage3_target_21d_distribution.png",
      stage3_plot_ret1d = "results/stage3_ret_1d_distribution.png",

      stage4_all_models = "results/all_models.csv",
      stage4_model_cache = "data/model_cache/model_results.csv",
      stage4_best_model_summary = "results/best_model_summary.txt",
      stage4_summary = "results/stage4_summary.txt",
      stage4_model_complexity_summary = "results/stage4_model_complexity_summary.csv",
      stage4_best_predictors = "results/best_predictors.csv",
      stage4_split_info = "results/split_info.csv",
      stage4_backtest = "results/backtest_pred_vs_actual.csv",
      stage4_pred_vs_actual = "results/predicted_vs_actual.csv",
      stage4_plot_rmse = "results/rmse_by_model_rank.png",
      stage4_plot_top20 = "results/top20_models_bar.png",
      stage4_plot_scatter = "results/best_model_pred_vs_actual.png",
      stage4_all_models_partial = "outputs/stage4_model_search/all_models_partial.csv",

      stage5_predictions = "results/predictions_latest.csv",
      stage5_latest_obs = "results/latest_observations_used.csv",
      stage5_hist = "results/predictions_histogram.png",
      stage5_top20 = "results/top20_predicted_chart.png",

      stage6_ranked = "report/ranked_stocks.csv",
      stage6_top_pick = "report/top_pick.csv",
      stage6_final_report = "report/final_report.txt",
      stage6_cumlog10_plot = "report/cumulative_log10_return.png",
      stage6_monthly_return_plot = "report/monthly_return.png",
      stage6_backtest_results = "report/backtest_results.csv",
      stage6_backtest_summary = "report/backtest_summary.csv",
      submission_ranked = "submission/ranked_stocks.csv",
      submission_top_pick = "submission/top_pick.csv",
      submission_report = "submission/final_report.txt"
    )
  )
}
