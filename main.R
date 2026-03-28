# main.R

source("config.R")
source("utils_io.R")
source("data_loader.R")
source("feature_engineering.R")
source("model_search.R")
source("predict.R")
source("report.R")

main <- function() {
  config <- get_config()
  create_output_dirs(config)
  load_required_packages(TRUE)

  write_master_log("Pipeline started", config)

  write_master_log("Running Stage 1+2", config)
  raw_panel <- load_and_clean_data(config)
  universe_df <- unique(raw_panel[, c("stock_code", "ticker")])
  write_master_log(paste0("Stage 1+2 done: tickers=", nrow(universe_df), ", rows=", nrow(raw_panel)), config)

  write_master_log("Running Stage 3", config)
  model_panel <- build_features_and_target(raw_panel, config)
  write_master_log(paste0("Stage 3 done: rows=", nrow(model_panel)), config)

  write_master_log("Running Stage 4", config)
  model_result <- search_best_model(model_panel, config)
  write_master_log(paste0("Stage 4 done: best_rmse=", model_result$best_metrics$valid_rmse[1]), config)

  write_master_log("Running Stage 5", config)
  prediction <- run_prediction(model_result, model_panel, config)
  write_master_log(paste0("Stage 5 done: predictions=", nrow(prediction)), config)

  write_master_log("Running Stage 6", config)
  report <- generate_report(prediction, raw_panel, model_result, universe_df, config)
  write_master_log(paste0("Stage 6 done: top_pick=", report$top_pick$ticker), config)

  write_master_log("Pipeline finished", config)
  list(universe = universe_df, raw_panel = raw_panel, model_panel = model_panel, model_result = model_result, prediction = prediction, report = report)
}

if (sys.nframe() == 0) main()
