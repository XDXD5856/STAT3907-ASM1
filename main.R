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
  config <- initialize_run_dirs(config)
  create_output_dirs(config)
  load_required_packages(TRUE)
  write_run_manifest(config)

  write_master_log("Pipeline started", config)
  write_master_log(paste0("Run ID: ", config$run_id), config)

  if (isTRUE(config$resume_run) && is_stage_done("stage12", config) && file.exists(config$files$stage12_raw_panel)) {
    write_master_log("Running Stage 1+2", config)
    raw_panel <- load_stage12_outputs(config)
    universe_df <- unique(raw_panel[, c("stock_code", "ticker")])
    write_master_log(paste0("Stage 1+2 done: reused existing output rows=", nrow(raw_panel)), config)
  } else {
    write_master_log("Running Stage 1+2", config)
    raw_panel <- load_and_clean_data(config)
    universe_df <- unique(raw_panel[, c("stock_code", "ticker")])
    mark_stage_done("stage12", config)
    write_run_manifest(config)
    write_master_log(paste0("Stage 1+2 done: tickers=", nrow(universe_df), ", rows=", nrow(raw_panel)), config)
  }

  if (isTRUE(config$resume_run) && is_stage_done("stage3", config) && file.exists(config$files$stage3_model_panel)) {
    write_master_log("Running Stage 3", config)
    model_panel <- load_stage3_outputs(config)
    write_master_log(paste0("Stage 3 done: reused existing output rows=", nrow(model_panel)), config)
  } else {
    write_master_log("Running Stage 3", config)
    model_panel <- build_features_and_target(raw_panel, config)
    mark_stage_done("stage3", config)
    write_run_manifest(config)
    write_master_log(paste0("Stage 3 done: rows=", nrow(model_panel)), config)
  }

  if (isTRUE(config$resume_run) && is_stage_done("stage4", config) && file.exists(config$files$stage4_all_models) && file.exists(config$files$stage4_best_predictors)) {
    write_master_log("Running Stage 4", config)
    model_result <- load_stage4_result(model_panel, config)
    write_master_log(paste0("Stage 4 done: reused existing best_rmse=", model_result$best_metrics$valid_rmse[1], ", test_rmse=", model_result$backtest_metrics$test_rmse[1], ", k=", model_result$selected_k), config)
  } else {
    write_master_log("Running Stage 4", config)
    model_result <- search_best_model(model_panel, config)
    mark_stage_done("stage4", config)
    write_run_manifest(config)
    write_master_log(paste0("Stage 4 done: best_rmse=", model_result$best_metrics$valid_rmse[1], ", test_rmse=", model_result$backtest_metrics$test_rmse[1], ", k=", model_result$selected_k), config)
  }

  if (isTRUE(config$resume_run) && is_stage_done("stage5", config) && file.exists(config$files$stage5_predictions)) {
    write_master_log("Running Stage 5", config)
    prediction <- load_stage5_outputs(config)
    write_master_log(paste0("Stage 5 done: reused existing predictions=", nrow(prediction)), config)
  } else {
    write_master_log("Running Stage 5", config)
    prediction <- run_prediction(model_result, model_panel, config)
    mark_stage_done("stage5", config)
    write_run_manifest(config)
    write_master_log(paste0("Stage 5 done: predictions=", nrow(prediction)), config)
  }

  if (isTRUE(config$resume_run) && is_stage_done("stage6", config) && file.exists(config$files$stage6_ranked) && file.exists(config$files$stage6_top_pick)) {
    write_master_log("Running Stage 6", config)
    report <- load_stage6_outputs(config)
    write_master_log(paste0("Stage 6 done: reused existing top_pick=", report$top_pick$ticker[1]), config)
  } else {
    write_master_log("Running Stage 6", config)
    report <- generate_report(prediction, raw_panel, model_result, universe_df, model_panel, config)
    mark_stage_done("stage6", config)
    write_run_manifest(config)
    write_master_log(paste0("Stage 6 done: top_pick=", report$top_pick$ticker), config)
  }

  write_master_log("Pipeline finished", config)
  write_run_manifest(config, end_time = Sys.time())
  list(universe = universe_df, raw_panel = raw_panel, model_panel = model_panel, model_result = model_result, prediction = prediction, report = report)
}

if (sys.nframe() == 0) invisible(main())
