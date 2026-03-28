# main.R
# Entry point for HK stock selection pipeline

source("config.R")
source("data_loader.R")
source("feature_engineering.R")
source("model_search.R")
source("predict.R")
source("report.R")

main <- function() {
  message("Starting pipeline...")

  config <- get_config()
  load_required_packages(attach = TRUE)
  initialize_project_dirs(config)

  # Stage 1: Universe construction
  stock_list_df <- construct_qualified_universe(config)
  utils::write.csv(stock_list_df, config$files$universe, row.names = FALSE)
  message(sprintf("Stage 1 complete: %d qualified stocks", nrow(stock_list_df)))

  # Stage 2: Data download
  raw_panel_df <- load_and_clean_data(stock_list_df, config)
  utils::write.csv(raw_panel_df, config$files$raw_panel, row.names = FALSE)
  message(sprintf("Stage 2 complete: %d panel rows", nrow(raw_panel_df)))

  # Stage 3: Feature engineering
  model_panel_df <- build_features_and_target(raw_panel_df, config)
  utils::write.csv(model_panel_df, config$files$model_panel, row.names = FALSE)
  message(sprintf("Stage 3 complete: %d model rows", nrow(model_panel_df)))

  # Stage 4: Model selection
  model_result <- search_best_model(
    dataset = model_panel_df,
    config = config,
    target_col = "target_21d",
    candidate_predictors = config$candidate_predictors,
    date_col = "date",
    ticker_col = "ticker"
  )
  utils::write.csv(model_result$all_models, config$files$all_models, row.names = FALSE)
  message("Stage 4 complete: best model selected")

  # Stage 5: Prediction
  prediction_result <- run_prediction(model_result, model_panel_df, config)
  utils::write.csv(prediction_result, config$files$predictions, row.names = FALSE)
  message(sprintf("Stage 5 complete: %d stock predictions", nrow(prediction_result)))

  # Stage 6: Ranking
  report_result <- generate_report(prediction_result, raw_panel_df, config)
  message("Stage 6 complete: ranking and top pick generated")

  message("Pipeline finished.")

  list(
    config = config,
    universe = stock_list_df,
    raw_panel = raw_panel_df,
    model_panel = model_panel_df,
    model_result = model_result,
    prediction_result = prediction_result,
    report_result = report_result
  )
}

if (sys.nframe() == 0) {
  main()
}
