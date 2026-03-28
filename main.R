# main.R
# Entry point for the Hong Kong stock selection pipeline.

source("config.R")
source("data_loader.R")
source("feature_engineering.R")
source("model_search.R")
source("predict.R")
source("report.R")

main <- function() {
  message("Starting pipeline...")

  # Stage 1: Download and clean data
  # data_list <- load_and_clean_data(config)

  # Stage 2: Feature engineering and target creation
  # dataset <- build_features_and_target(data_list, config)

  # Stage 3: Model search with for-loops and time-series split
  # model_result <- search_best_model(dataset, config)

  # Stage 4/5: Evaluate and predict next period return
  # prediction_result <- run_prediction(model_result, dataset, config)

  # Stage 6: Rank and export
  # report_paths <- generate_report(prediction_result, config)

  message("Pipeline structure created. Logic implementation pending.")
}

if (sys.nframe() == 0) {
  main()
}
