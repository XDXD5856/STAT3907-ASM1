# predict.R
# Stage 4 & 5: Evaluate on test set and predict next-period return by ticker.

run_prediction <- function(model_result, dataset, config) {
  # TODO:
  # 1) Evaluate selected model on test period
  # 2) Predict next 21-day return for latest row of each ticker
  # 3) Return prediction table

  stop("run_prediction() not implemented yet.")
}

evaluate_on_test <- function(model, test_df, target_col) {
  # TODO: Compute test metrics.
  stop("evaluate_on_test() not implemented yet.")
}

predict_next_period_by_ticker <- function(model, latest_feature_df) {
  # TODO: Predict next-period return per ticker.
  stop("predict_next_period_by_ticker() not implemented yet.")
}
