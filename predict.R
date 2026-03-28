# predict.R
# Stage 5: Predict future 21-day return for latest observation of each qualified stock

get_latest_observation_by_ticker <- function(model_panel_df) {
  tickers <- unique(model_panel_df$ticker)
  out <- list()

  for (tk in tickers) {
    part <- model_panel_df[model_panel_df$ticker == tk, , drop = FALSE]
    part <- part[order(part$date), , drop = FALSE]
    if (nrow(part) == 0) next
    out[[length(out) + 1]] <- part[nrow(part), , drop = FALSE]
  }

  if (length(out) == 0) {
    stop("No latest observations found for prediction.")
  }

  do.call(rbind, out)
}

run_prediction <- function(model_result, model_panel_df, config) {
  if (is.null(model_result$best_model)) {
    stop("model_result$best_model is missing.")
  }

  latest_df <- get_latest_observation_by_ticker(model_panel_df)

  predictors <- model_result$best_predictors
  required <- unique(c("ticker", "date", predictors))

  missing_cols <- setdiff(required, names(latest_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns for prediction: %s", paste(missing_cols, collapse = ", ")))
  }

  pred_ready <- latest_df[complete.cases(latest_df[, predictors, drop = FALSE]), , drop = FALSE]
  if (nrow(pred_ready) == 0) {
    stop("No valid latest rows remain after predictor complete.cases filter.")
  }

  pred <- as.numeric(predict(model_result$best_model, newdata = pred_ready))

  out <- data.frame(
    ticker = pred_ready$ticker,
    prediction_date = pred_ready$date,
    predicted_return_21d = pred,
    stringsAsFactors = FALSE
  )

  out <- out[order(out$ticker), , drop = FALSE]
  rownames(out) <- NULL
  out
}
