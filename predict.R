# predict.R
# Stage 5

get_latest_observation_by_ticker <- function(model_panel_df) {
  out <- list()
  for (tk in unique(model_panel_df$ticker)) {
    d <- model_panel_df[model_panel_df$ticker == tk, , drop = FALSE]
    d <- d[order(d$date), , drop = FALSE]
    if (nrow(d) > 0) out[[length(out) + 1]] <- d[nrow(d), , drop = FALSE]
  }
  do.call(rbind, out)
}

run_prediction <- function(model_result, model_panel_df, config) {
  write_stage_log("stage5", "Stage 5 started", config)

  latest <- get_latest_observation_by_ticker(model_panel_df)
  preds <- model_result$best_predictors
  ready <- latest[complete.cases(latest[, preds, drop = FALSE]), , drop = FALSE]

  pred <- as.numeric(predict(model_result$best_model, newdata = ready))
  company_name <- if ("company_name" %in% names(ready)) ready$company_name else ready$ticker
  volatility_col <- if ("volatility_21" %in% names(ready)) ready$volatility_21 else rep(NA_real_, nrow(ready))
  out <- data.frame(
    ticker = ready$ticker,
    stock_code = ready$stock_code,
    company_name = company_name,
    prediction_date = ready$date,
    predicted_return_21d = pred,
    volatility_21 = volatility_col,
    stringsAsFactors = FALSE
  )

  safe_write_csv(out, config$files$stage5_predictions)
  safe_write_csv(ready, config$files$stage5_latest_obs)

  save_stage_plot(function() hist(out$predicted_return_21d, breaks = 40, col = "lightblue", main = "Predicted return", xlab = "predicted_return_21d"), config$files$stage5_hist)
  top_n <- min(config$top_n_chart, nrow(out)); top_df <- out[order(-out$predicted_return_21d), ][1:top_n, , drop = FALSE]
  save_stage_plot(function() barplot(rev(top_df$predicted_return_21d), names.arg = rev(top_df$ticker), horiz = TRUE, las = 1, col = "orange", main = "Top predicted"), config$files$stage5_top20)

  write_stage_log("stage5", paste0("Stage 5 completed: predictions=", nrow(out)), config)
  out
}

load_stage5_outputs <- function(config) {
  if (!file.exists(config$files$stage5_predictions)) stop("Stage 5 cached output not found")
  pred_cached <- utils::read.csv(config$files$stage5_predictions, stringsAsFactors = FALSE)
  if ("prediction_date" %in% names(pred_cached)) pred_cached$prediction_date <- as.Date(pred_cached$prediction_date)
  pred_cached
}
