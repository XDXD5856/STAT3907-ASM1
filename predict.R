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

  latest_df <- get_latest_observation_by_ticker(model_panel_df)
  predictors <- model_result$best_predictors

  pred_ready <- latest_df[complete.cases(latest_df[, predictors, drop = FALSE]), , drop = FALSE]
  pred <- as.numeric(predict(model_result$best_model, newdata = pred_ready))

  out <- data.frame(
    ticker = pred_ready$ticker,
    stock_code = if ("stock_code" %in% names(pred_ready)) pred_ready$stock_code else NA_character_,
    company_name = if ("company_name" %in% names(pred_ready)) pred_ready$company_name else NA_character_,
    prediction_date = pred_ready$date,
    predicted_return_21d = pred,
    stringsAsFactors = FALSE
  )

  utils::write.csv(out, config$files$stage5_predictions_latest, row.names = FALSE)
  utils::write.csv(pred_ready, config$files$stage5_latest_obs, row.names = FALSE)

  save_stage_plot(
    plot_expr = function() hist(out$predicted_return_21d, breaks = 40, col = "lightblue", main = "Predicted 21d return", xlab = "predicted_return_21d"),
    file_path = file.path(config$paths$stage5, "predicted_return_hist.png")
  )

  top_n <- min(config$top_n_chart, nrow(out))
  top_df <- out[order(-out$predicted_return_21d), ][1:top_n, , drop = FALSE]
  save_stage_plot(
    plot_expr = function() {
      barplot(rev(top_df$predicted_return_21d), names.arg = rev(top_df$ticker), horiz = TRUE, las = 1, col = "orange", main = "Top predicted stocks")
    },
    file_path = file.path(config$paths$stage5, "top20_predicted_chart.png")
  )

  write_stage_log("stage5", paste0("Stage 5 completed: predictions=", nrow(out)), config)
  out
}
