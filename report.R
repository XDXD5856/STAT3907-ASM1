# report.R
# Stage 6

rank_stocks <- function(prediction_df) {
  r <- prediction_df
  if (!"company_name" %in% names(r)) r$company_name <- r$ticker
  if (!"volatility_21" %in% names(r)) r$volatility_21 <- NA_real_
  r$risk_adjusted_score <- r$predicted_return_21d / (r$volatility_21 + 1e-6)
  r <- r[order(-r$risk_adjusted_score), , drop = FALSE]
  r$rank <- seq_len(nrow(r))
  r[, c("rank", "ticker", "stock_code", "company_name", "predicted_return_21d", "volatility_21", "risk_adjusted_score"), drop = FALSE]
}

select_top_stock <- function(ranked_df, latest_price_df, investment_hkd) {
  top <- ranked_df[1, , drop = FALSE]
  px <- as.numeric(latest_price_df$close[latest_price_df$ticker == top$ticker][1])
  shares <- floor(investment_hkd / px)
  cash_left <- investment_hkd - shares * px
  data.frame(
    ticker = top$ticker,
    stock_code = top$stock_code,
    company_name = top$company_name,
    predicted_return_21d = top$predicted_return_21d,
    volatility_21 = top$volatility_21,
    risk_adjusted_score = top$risk_adjusted_score,
    latest_close = px,
    investment_hkd = investment_hkd,
    shares_to_buy = shares,
    cash_left_hkd = cash_left,
    stringsAsFactors = FALSE
  )
}

generate_report <- function(prediction_result, raw_panel_df, model_result, universe_df, config) {
  write_stage_log("stage6", "Stage 6 started", config)

  ranked <- rank_stocks(prediction_result)
  utils::write.csv(ranked, config$files$stage6_ranked, row.names = FALSE)

  latest_px <- raw_panel_df[order(raw_panel_df$ticker, raw_panel_df$date), c("ticker", "close")]
  latest_px <- latest_px[!duplicated(latest_px$ticker, fromLast = TRUE), , drop = FALSE]
  top_pick <- select_top_stock(ranked, latest_px, config$investment_hkd)
  utils::write.csv(top_pick, config$files$stage6_top_pick, row.names = FALSE)

  top10 <- head(ranked, 10)
  lines <- c(
    "HK Stock Selection Final Report",
    paste0("total qualified stocks: ", nrow(universe_df)),
    paste0("total downloaded stocks: ", length(unique(raw_panel_df$ticker))),
    paste0("best model: ", deparse(model_result$best_formula)),
    paste0("best predictors: ", paste(model_result$best_predictors, collapse = ", ")),
    paste0("best RMSE: ", model_result$best_metrics$valid_rmse[1]),
    "top 10 ranked stocks:",
    paste(apply(top10[, c("rank", "ticker", "company_name", "predicted_return_21d", "risk_adjusted_score")], 1, paste, collapse = " | "), collapse = "\n"),
    paste0("final stock choice for HKD 1,000,000: ", top_pick$ticker, " (", top_pick$company_name, ")")
  )
  writeLines(lines, config$files$stage6_final_report)

  file.copy(config$files$stage6_ranked, config$files$submission_ranked, overwrite = TRUE)
  file.copy(config$files$stage6_top_pick, config$files$submission_top_pick, overwrite = TRUE)
  file.copy(config$files$stage6_final_report, config$files$submission_report, overwrite = TRUE)

  write_stage_log("stage6", paste0("Stage 6 completed: top_pick=", top_pick$ticker), config)
  list(ranked = ranked, top_pick = top_pick)
}

load_stage6_outputs <- function(config) {
  if (!file.exists(config$files$stage6_ranked) || !file.exists(config$files$stage6_top_pick)) stop("Stage 6 cached outputs not found")
  list(
    ranked = utils::read.csv(config$files$stage6_ranked, stringsAsFactors = FALSE),
    top_pick = utils::read.csv(config$files$stage6_top_pick, stringsAsFactors = FALSE)
  )
}
