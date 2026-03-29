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

run_assignment_backtest <- function(model_panel_df, model_result, config, target_col = "target_log10_return") {
  factor_rows <- model_panel_df[!is.na(model_panel_df[[target_col]]), , drop = FALSE]
  sigs <- sort(unique(factor_rows$signal_date))
  out <- list()
  for (sd in sigs) {
    train <- factor_rows[factor_rows$signal_date < sd, , drop = FALSE]
    score <- factor_rows[factor_rows$signal_date == sd, , drop = FALSE]
    preds <- model_result$best_predictors
    if (nrow(train) < (length(preds) + 5) || nrow(score) == 0) next
    tr <- train[complete.cases(train[, c(target_col, preds), drop = FALSE]), c(target_col, preds), drop = FALSE]
    sc <- score[complete.cases(score[, preds, drop = FALSE]), , drop = FALSE]
    if (nrow(tr) < (length(preds) + 5) || nrow(sc) == 0) next
    fit <- lm(as.formula(paste(target_col, "~", paste(preds, collapse = " + "))), data = tr)
    sc$pred <- as.numeric(predict(fit, newdata = sc))
    sc <- sc[order(-sc$pred), , drop = FALSE]
    top <- sc[1, , drop = FALSE]
    out[[length(out) + 1]] <- data.frame(
      signal_date = sd,
      exit_date = top$exit_date,
      ticker = top$ticker,
      company_name = if ("company_name" %in% names(top)) top$company_name else top$ticker,
      predicted_return = top$pred,
      realized_return = top[[target_col]],
      benchmark_equal_weight = mean(sc[[target_col]], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

generate_report <- function(prediction_result, raw_panel_df, model_result, universe_df, model_panel_df, config) {
  write_stage_log("stage6", "Stage 6 started", config)

  ranked <- rank_stocks(prediction_result)
  safe_write_csv(ranked, config$files$stage6_ranked)

  latest_px <- raw_panel_df[order(raw_panel_df$ticker, raw_panel_df$date), c("ticker", "close")]
  latest_px <- latest_px[!duplicated(latest_px$ticker, fromLast = TRUE), , drop = FALSE]
  top_pick <- select_top_stock(ranked, latest_px, config$investment_hkd)
  safe_write_csv(top_pick, config$files$stage6_top_pick)

  top10 <- head(ranked, 10)
  ret_base <- pmax(1 + ranked$predicted_return_21d, 1e-8)
  ranked$cumulative_log10_return <- cumsum(log10(ret_base))
  save_stage_plot(
    function() plot(
      ranked$rank,
      ranked$cumulative_log10_return,
      type = "l",
      lwd = 2,
      col = "steelblue",
      xlab = "Rank",
      ylab = "Cumulative log10 return",
      main = "Cumulative log10 return by ranked stocks"
    ),
    config$files$stage6_cumlog10_plot
  )
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
  safe_write_lines(lines, config$files$stage6_final_report)

  bt <- run_assignment_backtest(model_panel_df, model_result, config, target_col = config$target_col)
  if (nrow(bt) > 0) {
    bt$cum_strategy <- cumsum(bt$realized_return)
    bt$cum_benchmark <- cumsum(bt$benchmark_equal_weight)
    safe_write_csv(bt, config$files$stage6_backtest_results)
    bt_summary <- data.frame(
      periods = nrow(bt),
      cumulative_return = sum(bt$realized_return, na.rm = TRUE),
      benchmark_cumulative_return = sum(bt$benchmark_equal_weight, na.rm = TRUE),
      strategy_mean = mean(bt$realized_return, na.rm = TRUE),
      benchmark_mean = mean(bt$benchmark_equal_weight, na.rm = TRUE),
      hit_rate = mean(bt$realized_return > 0, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    safe_write_csv(bt_summary, config$files$stage6_backtest_summary)
    bt$month <- format(as.Date(bt$signal_date), "%Y-%m")
    monthly <- aggregate(cbind(realized_return, benchmark_equal_weight) ~ month, data = bt, FUN = mean)
    save_stage_plot(function() barplot(monthly$realized_return, names.arg = monthly$month, las = 2, col = "steelblue", main = "Monthly Strategy Return"), config$files$stage6_monthly_return_plot)
    save_stage_plot(function() { plot(bt$signal_date, bt$cum_strategy, type = "l", col = "blue", lwd = 2, xlab = "Signal date", ylab = "Cumulative return"); lines(bt$signal_date, bt$cum_benchmark, col = "darkgray", lwd = 2); legend("topleft", legend = c("Strategy", "Equal-weight"), col = c("blue", "darkgray"), lty = 1, bty = "n") }, config$files$stage6_cumlog10_plot)
  }

  file.copy(config$files$stage6_ranked, config$files$submission_ranked, overwrite = TRUE)
  file.copy(config$files$stage6_top_pick, config$files$submission_top_pick, overwrite = TRUE)
  file.copy(config$files$stage6_final_report, config$files$submission_report, overwrite = TRUE)

  write_stage_log("stage6", paste0("Stage 6 completed: top_pick=", top_pick$ticker), config)
  list(ranked = ranked, top_pick = top_pick, backtest = bt)
}

load_stage6_outputs <- function(config) {
  if (!file.exists(config$files$stage6_ranked) || !file.exists(config$files$stage6_top_pick)) stop("Stage 6 cached outputs not found")
  list(
    ranked = utils::read.csv(config$files$stage6_ranked, stringsAsFactors = FALSE),
    top_pick = utils::read.csv(config$files$stage6_top_pick, stringsAsFactors = FALSE)
  )
}

load_stage6_outputs <- function(config) {
  if (!file.exists(config$files$stage6_ranked) || !file.exists(config$files$stage6_top_pick)) stop("Stage 6 cached outputs not found")
  list(
    ranked = utils::read.csv(config$files$stage6_ranked, stringsAsFactors = FALSE),
    top_pick = utils::read.csv(config$files$stage6_top_pick, stringsAsFactors = FALSE)
  )
}
