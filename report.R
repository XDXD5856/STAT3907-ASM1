# report.R
# Stage 6: Rank stocks and identify final investment choice

rank_stocks <- function(prediction_df) {
  if (!all(c("ticker", "prediction_date", "predicted_return_21d") %in% names(prediction_df))) {
    stop("prediction_df must contain ticker, prediction_date, predicted_return_21d")
  }

  ranked <- prediction_df[order(-prediction_df$predicted_return_21d), , drop = FALSE]
  ranked$rank <- seq_len(nrow(ranked))
  ranked <- ranked[, c("rank", "ticker", "prediction_date", "predicted_return_21d")]
  rownames(ranked) <- NULL
  ranked
}

select_top_stock <- function(ranked_df, latest_price_df, investment_hkd = 1000000) {
  if (nrow(ranked_df) == 0) {
    stop("ranked_df is empty")
  }

  top_ticker <- ranked_df$ticker[1]
  top_pred <- ranked_df$predicted_return_21d[1]

  price_row <- latest_price_df[latest_price_df$ticker == top_ticker, , drop = FALSE]
  if (nrow(price_row) == 0 || is.na(price_row$close[1]) || price_row$close[1] <= 0) {
    stop("Missing valid latest close price for top stock.")
  }

  px <- as.numeric(price_row$close[1])
  shares <- floor(investment_hkd / px)
  cash_left <- investment_hkd - shares * px

  data.frame(
    ticker = top_ticker,
    predicted_return_21d = top_pred,
    latest_close = px,
    investment_hkd = investment_hkd,
    shares_to_buy = shares,
    cash_left_hkd = cash_left,
    stringsAsFactors = FALSE
  )
}

generate_report <- function(prediction_result, raw_panel_df, config) {
  ranked <- rank_stocks(prediction_result)

  latest_price <- raw_panel_df[order(raw_panel_df$ticker, raw_panel_df$date), c("ticker", "date", "close")]
  latest_price <- latest_price[!duplicated(latest_price$ticker, fromLast = TRUE), , drop = FALSE]

  top_pick <- select_top_stock(
    ranked_df = ranked,
    latest_price_df = latest_price,
    investment_hkd = config$investment_hkd
  )

  utils::write.csv(ranked, config$files$ranking, row.names = FALSE)
  utils::write.csv(top_pick, config$files$top_pick, row.names = FALSE)

  list(
    ranked = ranked,
    top_pick = top_pick,
    ranked_path = config$files$ranking,
    top_pick_path = config$files$top_pick
  )
}
