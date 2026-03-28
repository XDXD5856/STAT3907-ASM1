# data_loader.R
# Stage 1: Download and clean historical daily data by ticker.

load_and_clean_data <- function(config) {
  # TODO:
  # 1) Loop through config$tickers
  # 2) Download OHLCV using quantmod
  # 3) Clean missing values / align dates
  # 4) Return named list by ticker

  stop("load_and_clean_data() not implemented yet.")
}

fetch_single_ticker <- function(ticker, start_date, end_date) {
  # TODO: Download single ticker data from data source.
  stop("fetch_single_ticker() not implemented yet.")
}

clean_single_ticker_data <- function(df, ticker) {
  # TODO: Standardize columns and handle missing values.
  stop("clean_single_ticker_data() not implemented yet.")
}
