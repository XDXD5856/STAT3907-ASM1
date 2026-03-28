# feature_engineering.R
# Stage 2: Create features and next 21-trading-day log return target by ticker.

build_features_and_target <- function(data_list, config) {
  # TODO:
  # 1) Apply feature creation for each ticker
  # 2) Create target = next 21-day log return
  # 3) Combine into modeling dataset

  stop("build_features_and_target() not implemented yet.")
}

create_ticker_features <- function(df, ticker, horizon = 21) {
  # TODO: Build technical/statistical features from daily data.
  stop("create_ticker_features() not implemented yet.")
}

create_ticker_target <- function(df, horizon = 21) {
  # TODO: Create forward 21-trading-day log return target.
  stop("create_ticker_target() not implemented yet.")
}
