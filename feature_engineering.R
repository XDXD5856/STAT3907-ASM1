# feature_engineering.R
# Stage 3: Feature engineering and target creation

safe_log_return <- function(x, lag_n) {
  out <- rep(NA_real_, length(x))
  if (length(x) <= lag_n) return(out)

  for (i in (lag_n + 1):length(x)) {
    if (is.na(x[i]) || is.na(x[i - lag_n]) || x[i] <= 0 || x[i - lag_n] <= 0) {
      out[i] <- NA_real_
    } else {
      out[i] <- log(x[i] / x[i - lag_n])
    }
  }
  out
}

forward_log_return <- function(x, horizon) {
  n <- length(x)
  out <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    j <- i + horizon
    if (j > n) {
      out[i] <- NA_real_
    } else if (is.na(x[i]) || is.na(x[j]) || x[i] <= 0 || x[j] <= 0) {
      out[i] <- NA_real_
    } else {
      out[i] <- log(x[j] / x[i])
    }
  }

  out
}

rolling_sd <- function(x, k) {
  n <- length(x)
  out <- rep(NA_real_, n)

  if (n < k) return(out)

  for (i in k:n) {
    out[i] <- sd(x[(i - k + 1):i], na.rm = TRUE)
  }
  out
}

rolling_mean <- function(x, k) {
  n <- length(x)
  out <- rep(NA_real_, n)

  if (n < k) return(out)

  for (i in k:n) {
    out[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
  }
  out
}

create_ticker_features <- function(df_ticker, horizon = 21) {
  df <- df_ticker[order(df_ticker$date), , drop = FALSE]

  close <- df$close
  volume <- df$volume
  high <- df$high
  low <- df$low
  open <- df$open

  df$ret_1d <- safe_log_return(close, 1)
  df$ret_3d <- safe_log_return(close, 3)
  df$ret_5d <- safe_log_return(close, 5)
  df$ret_10d <- safe_log_return(close, 10)
  df$ret_21d_past <- safe_log_return(close, 21)

  df$ln_close <- ifelse(close > 0, log(close), NA_real_)
  df$vol_chg_1d <- safe_log_return(pmax(volume, 1), 1)
  df$price_range <- (high - low) / pmax(close, 1e-8)
  df$oc_return <- ifelse(open > 0 & close > 0, log(close / open), NA_real_)

  ma_5 <- rolling_mean(close, 5)
  ma_10 <- rolling_mean(close, 10)
  ma_21 <- rolling_mean(close, 21)
  ma_63 <- rolling_mean(close, 63)

  df$ma_ratio_5_21 <- ma_5 / ma_21
  df$ma_ratio_10_21 <- ma_10 / ma_21
  df$ma_ratio_21_63 <- ma_21 / ma_63

  df$volatility_5 <- rolling_sd(df$ret_1d, 5)
  df$volatility_21 <- rolling_sd(df$ret_1d, 21)
  df$volatility_63 <- rolling_sd(df$ret_1d, 63)

  vol_ma_21 <- rolling_mean(volume, 21)
  df$volume_z_21 <- (volume - vol_ma_21) / pmax(rolling_sd(volume, 21), 1e-8)

  # target: future 21-day log return
  df$target_21d <- forward_log_return(close, horizon)

  df
}

build_features_and_target <- function(panel_df, config) {
  required_cols <- c("ticker", "date", "open", "high", "low", "close", "volume", "adjusted_close")
  miss <- setdiff(required_cols, names(panel_df))
  if (length(miss) > 0) {
    stop(sprintf("panel_df missing required columns: %s", paste(miss, collapse = ", ")))
  }

  tickers <- unique(panel_df$ticker)
  out_parts <- list()

  for (tk in tickers) {
    df_tk <- panel_df[panel_df$ticker == tk, , drop = FALSE]
    if (nrow(df_tk) < (config$target_horizon + 30)) next

    feat_df <- create_ticker_features(df_tk, horizon = config$target_horizon)
    out_parts[[length(out_parts) + 1]] <- feat_df
  }

  if (length(out_parts) == 0) {
    stop("No ticker has enough history to build features/target.")
  }

  model_df <- do.call(rbind, out_parts)
  model_df <- model_df[order(model_df$ticker, model_df$date), , drop = FALSE]

  # keep rows with valid target and at least one feature
  feature_cols <- setdiff(names(model_df), c("ticker", "date", "target_21d"))
  numeric_feature_cols <- feature_cols[vapply(model_df[feature_cols], is.numeric, logical(1))]

  has_any_feature <- rep(FALSE, nrow(model_df))
  for (cname in numeric_feature_cols) {
    has_any_feature <- has_any_feature | !is.na(model_df[[cname]])
  }

  model_df <- model_df[!is.na(model_df$target_21d) & has_any_feature, , drop = FALSE]
  rownames(model_df) <- NULL
  model_df
}
