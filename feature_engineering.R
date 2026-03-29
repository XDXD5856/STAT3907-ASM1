# feature_engineering.R
# Stage 3

safe_log_return <- function(x, lag_n) {
  out <- rep(NA_real_, length(x))
  if (length(x) <= lag_n) return(out)
  for (i in (lag_n + 1):length(x)) {
    if (!is.na(x[i]) && !is.na(x[i - lag_n]) && x[i] > 0 && x[i - lag_n] > 0) out[i] <- log(x[i] / x[i - lag_n])
  }
  out
}

forward_log_return <- function(x, horizon) {
  n <- length(x); out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    j <- i + horizon
    if (j <= n && !is.na(x[i]) && !is.na(x[j]) && x[i] > 0 && x[j] > 0) out[i] <- log(x[j] / x[i])
  }
  out
}

rolling_mean <- function(x, k) {
  out <- rep(NA_real_, length(x)); if (length(x) < k) return(out)
  for (i in k:length(x)) out[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
  out
}

rolling_sd <- function(x, k) {
  out <- rep(NA_real_, length(x)); if (length(x) < k) return(out)
  for (i in k:length(x)) out[i] <- sd(x[(i - k + 1):i], na.rm = TRUE)
  out
}

create_ticker_features <- function(d0, horizon = 21, include_time_index = FALSE) {
  d <- d0[order(d0$date), , drop = FALSE]
  cpx <- d$close; vol <- d$volume
  d$ret_1d <- safe_log_return(cpx, 1)
  d$ret_5d <- safe_log_return(cpx, 5)
  d$ret_10d <- safe_log_return(cpx, 10)
  d$ret_21d_past <- safe_log_return(cpx, 21)
  d$ln_close <- ifelse(cpx > 0, log(cpx), NA_real_)
  d$vol_chg_1d <- safe_log_return(pmax(vol, 1), 1)
  d$price_range <- (d$high - d$low) / pmax(cpx, 1e-8)
  d$oc_return <- ifelse(d$open > 0 & cpx > 0, log(cpx / d$open), NA_real_)
  d$ma_ratio_5_21 <- rolling_mean(cpx, 5) / rolling_mean(cpx, 21)
  d$volatility_21 <- rolling_sd(d$ret_1d, 21)
  d$volume_z_21 <- (vol - rolling_mean(vol, 21)) / pmax(rolling_sd(vol, 21), 1e-8)
  if (isTRUE(include_time_index)) d$time_index <- seq_len(nrow(d))
  d$target_21d <- forward_log_return(cpx, horizon)
  d
}

build_features_and_target <- function(raw_panel_df, config) {
  write_stage_log("stage3", "Stage 3 started", config)
  if (!isTRUE(config$refresh_features) && file.exists(config$files$stage3_model_panel)) {
    write_stage_log("stage3", "refresh_features=FALSE and feature panel exists; reusing local files", config)
    return(load_stage3_outputs(config))
  }

  signal_dates <- get_signal_dates(raw_panel_df$date)
  market_daily <- aggregate(ret_1d ~ date, data = build_daily_returns(raw_panel_df), FUN = function(x) mean(x, na.rm = TRUE))
  names(market_daily)[2] <- "mkt_ret_1d"
  market_prices <- aggregate(close ~ date, data = raw_panel_df, FUN = function(x) mean(x, na.rm = TRUE))
  names(market_prices)[2] <- "mkt_close"
  market_df <- merge(market_daily, market_prices, by = "date", all = TRUE)
  market_df <- market_df[order(market_df$date), , drop = FALSE]
  per_ticker <- list(); summary_rows <- list()
  for (tk in unique(raw_panel_df$ticker)) {
    d0 <- raw_panel_df[raw_panel_df$ticker == tk, , drop = FALSE]
    d0 <- d0[order(d0$date), , drop = FALSE]
    if (nrow(d0) < 260) next
    d1 <- build_assignment_factor_rows(d0, signal_dates, market_df, include_time_index = isTRUE(config$stage4_include_time_index))
    if (nrow(d1) == 0) next
    safe_write_csv(d1, file.path(config$files$stage3_ticker_dir, paste0(tk, "_features.csv")))
    fcols <- setdiff(names(d1), c("ticker", "stock_code", "signal_date", "exit_date"))
    summary_rows[[length(summary_rows) + 1]] <- data.frame(ticker = tk, rows = nrow(d1), missing_ratio = mean(!complete.cases(d1[, fcols, drop = FALSE])), stringsAsFactors = FALSE)
    per_ticker[[length(per_ticker) + 1]] <- d1
  }

  if (length(per_ticker) == 0) stop("No valid ticker for Stage 3")

  model_panel <- do.call(rbind, per_ticker)
  model_panel <- standardize_factors_cross_section(model_panel)
  model_panel <- model_panel[order(model_panel$signal_date, model_panel$ticker), , drop = FALSE]

  safe_write_csv(model_panel, config$files$stage3_model_panel)
  safe_write_csv(do.call(rbind, summary_rows), config$files$stage3_summary)

  save_stage_plot(function() hist(model_panel$target_log10_return, breaks = 60, col = "skyblue", main = "target_log10_return", xlab = "target_log10_return"), config$files$stage3_plot_target)
  save_stage_plot(function() hist(model_panel$mom_1m, breaks = 60, col = "tan", main = "mom_1m", xlab = "mom_1m"), config$files$stage3_plot_ret1d)

  write_stage_log("stage3", paste0("Stage 3 completed: rows=", nrow(model_panel), ", tickers=", length(unique(model_panel$ticker))), config)
  model_panel
}

build_daily_returns <- function(raw_panel_df) {
  out <- raw_panel_df[, c("ticker", "date", "close"), drop = FALSE]
  out <- out[order(out$ticker, out$date), , drop = FALSE]
  out$ret_1d <- ave(out$close, out$ticker, FUN = function(x) c(NA_real_, diff(log(pmax(x, 1e-8)))))
  out
}

get_signal_dates <- function(dates) {
  d <- sort(unique(as.Date(dates)))
  target <- d[as.integer(format(d, "%d")) <= 27]
  if (length(target) == 0) return(as.Date(character(0)))
  as.Date(unlist(lapply(split(target, format(target, "%Y-%m")), max)), origin = "1970-01-01")
}

get_last_trading_day_in_month <- function(dates, year_month) {
  d <- dates[format(dates, "%Y-%m") == year_month]
  if (length(d) == 0) return(NA)
  max(d)
}

build_assignment_factor_rows <- function(d0, signal_dates, market_df, include_time_index = FALSE) {
  d <- merge(d0, market_df, by = "date", all.x = TRUE)
  d <- d[order(d$date), , drop = FALSE]
  d$ret_1d <- c(NA_real_, diff(log(pmax(d$close, 1e-8))))
  rows <- list()
  for (sd in signal_dates) {
    idx <- which(d$date == sd)
    if (length(idx) == 0 || idx <= 252) next
    exit_idx <- idx + 21
    if (exit_idx > nrow(d)) next
    exit_date <- d$date[exit_idx]
    close_vec <- d$close
    vol_vec <- d$volume
    mkt_close <- d$mkt_close
    mkt_ret <- d$mkt_ret_1d
    stock_ret <- d$ret_1d
    r <- data.frame(
      ticker = d$ticker[idx],
      stock_code = d$stock_code[idx],
      signal_date = sd,
      exit_date = exit_date,
      entry_close = close_vec[idx],
      exit_close = close_vec[exit_idx],
      target_log10_return = log10(pmax(close_vec[exit_idx], 1e-8) / pmax(close_vec[idx], 1e-8)),
      mom_1m = log(pmax(close_vec[idx], 1e-8) / pmax(close_vec[idx - 21], 1e-8)),
      mom_3m = log(pmax(close_vec[idx], 1e-8) / pmax(close_vec[idx - 63], 1e-8)),
      mom_6m = log(pmax(close_vec[idx], 1e-8) / pmax(close_vec[idx - 126], 1e-8)),
      mom_12m_1m = log(pmax(close_vec[idx - 21], 1e-8) / pmax(close_vec[idx - 252], 1e-8)),
      reversal_1w = -log(pmax(close_vec[idx], 1e-8) / pmax(close_vec[idx - 5], 1e-8)),
      vol_1m = sd(stock_ret[(idx - 20):idx], na.rm = TRUE),
      drawdown_3m = close_vec[idx] / max(close_vec[(idx - 62):idx], na.rm = TRUE) - 1,
      beta_3m = suppressWarnings(stats::cov(stock_ret[(idx - 62):idx], mkt_ret[(idx - 62):idx], use = "complete.obs") / stats::var(mkt_ret[(idx - 62):idx], na.rm = TRUE)),
      volume_trend_1m = mean(vol_vec[(idx - 4):idx], na.rm = TRUE) / pmax(mean(vol_vec[(idx - 20):idx], na.rm = TRUE), 1e-8) - 1,
      liquidity_proxy = mean(close_vec[(idx - 20):idx] * vol_vec[(idx - 20):idx], na.rm = TRUE),
      rel_strength_3m = log(pmax(close_vec[idx], 1e-8) / pmax(close_vec[idx - 63], 1e-8)) - log(pmax(mkt_close[idx], 1e-8) / pmax(mkt_close[idx - 63], 1e-8)),
      stringsAsFactors = FALSE
    )
    if (isTRUE(include_time_index)) r$time_index <- idx
    rows[[length(rows) + 1]] <- r
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

standardize_factors_cross_section <- function(panel_df) {
  factor_cols <- c("mom_1m", "mom_3m", "mom_6m", "mom_12m_1m", "reversal_1w", "vol_1m", "drawdown_3m", "beta_3m", "volume_trend_1m", "liquidity_proxy", "rel_strength_3m")
  out <- panel_df
  for (sd in unique(out$signal_date)) {
    idx <- which(out$signal_date == sd)
    for (fc in factor_cols) {
      x <- out[idx, fc]
      mu <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
      out[idx, fc] <- if (is.finite(s) && s > 0) (x - mu) / s else 0
    }
  }
  out
}

load_stage3_outputs <- function(config) {
  if (!file.exists(config$files$stage3_model_panel)) stop("Stage 3 cached output not found")
  model_panel_cached <- utils::read.csv(config$files$stage3_model_panel, stringsAsFactors = FALSE)
  if ("date" %in% names(model_panel_cached)) model_panel_cached$date <- as.Date(model_panel_cached$date)
  if ("signal_date" %in% names(model_panel_cached)) model_panel_cached$signal_date <- as.Date(model_panel_cached$signal_date)
  if ("exit_date" %in% names(model_panel_cached)) model_panel_cached$exit_date <- as.Date(model_panel_cached$exit_date)
  model_panel_cached
}
