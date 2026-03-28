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

  per_ticker <- list(); summary_rows <- list()
  for (tk in unique(raw_panel_df$ticker)) {
    d0 <- raw_panel_df[raw_panel_df$ticker == tk, , drop = FALSE]
    if (nrow(d0) < (config$target_horizon + 30)) next

    d1 <- create_ticker_features(d0, config$target_horizon, isTRUE(config$stage4_include_time_index))
    safe_write_csv(d1, file.path(config$files$stage3_ticker_dir, paste0(tk, "_features.csv")))

    fcols <- setdiff(names(d1), c("ticker", "stock_code", "date"))
    summary_rows[[length(summary_rows) + 1]] <- data.frame(ticker = tk, rows = nrow(d1), missing_ratio = mean(!complete.cases(d1[, fcols, drop = FALSE])), stringsAsFactors = FALSE)
    per_ticker[[length(per_ticker) + 1]] <- d1
  }

  if (length(per_ticker) == 0) stop("No valid ticker for Stage 3")

  model_panel <- do.call(rbind, per_ticker)
  model_panel <- model_panel[order(model_panel$ticker, model_panel$date), , drop = FALSE]
  model_panel <- model_panel[!is.na(model_panel$target_21d), , drop = FALSE]

  safe_write_csv(model_panel, config$files$stage3_model_panel)
  safe_write_csv(do.call(rbind, summary_rows), config$files$stage3_summary)

  save_stage_plot(function() hist(model_panel$target_21d, breaks = 60, col = "skyblue", main = "target_21d", xlab = "target_21d"), config$files$stage3_plot_target)
  save_stage_plot(function() hist(model_panel$ret_1d, breaks = 60, col = "tan", main = "ret_1d", xlab = "ret_1d"), config$files$stage3_plot_ret1d)

  write_stage_log("stage3", paste0("Stage 3 completed: rows=", nrow(model_panel), ", tickers=", length(unique(model_panel$ticker))), config)
  model_panel
}

load_stage3_outputs <- function(config) {
  if (!file.exists(config$files$stage3_model_panel)) stop("Stage 3 cached output not found")
  model_panel_cached <- utils::read.csv(config$files$stage3_model_panel, stringsAsFactors = FALSE)
  if ("date" %in% names(model_panel_cached)) model_panel_cached$date <- as.Date(model_panel_cached$date)
  model_panel_cached
}
