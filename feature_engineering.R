# feature_engineering.R
# Stage 3

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
    if (j <= n && !is.na(x[i]) && !is.na(x[j]) && x[i] > 0 && x[j] > 0) out[i] <- log(x[j] / x[i])
  }
  out
}

rolling_mean <- function(x, k) {
  out <- rep(NA_real_, length(x))
  if (length(x) < k) return(out)
  for (i in k:length(x)) out[i] <- mean(x[(i - k + 1):i], na.rm = TRUE)
  out
}

rolling_sd <- function(x, k) {
  out <- rep(NA_real_, length(x))
  if (length(x) < k) return(out)
  for (i in k:length(x)) out[i] <- sd(x[(i - k + 1):i], na.rm = TRUE)
  out
}

create_ticker_features <- function(df_ticker, horizon = 21) {
  d <- df_ticker[order(df_ticker$date), , drop = FALSE]
  cpx <- d$close
  vol <- d$volume

  d$ret_1d <- safe_log_return(cpx, 1)
  d$ret_5d <- safe_log_return(cpx, 5)
  d$ret_10d <- safe_log_return(cpx, 10)
  d$ret_21d_past <- safe_log_return(cpx, 21)
  d$ln_close <- ifelse(cpx > 0, log(cpx), NA_real_)
  d$vol_chg_1d <- safe_log_return(pmax(vol, 1), 1)
  d$price_range <- (d$high - d$low) / pmax(cpx, 1e-8)
  d$oc_return <- ifelse(d$open > 0 & cpx > 0, log(cpx / d$open), NA_real_)

  ma5 <- rolling_mean(cpx, 5)
  ma21 <- rolling_mean(cpx, 21)
  d$ma_ratio_5_21 <- ma5 / ma21
  d$volatility_21 <- rolling_sd(d$ret_1d, 21)
  d$volume_z_21 <- (vol - rolling_mean(vol, 21)) / pmax(rolling_sd(vol, 21), 1e-8)

  d$target_21d <- forward_log_return(cpx, horizon)
  d
}

build_features_and_target <- function(raw_panel_df, config) {
  write_stage_log("stage3", "Stage 3 started", config)

  tickers <- unique(raw_panel_df$ticker)
  stage3_dir <- config$paths$stage3
  out_parts <- list()
  summary_rows <- list()

  for (tk in tickers) {
    d0 <- raw_panel_df[raw_panel_df$ticker == tk, , drop = FALSE]
    if (nrow(d0) < (config$target_horizon + 30)) next

    d1 <- create_ticker_features(d0, config$target_horizon)
    feature_cols <- setdiff(names(d1), c("ticker", "stock_code", "company_name", "date"))
    miss_ratio <- mean(!complete.cases(d1[, feature_cols, drop = FALSE]))

    utils::write.csv(d1, file.path(stage3_dir, paste0(tk, "_features.csv")), row.names = FALSE)

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      ticker = tk,
      rows = nrow(d1),
      missing_ratio = miss_ratio,
      stringsAsFactors = FALSE
    )

    out_parts[[length(out_parts) + 1]] <- d1
  }

  if (length(out_parts) == 0) stop("No ticker available for Stage 3 feature generation")

  model_panel <- do.call(rbind, out_parts)
  model_panel <- model_panel[order(model_panel$ticker, model_panel$date), , drop = FALSE]

  # keep valid target and at least one numeric feature
  feat_cols <- setdiff(names(model_panel), c("ticker", "stock_code", "company_name", "date", "target_21d"))
  num_cols <- feat_cols[vapply(model_panel[feat_cols], is.numeric, logical(1))]
  has_feature <- rep(FALSE, nrow(model_panel))
  for (cn in num_cols) has_feature <- has_feature | !is.na(model_panel[[cn]])
  model_panel <- model_panel[!is.na(model_panel$target_21d) & has_feature, , drop = FALSE]

  utils::write.csv(model_panel, config$files$stage3_model_panel, row.names = FALSE)
  feature_summary <- do.call(rbind, summary_rows)
  utils::write.csv(feature_summary, config$files$stage3_feature_summary, row.names = FALSE)

  save_stage_plot(
    plot_expr = function() hist(model_panel$target_21d, breaks = 60, col = "skyblue", main = "target_21d distribution", xlab = "target_21d"),
    file_path = file.path(stage3_dir, "target_21d_distribution.png")
  )
  save_stage_plot(
    plot_expr = function() hist(model_panel$ret_1d, breaks = 60, col = "tan", main = "ret_1d distribution", xlab = "ret_1d"),
    file_path = file.path(stage3_dir, "ret_1d_distribution.png")
  )

  write_stage_log("stage3", paste0("Stage 3 completed: rows=", nrow(model_panel), ", tickers=", length(unique(model_panel$ticker))), config)
  model_panel
}
