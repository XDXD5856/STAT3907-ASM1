# data_loader.R
# Stage 1 + Stage 2: fixed stock code universe + Yahoo loop download

format_hkex_to_yahoo <- function(stock_code) {
  sprintf("%04d.HK", as.integer(stock_code))
}

build_universe_from_codes <- function(config) {
  codes <- unique(as.integer(config$stock_codes))
  data.frame(
    stock_code = sprintf("%04d", codes),
    ticker = sprintf("%04d.HK", codes),
    stringsAsFactors = FALSE
  )
}

download_single_ticker_history <- function(ticker, from, to) {
  xt <- tryCatch(
    quantmod::getSymbols(Symbols = ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE, warnings = FALSE),
    error = function(e) NULL
  )
  if (is.null(xt) || NROW(xt) == 0) return(NULL)

  data.frame(
    ticker = ticker,
    stock_code = substr(ticker, 1, 4),
    date = as.Date(zoo::index(xt)),
    open = as.numeric(quantmod::Op(xt)),
    high = as.numeric(quantmod::Hi(xt)),
    low = as.numeric(quantmod::Lo(xt)),
    close = as.numeric(quantmod::Cl(xt)),
    volume = as.numeric(quantmod::Vo(xt)),
    adjusted_close = as.numeric(quantmod::Ad(xt)),
    stringsAsFactors = FALSE
  )
}

load_and_clean_data <- function(config) {
  write_stage_log("stage12", "Stage 1+2 started", config)

  universe <- build_universe_from_codes(config)
  utils::write.csv(universe, config$files$stage12_universe, row.names = FALSE)
  write_stage_log("stage12", paste0("Universe size from fixed codes: ", nrow(universe)), config)

  success_list <- list()
  failed_list <- list()

  for (i in seq_len(nrow(universe))) {
    s <- universe$ticker[i]
    cat("Downloading:", s, "\n")
    write_stage_log("stage12", paste0("Downloading: ", s), config)

    d <- tryCatch(download_single_ticker_history(s, config$start_date, config$end_date), error = function(e) NULL)

    if (is.null(d) || nrow(d) == 0) {
      failed_list[[length(failed_list) + 1]] <- data.frame(ticker = s, stock_code = universe$stock_code[i], reason = "download_failed_or_empty", stringsAsFactors = FALSE)
      next
    }

    d <- d[
      !is.na(d$date) & !is.na(d$close) & d$close > 0 & !is.na(d$open) & !is.na(d$high) & !is.na(d$low) & !is.na(d$volume),
      ,
      drop = FALSE
    ]

    missing_ratio <- mean(!complete.cases(d[, c("open", "high", "low", "close", "volume", "adjusted_close")]))
    avg_volume <- mean(d$volume, na.rm = TRUE)

    if (nrow(d) < config$min_history_days || !is.finite(missing_ratio) || missing_ratio > config$max_missing_ratio || !is.finite(avg_volume) || avg_volume < config$min_avg_daily_volume) {
      failed_list[[length(failed_list) + 1]] <- data.frame(ticker = s, stock_code = universe$stock_code[i], reason = "failed_quality_filters", stringsAsFactors = FALSE)
      next
    }

    # exact standardized raw columns
    d <- d[, c("ticker", "stock_code", "date", "open", "high", "low", "close", "volume", "adjusted_close")]
    success_list[[length(success_list) + 1]] <- d

    utils::write.csv(d, file.path(config$files$stage12_ticker_dir, paste0(s, "_raw.csv")), row.names = FALSE)
    save_stage_plot(
      plot_expr = function() plot(d$date, d$close, type = "l", col = "steelblue", xlab = "Date", ylab = "Close", main = paste("Close", s)),
      file_path = file.path(config$files$stage12_ticker_dir, paste0(s, "_close.png"))
    )
  }

  if (length(success_list) == 0) stop("No successful ticker downloads after filters")

  raw_panel <- do.call(rbind, success_list)
  raw_panel <- raw_panel[order(raw_panel$ticker, raw_panel$date), , drop = FALSE]
  rownames(raw_panel) <- NULL

  failed_df <- if (length(failed_list) > 0) do.call(rbind, failed_list) else data.frame()

  utils::write.csv(raw_panel, config$files$stage12_raw_panel, row.names = FALSE)
  utils::write.csv(failed_df, config$files$stage12_failed, row.names = FALSE)

  write_stage_log("stage12", paste0("Stage 1+2 completed: success_tickers=", length(unique(raw_panel$ticker)), ", failed=", nrow(failed_df)), config)
  raw_panel
}
