# data_loader.R
# Stage 1: Universe construction
# Stage 2: Historical data download and panel standardization

pad_hk_code <- function(x) {
  code_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(x))))
  ifelse(is.na(code_num), NA_character_, sprintf("%04d", code_num))
}

to_yahoo_ticker <- function(hk_code) {
  paste0(pad_hk_code(hk_code), ".HK")
}

safe_get_hkex_list <- function(url) {
  tmp <- tempfile(fileext = ".csv")

  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) {
    stop("Failed to download HKEX securities list. Please verify hkex_securities_list_url.")
  }

  df <- tryCatch(utils::read.csv(tmp, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) {
    stop("Failed to parse HKEX securities list CSV.")
  }

  df
}

normalize_hkex_universe <- function(hkex_df) {
  raw_names <- names(hkex_df)
  clean_names <- tolower(gsub("[^a-z0-9]", "_", raw_names))
  names(hkex_df) <- clean_names

  code_candidates <- c("stock_code", "stockcode", "code", "stock_code__", "stock_code_", "stock_code_no")
  name_candidates <- c("name_of_securities", "name", "stock_name", "nameofsecurities")
  type_candidates <- c("category", "security_type", "type", "board_lot")
  board_candidates <- c("board", "market", "board_category", "sub_category")

  code_col <- intersect(code_candidates, names(hkex_df))
  name_col <- intersect(name_candidates, names(hkex_df))
  type_col <- intersect(type_candidates, names(hkex_df))
  board_col <- intersect(board_candidates, names(hkex_df))

  if (length(code_col) == 0) {
    # fallback: first column often contains code
    code_col <- names(hkex_df)[1]
  } else {
    code_col <- code_col[1]
  }

  name_col <- if (length(name_col) > 0) name_col[1] else NA_character_
  type_col <- if (length(type_col) > 0) type_col[1] else NA_character_
  board_col <- if (length(board_col) > 0) board_col[1] else NA_character_

  out <- data.frame(
    stock_code = hkex_df[[code_col]],
    stock_name = if (!is.na(name_col)) hkex_df[[name_col]] else NA_character_,
    security_type_raw = if (!is.na(type_col)) hkex_df[[type_col]] else NA_character_,
    board_raw = if (!is.na(board_col)) hkex_df[[board_col]] else NA_character_,
    stringsAsFactors = FALSE
  )

  out$stock_code <- pad_hk_code(out$stock_code)
  out$ticker <- to_yahoo_ticker(out$stock_code)
  out
}

apply_universe_rules <- function(universe_df) {
  d <- universe_df

  # equity/common stock heuristic filter (based on available text fields)
  sec_text <- tolower(paste(d$security_type_raw, d$stock_name, sep = " "))
  not_equity <- grepl("warrant|bond|etf|trust|debt|right|derivative|callable|cbbc|fund", sec_text)

  # main board heuristic when board info is available
  board_text <- tolower(as.character(d$board_raw))
  has_board_info <- !all(is.na(d$board_raw) | d$board_raw == "")
  mainboard_ok <- if (has_board_info) grepl("main", board_text) else rep(TRUE, nrow(d))

  d <- d[!is.na(d$stock_code) & !not_equity & mainboard_ok, , drop = FALSE]
  d <- d[!duplicated(d$ticker), , drop = FALSE]
  rownames(d) <- NULL
  d
}

fetch_yahoo_ohlcv <- function(ticker, start_date, end_date) {
  env <- new.env(parent = emptyenv())

  ok <- tryCatch({
    quantmod::getSymbols(
      Symbols = ticker,
      src = "yahoo",
      from = start_date,
      to = end_date,
      auto.assign = TRUE,
      env = env,
      warnings = FALSE
    )
    TRUE
  }, error = function(e) FALSE)

  if (!ok) return(NULL)

  if (!exists(ticker, envir = env, inherits = FALSE)) return(NULL)

  xt <- get(ticker, envir = env)
  if (NROW(xt) == 0) return(NULL)

  df <- data.frame(
    date = as.Date(zoo::index(xt)),
    open = as.numeric(quantmod::Op(xt)),
    high = as.numeric(quantmod::Hi(xt)),
    low = as.numeric(quantmod::Lo(xt)),
    close = as.numeric(quantmod::Cl(xt)),
    volume = as.numeric(quantmod::Vo(xt)),
    adjusted_close = as.numeric(quantmod::Ad(xt)),
    stringsAsFactors = FALSE
  )

  df$ticker <- ticker
  df[, c("ticker", "date", "open", "high", "low", "close", "volume", "adjusted_close")]
}

compute_quality_metrics <- function(df) {
  n <- nrow(df)
  missing_ratio <- mean(!complete.cases(df[, c("open", "high", "low", "close", "volume", "adjusted_close")]))
  avg_volume <- mean(df$volume, na.rm = TRUE)
  positive_close_ratio <- mean(df$close > 0, na.rm = TRUE)

  list(
    n_obs = n,
    missing_ratio = missing_ratio,
    avg_volume = avg_volume,
    positive_close_ratio = positive_close_ratio
  )
}

is_qualified_ticker <- function(df, config) {
  if (is.null(df) || nrow(df) < config$min_history_days) return(FALSE)

  qm <- compute_quality_metrics(df)

  if (is.na(qm$missing_ratio) || qm$missing_ratio > config$max_missing_ratio) return(FALSE)
  if (is.na(qm$avg_volume) || qm$avg_volume < config$min_avg_daily_volume) return(FALSE)
  if (is.na(qm$positive_close_ratio) || qm$positive_close_ratio < 1) return(FALSE)

  TRUE
}

construct_qualified_universe <- function(config) {
  hkex_raw <- safe_get_hkex_list(config$hkex_securities_list_url)
  universe <- normalize_hkex_universe(hkex_raw)
  universe <- apply_universe_rules(universe)

  qualified_rows <- list()

  for (i in seq_len(nrow(universe))) {
    ticker <- universe$ticker[i]
    hist_df <- fetch_yahoo_ohlcv(ticker, config$start_date, config$end_date)

    if (!is_qualified_ticker(hist_df, config)) {
      next
    }

    qm <- compute_quality_metrics(hist_df)

    qualified_rows[[length(qualified_rows) + 1]] <- data.frame(
      stock_code = universe$stock_code[i],
      stock_name = universe$stock_name[i],
      ticker = ticker,
      n_obs = qm$n_obs,
      missing_ratio = qm$missing_ratio,
      avg_volume = qm$avg_volume,
      stringsAsFactors = FALSE
    )
  }

  if (length(qualified_rows) == 0) {
    stop("No qualified stocks found. Please relax filtering thresholds in config.")
  }

  qualified <- do.call(rbind, qualified_rows)
  qualified <- qualified[order(qualified$ticker), , drop = FALSE]
  rownames(qualified) <- NULL
  qualified
}

# Stage 2
load_and_clean_data <- function(stock_list_df, config) {
  if (!is.data.frame(stock_list_df) || !("ticker" %in% names(stock_list_df))) {
    stop("stock_list_df must be a data.frame with a 'ticker' column.")
  }

  panel_parts <- list()

  for (tk in stock_list_df$ticker) {
    df <- fetch_yahoo_ohlcv(tk, config$start_date, config$end_date)
    if (is.null(df) || nrow(df) == 0) next

    # keep valid positive close rows and standard columns
    df <- df[!is.na(df$close) & df$close > 0, , drop = FALSE]
    if (nrow(df) == 0) next

    panel_parts[[length(panel_parts) + 1]] <- df
  }

  if (length(panel_parts) == 0) {
    stop("Failed to download usable Yahoo data for qualified stocks.")
  }

  panel <- do.call(rbind, panel_parts)
  panel <- panel[order(panel$ticker, panel$date), , drop = FALSE]
  rownames(panel) <- NULL

  panel
}
