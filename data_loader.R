# data_loader.R
# Stage 1: Universe construction
# Stage 2: Historical OHLCV download and cleaning

format_hkex_to_yahoo <- function(stock_code) {
  code_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(stock_code))))
  if (is.na(code_num)) return(NA_character_)
  sprintf("%04d.HK", code_num)
}

get_hkex_universe <- function(config) {
  if (is.null(config$hkex_securities_list_url)) {
    stop("config$hkex_securities_list_url is required")
  }

  message("[Stage 1] Downloading HKEX list from: ", config$hkex_securities_list_url)

  hkex_raw <- tryCatch(
    utils::read.csv(config$hkex_securities_list_url, stringsAsFactors = FALSE),
    error = function(e) {
      stop("Failed to read HKEX securities list: ", conditionMessage(e))
    }
  )

  if (nrow(hkex_raw) == 0) {
    stop("HKEX securities list is empty")
  }

  # Standardize column names
  raw_names <- names(hkex_raw)
  clean_names <- tolower(gsub("[^a-z0-9]", "_", raw_names))
  names(hkex_raw) <- clean_names

  # Flexible matching for expected fields
  pick_first <- function(candidates, pool) {
    hit <- intersect(candidates, pool)
    if (length(hit) == 0) NA_character_ else hit[1]
  }

  code_col <- pick_first(c("stock_code", "stockcode", "code", "stock_code_", "stock_code__"), names(hkex_raw))
  name_col <- pick_first(c("name_of_securities", "name", "stock_name", "company_name"), names(hkex_raw))
  cat_col <- pick_first(c("category", "security_type", "type", "classification"), names(hkex_raw))
  sub_cat_col <- pick_first(c("sub_category", "subcategory", "sub_type", "subclassification", "market"), names(hkex_raw))

  if (is.na(code_col)) {
    stop("Cannot find stock code column in HKEX file")
  }

  universe <- data.frame(
    stock_code = hkex_raw[[code_col]],
    company_name = if (!is.na(name_col)) hkex_raw[[name_col]] else NA_character_,
    category = if (!is.na(cat_col)) hkex_raw[[cat_col]] else NA_character_,
    sub_category = if (!is.na(sub_cat_col)) hkex_raw[[sub_cat_col]] else NA_character_,
    stringsAsFactors = FALSE
  )

  # Normalize code and ticker
  universe$stock_code <- sprintf(
    "%04d",
    suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(universe$stock_code))))
  )
  universe$ticker <- vapply(universe$stock_code, format_hkex_to_yahoo, character(1))

  # Common equity / ordinary stock heuristic filters
  text_blob <- tolower(paste(universe$category, universe$sub_category, universe$company_name))
  include_equity <- grepl("equity|ordinary|stock|share", text_blob)
  exclude_non_equity <- !grepl("warrant|bond|etf|fund|right|cbbc|derivative|trust|debt", text_blob)

  # Main board heuristic if available in sub_category
  sub_cat_text <- tolower(as.character(universe$sub_category))
  has_subcat <- !(all(is.na(universe$sub_category)) || all(universe$sub_category == ""))
  mainboard_ok <- if (has_subcat) grepl("main", sub_cat_text) | !grepl("gem", sub_cat_text) else rep(TRUE, nrow(universe))

  universe <- universe[
    !is.na(universe$stock_code) &
      !is.na(universe$ticker) &
      include_equity &
      exclude_non_equity &
      mainboard_ok,
    c("stock_code", "company_name", "category", "sub_category", "ticker"),
    drop = FALSE
  ]

  universe <- universe[!duplicated(universe$ticker), , drop = FALSE]
  rownames(universe) <- NULL

  message("[Stage 1] Qualified HKEX universe size: ", nrow(universe))
  universe
}

download_single_ticker_history <- function(ticker, from, to) {
  xt <- tryCatch(
    quantmod::getSymbols(
      Symbols = ticker,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE,
      warnings = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(xt) || NROW(xt) == 0) {
    return(NULL)
  }

  data.frame(
    ticker = ticker,
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
  universe <- get_hkex_universe(config)

  if (nrow(universe) == 0) {
    stop("No qualified stocks found in HKEX universe")
  }

  panel_parts <- list()
  total <- nrow(universe)

  message("[Stage 2] Starting Yahoo download for ", total, " tickers...")

  for (i in seq_len(total)) {
    stock_code_i <- universe$stock_code[i]
    company_name_i <- universe$company_name[i]
    ticker_i <- universe$ticker[i]

    if (i %% 20 == 0 || i == 1 || i == total) {
      message(sprintf("[Stage 2] Progress: %d/%d (%s)", i, total, ticker_i))
    }

    hist_df <- tryCatch(
      download_single_ticker_history(ticker = ticker_i, from = config$start_date, to = config$end_date),
      error = function(e) NULL
    )

    if (is.null(hist_df) || nrow(hist_df) == 0) {
      next
    }

    # basic row-level data cleaning
    hist_df <- hist_df[
      !is.na(hist_df$date) &
        !is.na(hist_df$close) &
        hist_df$close > 0 &
        !is.na(hist_df$open) &
        !is.na(hist_df$high) &
        !is.na(hist_df$low) &
        !is.na(hist_df$volume),
      ,
      drop = FALSE
    ]

    if (nrow(hist_df) < config$min_history_days) {
      next
    }

    missing_ratio <- mean(!complete.cases(hist_df[, c("open", "high", "low", "close", "volume", "adjusted_close")]))
    avg_volume <- mean(hist_df$volume, na.rm = TRUE)

    if (!is.finite(missing_ratio) || missing_ratio > config$max_missing_ratio) {
      next
    }

    if (!is.finite(avg_volume) || avg_volume < config$min_avg_daily_volume) {
      next
    }

    hist_df$stock_code <- stock_code_i
    hist_df$company_name <- company_name_i

    hist_df <- hist_df[, c(
      "ticker", "stock_code", "company_name", "date",
      "open", "high", "low", "close", "volume", "adjusted_close"
    )]

    panel_parts[[length(panel_parts) + 1]] <- hist_df
  }

  if (length(panel_parts) == 0) {
    stop("No ticker passed Yahoo download and quality filters. Relax config thresholds.")
  }

  panel_df <- do.call(rbind, panel_parts)
  panel_df <- panel_df[order(panel_df$ticker, panel_df$date), , drop = FALSE]
  rownames(panel_df) <- NULL

  message("[Stage 2] Final panel rows: ", nrow(panel_df), " | tickers: ", length(unique(panel_df$ticker)))
  panel_df
}
