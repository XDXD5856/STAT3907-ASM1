# data_loader.R
# Stage 1 + Stage 2

format_hkex_to_yahoo <- function(stock_code) {
  code_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(stock_code))))
  if (is.na(code_num)) return(NA_character_)
  sprintf("%04d.HK", code_num)
}

get_hkex_universe <- function(config) {
  raw_df <- tryCatch(
    utils::read.csv(config$hkex_securities_list_url, stringsAsFactors = FALSE),
    error = function(e) stop("Failed to read HKEX list: ", conditionMessage(e))
  )

  total_stocks <- nrow(raw_df)
  if (total_stocks == 0) stop("HKEX list is empty")

  names(raw_df) <- tolower(gsub("[^a-z0-9]", "_", names(raw_df)))
  pick <- function(x) { y <- intersect(x, names(raw_df)); if (length(y) == 0) NA_character_ else y[1] }

  code_col <- pick(c("stock_code", "stockcode", "code", "stock_code_", "stock_code__"))
  name_col <- pick(c("name_of_securities", "name", "stock_name", "company_name"))
  cat_col <- pick(c("category", "security_type", "type"))
  sub_col <- pick(c("sub_category", "subcategory", "sub_type", "market"))

  if (is.na(code_col)) stop("Cannot find stock code column in HKEX list")

  u <- data.frame(
    stock_code = raw_df[[code_col]],
    company_name = if (!is.na(name_col)) raw_df[[name_col]] else NA_character_,
    category = if (!is.na(cat_col)) raw_df[[cat_col]] else NA_character_,
    sub_category = if (!is.na(sub_col)) raw_df[[sub_col]] else NA_character_,
    stringsAsFactors = FALSE
  )

  code_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(u$stock_code))))
  u$stock_code <- ifelse(is.na(code_num), NA_character_, sprintf("%04d", code_num))
  u$ticker <- vapply(u$stock_code, format_hkex_to_yahoo, character(1))

  txt <- tolower(paste(u$category, u$sub_category, u$company_name))
  include_equity <- grepl("equity|ordinary|stock|share", txt)
  exclude_non_equity <- !grepl("warrant|bond|etf|fund|right|cbbc|derivative|trust|debt", txt)

  sub_txt <- tolower(as.character(u$sub_category))
  has_sub <- !(all(is.na(u$sub_category)) || all(u$sub_category == ""))
  mainboard_ok <- if (has_sub) (grepl("main", sub_txt) | !grepl("gem", sub_txt)) else rep(TRUE, nrow(u))

  u <- u[
    !is.na(u$stock_code) & !is.na(u$ticker) & include_equity & exclude_non_equity & mainboard_ok,
    c("stock_code", "company_name", "category", "sub_category", "ticker"),
    drop = FALSE
  ]
  u <- u[!duplicated(u$ticker), , drop = FALSE]
  rownames(u) <- NULL

  attr(u, "total_hkex_stocks") <- total_stocks
  attr(u, "qualified_stocks") <- nrow(u)
  attr(u, "excluded_stocks") <- total_stocks - nrow(u)
  u
}

download_single_ticker_history <- function(ticker, from, to) {
  xt <- tryCatch(
    quantmod::getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE, warnings = FALSE),
    error = function(e) NULL
  )

  if (is.null(xt) || NROW(xt) == 0) return(NULL)

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
  # ---------- Stage 1 ----------
  write_stage_log("stage1", "Stage 1 started", config)
  universe <- get_hkex_universe(config)

  utils::write.csv(universe, config$files$stage1_universe, row.names = FALSE)
  summary_lines <- c(
    sprintf("total HKEX stocks: %d", attr(universe, "total_hkex_stocks")),
    sprintf("qualified stocks: %d", attr(universe, "qualified_stocks")),
    sprintf("excluded stocks: %d", attr(universe, "excluded_stocks"))
  )
  writeLines(summary_lines, con = config$files$stage1_summary)
  write_stage_log("stage1", paste(summary_lines, collapse = " | "), config)
  write_stage_log("stage1", "Stage 1 completed", config)

  # ---------- Stage 2 ----------
  write_stage_log("stage2", "Stage 2 started", config)
  stage2_dir <- config$paths$stage2

  panel_parts <- list()
  failed_rows <- list()
  summary_rows <- list()

  n_total <- nrow(universe)
  for (i in seq_len(n_total)) {
    ticker_i <- universe$ticker[i]
    stock_code_i <- universe$stock_code[i]
    company_name_i <- universe$company_name[i]

    msg <- paste0("Downloading: ", ticker_i, " (", i, "/", n_total, ")")
    write_stage_log("stage2", msg, config)

    ticker_dir <- file.path(stage2_dir, ticker_i)
    if (!dir.exists(ticker_dir)) dir.create(ticker_dir, recursive = TRUE, showWarnings = FALSE)

    df <- tryCatch(
      download_single_ticker_history(ticker_i, config$start_date, config$end_date),
      error = function(e) NULL
    )

    if (is.null(df) || nrow(df) == 0) {
      failed_rows[[length(failed_rows) + 1]] <- data.frame(
        ticker = ticker_i,
        stock_code = stock_code_i,
        company_name = company_name_i,
        reason = "download_failed_or_empty",
        stringsAsFactors = FALSE
      )
      next
    }

    before_n <- nrow(df)
    df <- df[
      !is.na(df$date) & !is.na(df$close) & df$close > 0 & !is.na(df$open) & !is.na(df$high) & !is.na(df$low) & !is.na(df$volume),
      ,
      drop = FALSE
    ]

    missing_ratio <- mean(!complete.cases(df[, c("open", "high", "low", "close", "volume", "adjusted_close")]))
    avg_volume <- mean(df$volume, na.rm = TRUE)

    pass_history <- nrow(df) >= config$min_history_days
    pass_missing <- is.finite(missing_ratio) && missing_ratio <= config$max_missing_ratio
    pass_liq <- is.finite(avg_volume) && avg_volume >= config$min_avg_daily_volume

    if (!(pass_history && pass_missing && pass_liq)) {
      failed_rows[[length(failed_rows) + 1]] <- data.frame(
        ticker = ticker_i,
        stock_code = stock_code_i,
        company_name = company_name_i,
        reason = paste0("filter_failed(history=", pass_history, ",missing=", pass_missing, ",liq=", pass_liq, ")"),
        stringsAsFactors = FALSE
      )
      next
    }

    df$stock_code <- stock_code_i
    df$company_name <- company_name_i
    df <- df[, c("ticker", "stock_code", "company_name", "date", "open", "high", "low", "close", "volume", "adjusted_close")]

    utils::write.csv(df, file.path(ticker_dir, paste0(ticker_i, "_raw.csv")), row.names = FALSE)

    save_stage_plot(
      plot_expr = function() {
        plot(df$date, df$close, type = "l", col = "steelblue", xlab = "Date", ylab = "Close", main = paste("Close -", ticker_i))
      },
      file_path = file.path(ticker_dir, paste0(ticker_i, "_close.png"))
    )

    panel_parts[[length(panel_parts) + 1]] <- df
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      ticker = ticker_i,
      stock_code = stock_code_i,
      company_name = company_name_i,
      rows_before_clean = before_n,
      rows_after_clean = nrow(df),
      avg_volume = avg_volume,
      missing_ratio = missing_ratio,
      status = "success",
      stringsAsFactors = FALSE
    )
  }

  if (length(panel_parts) == 0) stop("No stock passed Stage 2 download/filter process")

  raw_panel <- do.call(rbind, panel_parts)
  raw_panel <- raw_panel[order(raw_panel$ticker, raw_panel$date), , drop = FALSE]
  rownames(raw_panel) <- NULL

  failed_df <- if (length(failed_rows) > 0) do.call(rbind, failed_rows) else data.frame()
  download_summary <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()

  utils::write.csv(raw_panel, config$files$stage2_raw_panel, row.names = FALSE)
  utils::write.csv(failed_df, config$files$stage2_failed, row.names = FALSE)
  utils::write.csv(download_summary, config$files$stage2_download_summary, row.names = FALSE)

  write_stage_log("stage2", paste0("Stage 2 completed: success_tickers=", length(unique(raw_panel$ticker)), ", failed=", nrow(failed_df)), config)

  raw_panel
}
