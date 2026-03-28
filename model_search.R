# model_search.R
# Stage 4: OLS model selection via exhaustive predictor enumeration

search_best_model <- function(
  dataset,
  config,
  target_col = "target_21d",
  candidate_predictors = NULL,
  date_col = "date",
  ticker_col = "ticker",
  min_predictors = 1,
  max_predictors = NULL,
  compute_ic = TRUE
) {
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data.frame")
  }

  required <- c(ticker_col, date_col, target_col)
  missing_required <- setdiff(required, names(dataset))
  if (length(missing_required) > 0) {
    stop(sprintf("dataset missing required columns: %s", paste(missing_required, collapse = ", ")))
  }

  if (is.null(max_predictors)) {
    if (!is.null(config$max_predictors)) {
      max_predictors <- as.integer(config$max_predictors)
    } else {
      max_predictors <- 15L
    }
  }

  # Auto-detect all numeric feature columns when candidate_predictors is NULL
  if (is.null(candidate_predictors)) {
    excluded <- c(ticker_col, date_col, target_col)
    remaining <- setdiff(names(dataset), excluded)
    candidate_predictors <- remaining[vapply(dataset[remaining], is.numeric, logical(1))]
  }

  available_predictors <- intersect(candidate_predictors, names(dataset))
  available_predictors <- available_predictors[vapply(dataset[available_predictors], is.numeric, logical(1))]

  if (length(available_predictors) == 0L) {
    stop("No numeric candidate predictors available.")
  }

  max_predictors <- min(max_predictors, length(available_predictors))
  min_predictors <- max(1L, as.integer(min_predictors))

  if (min_predictors > max_predictors) {
    stop("min_predictors cannot be larger than max_predictors.")
  }

  split <- generate_time_series_split(
    df = dataset,
    train_ratio = config$train_ratio,
    date_col = date_col,
    ticker_col = ticker_col
  )

  train_df <- split$train
  valid_df <- split$test

  if (nrow(train_df) == 0L || nrow(valid_df) == 0L) {
    stop("Time-series split produced empty train or validation set.")
  }

  results_list <- list()
  model_store <- list()
  model_id <- 1L

  for (k in seq(min_predictors, max_predictors)) {
    comb_list <- combn(available_predictors, k, simplify = FALSE)

    for (i in seq_along(comb_list)) {
      predictors <- comb_list[[i]]
      needed <- c(target_col, predictors)

      train_sub <- train_df[complete.cases(train_df[, needed, drop = FALSE]), needed, drop = FALSE]
      valid_sub <- valid_df[complete.cases(valid_df[, needed, drop = FALSE]), needed, drop = FALSE]

      if (nrow(train_sub) < (length(predictors) + 2L) || nrow(valid_sub) < 2L) {
        next
      }

      formula_obj <- as.formula(
        paste(target_col, "~", paste(predictors, collapse = " + "))
      )

      fit <- tryCatch(
        fit_ols_model(train_df = train_sub, formula_obj = formula_obj),
        error = function(e) NULL
      )

      if (is.null(fit)) next

      sc <- score_model(
        model = fit,
        valid_df = valid_sub,
        target_col = target_col,
        compute_ic = compute_ic
      )

      if (!is.finite(sc$rmse)) next

      s <- summary(fit)

      results_list[[length(results_list) + 1L]] <- data.frame(
        model_id = model_id,
        predictors = paste(predictors, collapse = ", "),
        n_predictors = length(predictors),
        n_train = nrow(train_sub),
        n_valid = nrow(valid_sub),
        adj_r_squared = unname(s$adj.r.squared),
        aic = AIC(fit),
        bic = BIC(fit),
        valid_rmse = sc$rmse,
        ic = sc$ic,
        stringsAsFactors = FALSE
      )

      model_store[[as.character(model_id)]] <- list(
        model = fit,
        formula = formula_obj,
        predictors = predictors
      )

      model_id <- model_id + 1L
    }
  }

  if (length(results_list) == 0L) {
    stop("No valid model fitted. Check data, missing values, or predictor limits.")
  }

  all_models <- do.call(rbind, results_list)
  all_models <- all_models[order(all_models$valid_rmse, -all_models$adj_r_squared), , drop = FALSE]
  rownames(all_models) <- NULL

  best_id <- as.character(all_models$model_id[1])
  best_obj <- model_store[[best_id]]

  list(
    best_model = best_obj$model,
    best_formula = best_obj$formula,
    best_predictors = best_obj$predictors,
    best_metrics = all_models[1, , drop = FALSE],
    all_models = all_models,
    split_info = list(
      train_n = nrow(train_df),
      valid_n = nrow(valid_df),
      train_ratio = config$train_ratio,
      predictor_count = length(available_predictors)
    )
  )
}

generate_time_series_split <- function(
  df,
  train_ratio = 0.7,
  date_col = "date",
  ticker_col = "ticker"
) {
  if (!is.data.frame(df)) {
    stop("df must be a data.frame")
  }

  if (!(train_ratio > 0 && train_ratio < 1)) {
    stop("train_ratio must be between 0 and 1")
  }

  has_ticker <- ticker_col %in% names(df)
  has_date <- date_col %in% names(df)

  if (!has_ticker) {
    x <- df
    if (has_date) x <- x[order(x[[date_col]]), , drop = FALSE]

    n <- nrow(x)
    if (n < 2L) stop("Need at least 2 rows for time-series split")

    n_train <- max(1L, floor(n * train_ratio))
    n_train <- min(n_train, n - 1L)

    return(list(
      train = x[seq_len(n_train), , drop = FALSE],
      test = x[(n_train + 1L):n, , drop = FALSE]
    ))
  }

  train_parts <- list()
  test_parts <- list()

  tickers <- unique(df[[ticker_col]])

  for (tk in tickers) {
    part <- df[df[[ticker_col]] == tk, , drop = FALSE]
    if (has_date) part <- part[order(part[[date_col]]), , drop = FALSE]

    n <- nrow(part)
    if (n < 2L) next

    n_train <- max(1L, floor(n * train_ratio))
    n_train <- min(n_train, n - 1L)

    train_parts[[length(train_parts) + 1L]] <- part[seq_len(n_train), , drop = FALSE]
    test_parts[[length(test_parts) + 1L]] <- part[(n_train + 1L):n, , drop = FALSE]
  }

  if (length(train_parts) == 0L || length(test_parts) == 0L) {
    stop("Unable to create per-ticker time-series train/test split")
  }

  list(
    train = do.call(rbind, train_parts),
    test = do.call(rbind, test_parts)
  )
}

fit_ols_model <- function(train_df, formula_obj) {
  lm(formula_obj, data = train_df)
}

score_model <- function(model, valid_df, target_col, compute_ic = TRUE) {
  pred <- as.numeric(predict(model, newdata = valid_df))
  actual <- as.numeric(valid_df[[target_col]])

  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  ic <- NA_real_

  if (isTRUE(compute_ic) && length(pred) > 1L && length(actual) > 1L) {
    ic <- suppressWarnings(cor(pred, actual, use = "complete.obs"))
  }

  list(rmse = rmse, ic = ic)
}
