# model_search.R
# Stage 3: Search best OLS model with for-loops and time-series split.

search_best_model <- function(
  dataset,
  config,
  target_col = "target_21d",
  candidate_predictors = NULL,
  date_col = "date",
  ticker_col = "ticker",
  min_predictors = 1,
  max_predictors = NULL
) {
  if (!is.data.frame(dataset)) {
    stop("dataset must be a data.frame")
  }

  if (!(target_col %in% names(dataset))) {
    stop(sprintf("Target column '%s' is missing in dataset.", target_col))
  }

  # Easy-to-edit default predictor set
  default_predictors <- c(
    "ret_1d", "ret_5d", "ret_10d", "ret_21d_past",
    "ln_close", "vol_chg_1d", "volume", "close"
  )

  if (is.null(candidate_predictors)) {
    if (!is.null(config$candidate_predictors)) {
      candidate_predictors <- config$candidate_predictors
    } else {
      candidate_predictors <- default_predictors
    }
  }

  available_predictors <- intersect(candidate_predictors, names(dataset))

  if (length(available_predictors) == 0) {
    stop("No candidate predictors are available in dataset.")
  }

  if (is.null(max_predictors)) {
    max_predictors <- length(available_predictors)
  }

  max_predictors <- min(max_predictors, length(available_predictors))
  min_predictors <- max(1, min_predictors)

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
  test_df <- split$test

  if (nrow(train_df) == 0 || nrow(test_df) == 0) {
    stop("Time-series split produced an empty train or test set.")
  }

  results_list <- list()
  model_store <- list()
  row_id <- 1

  for (k in seq(min_predictors, max_predictors)) {
    comb_list <- combn(available_predictors, k, simplify = FALSE)

    for (combo_idx in seq_along(comb_list)) {
      predictors <- comb_list[[combo_idx]]
      needed_cols <- c(target_col, predictors)

      train_sub <- train_df[complete.cases(train_df[, needed_cols, drop = FALSE]), needed_cols, drop = FALSE]
      test_sub <- test_df[complete.cases(test_df[, needed_cols, drop = FALSE]), needed_cols, drop = FALSE]

      if (nrow(train_sub) < length(predictors) + 2 || nrow(test_sub) < 1) {
        next
      }

      formula_obj <- as.formula(
        paste(target_col, "~", paste(predictors, collapse = " + "))
      )

      fit <- tryCatch(
        fit_ols_model(train_sub, formula_obj),
        error = function(e) NULL
      )

      if (is.null(fit)) {
        next
      }

      rmse_test <- score_model(fit, test_sub, target_col)
      fit_summary <- summary(fit)

      results_list[[row_id]] <- data.frame(
        model_id = row_id,
        predictors = paste(predictors, collapse = ", "),
        n_predictors = length(predictors),
        n_train = nrow(train_sub),
        n_test = nrow(test_sub),
        adj_r_squared = unname(fit_summary$adj.r.squared),
        aic = AIC(fit),
        bic = BIC(fit),
        test_rmse = rmse_test,
        stringsAsFactors = FALSE
      )

      model_store[[as.character(row_id)]] <- list(
        model = fit,
        formula = formula_obj,
        predictors = predictors
      )

      row_id <- row_id + 1
    }
  }

  if (length(results_list) == 0) {
    stop("No valid model was fitted. Check data quality and candidate predictors.")
  }

  all_models <- do.call(rbind, results_list)
  all_models <- all_models[order(all_models$test_rmse, -all_models$adj_r_squared), ]
  rownames(all_models) <- NULL

  best_model_id <- as.character(all_models$model_id[1])
  best_model_obj <- model_store[[best_model_id]]

  list(
    best_model = best_model_obj$model,
    best_formula = best_model_obj$formula,
    best_predictors = best_model_obj$predictors,
    best_metrics = all_models[1, , drop = FALSE],
    all_models = all_models,
    split_info = list(
      train_n = nrow(train_df),
      test_n = nrow(test_df),
      train_ratio = config$train_ratio
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
    stop("train_ratio must be between 0 and 1.")
  }

  if (date_col %in% names(df)) {
    df <- df[order(df[[date_col]]), , drop = FALSE]
  }

  has_ticker <- ticker_col %in% names(df)

  if (!has_ticker) {
    n <- nrow(df)
    train_n <- max(1, floor(n * train_ratio))
    train_idx <- seq_len(train_n)
    test_idx <- setdiff(seq_len(n), train_idx)

    return(list(
      train = df[train_idx, , drop = FALSE],
      test = df[test_idx, , drop = FALSE]
    ))
  }

  train_parts <- list()
  test_parts <- list()

  tickers <- unique(df[[ticker_col]])

  for (tk in tickers) {
    tk_df <- df[df[[ticker_col]] == tk, , drop = FALSE]

    if (date_col %in% names(tk_df)) {
      tk_df <- tk_df[order(tk_df[[date_col]]), , drop = FALSE]
    }

    n <- nrow(tk_df)
    if (n < 2) {
      next
    }

    train_n <- max(1, floor(n * train_ratio))
    train_n <- min(train_n, n - 1)

    train_parts[[length(train_parts) + 1]] <- tk_df[seq_len(train_n), , drop = FALSE]
    test_parts[[length(test_parts) + 1]] <- tk_df[(train_n + 1):n, , drop = FALSE]
  }

  if (length(train_parts) == 0 || length(test_parts) == 0) {
    stop("Unable to create train/test split by ticker.")
  }

  list(
    train = do.call(rbind, train_parts),
    test = do.call(rbind, test_parts)
  )
}

fit_ols_model <- function(train_df, formula_obj) {
  lm(formula_obj, data = train_df)
}

score_model <- function(model, valid_df, target_col) {
  pred <- predict(model, newdata = valid_df)
  actual <- valid_df[[target_col]]
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}
