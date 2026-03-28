# model_search.R
# Stage 4

search_best_model <- function(dataset, config, target_col = "target_21d", candidate_predictors = NULL, date_col = "date", ticker_col = "ticker", min_predictors = 1, max_predictors = NULL, compute_ic = TRUE) {
  write_stage_log("stage4", "Stage 4 started", config)

  if (is.null(max_predictors)) max_predictors <- as.integer(config$max_predictors)

  if (is.null(candidate_predictors)) {
    excluded <- c(ticker_col, "stock_code", date_col, target_col)
    rem <- setdiff(names(dataset), excluded)
    candidate_predictors <- rem[vapply(dataset[rem], is.numeric, logical(1))]
  }

  split <- generate_time_series_split(dataset, config$train_ratio, date_col, ticker_col)
  train_df <- split$train; valid_df <- split$test

  avail <- intersect(candidate_predictors, names(dataset))
  avail <- avail[vapply(dataset[avail], is.numeric, logical(1))]
  max_predictors <- min(max_predictors, length(avail))

  results <- list(); models <- list(); mid <- 1L

  for (k in seq(min_predictors, max_predictors)) {
    combos <- combn(avail, k, simplify = FALSE)
    for (i in seq_along(combos)) {
      preds <- combos[[i]]
      cols <- c(target_col, preds)
      tr <- train_df[complete.cases(train_df[, cols, drop = FALSE]), cols, drop = FALSE]
      va <- valid_df[complete.cases(valid_df[, cols, drop = FALSE]), cols, drop = FALSE]
      if (nrow(tr) < (length(preds) + 2L) || nrow(va) < 2L) next

      fm <- as.formula(paste(target_col, "~", paste(preds, collapse = " + ")))
      fit <- tryCatch(fit_ols_model(tr, fm), error = function(e) NULL)
      if (is.null(fit)) next

      sc <- score_model(fit, va, target_col, compute_ic)
      if (!is.finite(sc$rmse)) next

      s <- summary(fit)
      results[[length(results) + 1L]] <- data.frame(model_id = mid, predictors = paste(preds, collapse = ", "), n_predictors = length(preds), n_train = nrow(tr), n_valid = nrow(va), adj_r_squared = unname(s$adj.r.squared), aic = AIC(fit), bic = BIC(fit), valid_rmse = sc$rmse, ic = sc$ic, stringsAsFactors = FALSE)
      models[[as.character(mid)]] <- list(model = fit, formula = fm, predictors = preds)
      mid <- mid + 1L
    }
  }

  if (length(results) == 0) stop("No valid model fitted")

  all_models <- do.call(rbind, results)
  all_models <- all_models[order(all_models$valid_rmse, -all_models$adj_r_squared), , drop = FALSE]

  best_id <- as.character(all_models$model_id[1]); best_obj <- models[[best_id]]
  bp <- best_obj$predictors
  va_best <- valid_df[complete.cases(valid_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
  pred <- as.numeric(predict(best_obj$model, newdata = va_best))
  pva <- data.frame(actual = va_best[[target_col]], predicted = pred)

  utils::write.csv(all_models, config$files$stage4_all_models, row.names = FALSE)
  utils::write.csv(data.frame(predictor = best_obj$predictors), config$files$stage4_best_predictors, row.names = FALSE)
  utils::write.csv(data.frame(train_n = nrow(train_df), valid_n = nrow(valid_df), train_ratio = config$train_ratio), config$files$stage4_split_info, row.names = FALSE)
  utils::write.csv(pva, config$files$stage4_pred_vs_actual, row.names = FALSE)
  writeLines(c(paste0("best_formula: ", deparse(best_obj$formula)), paste0("best_rmse: ", all_models$valid_rmse[1]), paste0("best_adj_r2: ", all_models$adj_r_squared[1]), paste0("best_aic: ", all_models$aic[1]), paste0("best_bic: ", all_models$bic[1]), paste0("best_ic: ", all_models$ic[1])), config$files$stage4_best_model_summary)

  save_stage_plot(function() plot(seq_len(nrow(all_models)), all_models$valid_rmse, type = "l", col = "steelblue", xlab = "Model rank", ylab = "RMSE", main = "RMSE by model rank"), config$files$stage4_plot_rmse)
  top_n <- min(config$top_n_chart, nrow(all_models))
  save_stage_plot(function() barplot(rev(all_models$valid_rmse[1:top_n]), names.arg = rev(paste0("M", all_models$model_id[1:top_n])), horiz = TRUE, las = 1, col = "darkseagreen3", main = "Top models"), config$files$stage4_plot_top20)
  if (nrow(pva) > 1) save_stage_plot(function() { plot(pva$actual, pva$predicted, pch = 16, col = rgb(0,0,1,0.4), xlab = "Actual", ylab = "Predicted", main = "Predicted vs Actual"); abline(0,1,col="red",lwd=2) }, config$files$stage4_plot_scatter)

  write_stage_log("stage4", paste0("Stage 4 completed: models=", nrow(all_models), ", best_rmse=", all_models$valid_rmse[1]), config)

  list(best_model = best_obj$model, best_formula = best_obj$formula, best_predictors = best_obj$predictors, best_metrics = all_models[1, , drop = FALSE], all_models = all_models, split_info = list(train_n = nrow(train_df), valid_n = nrow(valid_df), train_ratio = config$train_ratio))
}

generate_time_series_split <- function(df, train_ratio = 0.7, date_col = "date", ticker_col = "ticker") {
  train_parts <- list(); test_parts <- list()
  for (tk in unique(df[[ticker_col]])) {
    d <- df[df[[ticker_col]] == tk, , drop = FALSE]
    d <- d[order(d[[date_col]]), , drop = FALSE]
    n <- nrow(d); if (n < 2) next
    n_train <- max(1, floor(n * train_ratio)); n_train <- min(n_train, n - 1)
    train_parts[[length(train_parts) + 1]] <- d[seq_len(n_train), , drop = FALSE]
    test_parts[[length(test_parts) + 1]] <- d[(n_train + 1):n, , drop = FALSE]
  }
  list(train = do.call(rbind, train_parts), test = do.call(rbind, test_parts))
}

fit_ols_model <- function(train_df, formula_obj) lm(formula_obj, data = train_df)
score_model <- function(model, valid_df, target_col, compute_ic = TRUE) {
  pred <- as.numeric(predict(model, newdata = valid_df)); actual <- as.numeric(valid_df[[target_col]])
  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  ic <- if (isTRUE(compute_ic) && length(pred) > 1) suppressWarnings(cor(pred, actual, use = "complete.obs")) else NA_real_
  list(rmse = rmse, ic = ic)
}
