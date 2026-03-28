# model_search.R
# Stage 4

search_best_model <- function(dataset, config, target_col = "target_21d", candidate_predictors = NULL, date_col = "date", ticker_col = "ticker", min_predictors = 1, max_predictors = NULL, compute_ic = TRUE) {
  stage4_start <- Sys.time()
  write_stage_log("stage4", "Stage 4 started", config)
  write_stage_log("stage4", "Stage 4 note: exhaustive base R lm() search is CPU-based by design", config)
  gpu_info <- detect_gpu_availability()
  write_stage_log("stage4", paste0("GPU usable: ", gpu_info$usable, "; detail: ", gpu_info$detail), config)

  if (is.null(max_predictors)) max_predictors <- as.integer(config$max_predictors)

  if (is.null(candidate_predictors)) {
    excluded <- c(ticker_col, "stock_code", date_col, target_col)
    rem <- setdiff(names(dataset), excluded)
    candidate_predictors <- rem[vapply(dataset[rem], is.numeric, logical(1))]
  }

  avail <- intersect(candidate_predictors, names(dataset))
  avail <- avail[vapply(dataset[avail], is.numeric, logical(1))]
  max_predictors <- min(max_predictors, length(avail))
  if (max_predictors < min_predictors) stop("No available predictors for requested min/max range")
  feature_map <- get_feature_group_map(config, avail)
  required_groups <- unique(unname(feature_map))
  write_stage_log("stage4", paste0("Feature-group constraints active; required_groups=", paste(required_groups, collapse = ", "), "; max_per_group=3"), config)

  split <- generate_time_series_split(
    dataset, config$train_ratio, date_col, ticker_col,
    use_rolling_window = isTRUE(config$stage4_use_rolling_window),
    rolling_window_size = as.integer(config$stage4_rolling_window_size)
  )
  train_df <- split$train; valid_df <- split$test

  if (!isTRUE(config$refresh_model_search) && file.exists(config$files$stage4_all_models) && file.exists(config$files$stage4_best_predictors)) {
    write_stage_log("stage4", "refresh_model_search=FALSE and all_models.csv exists; skipping exhaustive rerun", config)
    cached_models <- utils::read.csv(config$files$stage4_all_models, stringsAsFactors = FALSE)
    if (nrow(cached_models) == 0) stop("Cached all_models.csv exists but is empty")
    bp <- utils::read.csv(config$files$stage4_best_predictors, stringsAsFactors = FALSE)$predictor
    bp <- bp[bp %in% names(dataset)]
    fm <- as.formula(paste(target_col, "~", paste(bp, collapse = " + ")))
    tr <- train_df[complete.cases(train_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
    fit <- fit_ols_model(tr, fm)

    va_best <- valid_df[complete.cases(valid_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
    pred <- as.numeric(predict(fit, newdata = va_best))
    pva <- data.frame(actual = va_best[[target_col]], predicted = pred)
    if (nrow(pva) > 0) utils::write.csv(pva, config$files$stage4_pred_vs_actual, row.names = FALSE)

    write_stage4_runtime_summary(config, stage4_start, Sys.time(), nrow(cached_models), cached_models$valid_rmse[1], skipped = TRUE, filtered_count = NA_integer_, total_combinations = NA_integer_)
    return(list(best_model = fit, best_formula = fm, best_predictors = bp, best_metrics = cached_models[1, , drop = FALSE], all_models = cached_models, split_info = list(train_n = nrow(train_df), valid_n = nrow(valid_df), train_ratio = config$train_ratio)))
  }

  combos_by_k <- vapply(seq(min_predictors, max_predictors), function(k) utils::combn(length(avail), k), numeric(1))
  total_combos <- as.integer(sum(combos_by_k))
  write_stage_log("stage4", paste0("Total predictor combinations to evaluate: ", total_combos), config)

  progress_every <- max(1L, as.integer(config$stage4_progress_every))
  partial_save_every <- max(1L, as.integer(config$stage4_partial_save_every))
  results <- list(); models <- list(); mid <- 1L; combo_counter <- 0L; filtered_counter <- 0L

  for (k in seq(min_predictors, max_predictors)) {
    combos <- combn(avail, k, simplify = FALSE)
    for (i in seq_along(combos)) {
      combo_counter <- combo_counter + 1L
      preds <- combos[[i]]
      if (!is_valid_feature_group_combo(preds, feature_map, required_groups, max_per_group = 3L)) {
        filtered_counter <- filtered_counter + 1L
        next
      }
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

      if (combo_counter %% progress_every == 0L) {
        write_stage_log("stage4", paste0("Progress: evaluated ", combo_counter, "/", total_combos, " combinations; valid_models=", length(results)), config)
      }
      if (length(results) > 0 && combo_counter %% partial_save_every == 0L) {
        utils::write.csv(do.call(rbind, results), config$files$stage4_all_models_partial, row.names = FALSE)
      }
    }
  }

  write_stage_log("stage4", paste0("Feature-group filtered combinations: ", filtered_counter, "/", total_combos), config)
  if (length(results) == 0) stop("No valid model fitted")

  all_models <- do.call(rbind, results)
  all_models <- all_models[order(all_models$valid_rmse, -all_models$ic), , drop = FALSE]

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
  write_stage4_runtime_summary(config, stage4_start, Sys.time(), nrow(all_models), all_models$valid_rmse[1], skipped = FALSE, filtered_count = filtered_counter, total_combinations = total_combos)

  save_stage_plot(function() plot(seq_len(nrow(all_models)), all_models$valid_rmse, type = "l", col = "steelblue", xlab = "Model rank", ylab = "RMSE", main = "RMSE by model rank"), config$files$stage4_plot_rmse)
  top_n <- min(config$top_n_chart, nrow(all_models))
  save_stage_plot(function() barplot(rev(all_models$valid_rmse[1:top_n]), names.arg = rev(paste0("M", all_models$model_id[1:top_n])), horiz = TRUE, las = 1, col = "darkseagreen3", main = "Top models"), config$files$stage4_plot_top20)
  if (nrow(pva) > 1) save_stage_plot(function() { plot(pva$actual, pva$predicted, pch = 16, col = rgb(0,0,1,0.4), xlab = "Actual", ylab = "Predicted", main = "Predicted vs Actual"); abline(0,1,col="red",lwd=2) }, config$files$stage4_plot_scatter)

  write_stage_log("stage4", paste0("Stage 4 completed: models=", nrow(all_models), ", best_rmse=", all_models$valid_rmse[1]), config)

  list(best_model = best_obj$model, best_formula = best_obj$formula, best_predictors = best_obj$predictors, best_metrics = all_models[1, , drop = FALSE], all_models = all_models, split_info = list(train_n = nrow(train_df), valid_n = nrow(valid_df), train_ratio = config$train_ratio))
}

get_feature_group_map <- function(config, available_predictors) {
  default_feature_map <- c(
    ret_1d = "close",
    ret_5d = "close",
    ret_10d = "close",
    ret_21d_past = "close",
    ln_close = "close",
    vol_chg_1d = "volume",
    price_range = "high_low_close",
    oc_return = "open_close",
    ma_ratio_5_21 = "close",
    volatility_21 = "close",
    volume_z_21 = "volume",
    time_index = "time_index"
  )
  map_from_config <- config$feature_map
  feature_map <- if (!is.null(map_from_config)) map_from_config else default_feature_map
  feature_map <- feature_map[names(feature_map) %in% available_predictors]
  if (length(feature_map) == 0) {
    names(feature_map) <- NULL
    return(setNames(character(0), character(0)))
  }
  feature_map
}

is_valid_feature_group_combo <- function(predictors, feature_map, required_groups, max_per_group = 3L) {
  combo_map <- feature_map[predictors]
  combo_map <- combo_map[!is.na(combo_map)]
  if (length(required_groups) == 0) return(TRUE)
  counts <- table(combo_map)
  has_minimum <- all(required_groups %in% names(counts))
  within_max <- all(as.integer(counts) <= as.integer(max_per_group))
  has_minimum && within_max
}

write_stage4_runtime_summary <- function(config, start_time, end_time, model_count, best_rmse, skipped = FALSE, filtered_count = NA_integer_, total_combinations = NA_integer_) {
  elapsed_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
  elapsed_min <- elapsed_sec / 60
  writeLines(
    c(
      paste0("stage4_started_at: ", format(start_time, "%Y-%m-%d %H:%M:%S")),
      paste0("stage4_finished_at: ", format(end_time, "%Y-%m-%d %H:%M:%S")),
      paste0("elapsed_seconds: ", round(elapsed_sec, 2)),
      paste0("elapsed_minutes: ", round(elapsed_min, 2)),
      paste0("models_count: ", model_count),
      paste0("best_rmse: ", best_rmse),
      paste0("filtered_combinations: ", filtered_count),
      paste0("total_combinations: ", total_combinations),
      paste0("skipped_exhaustive_search: ", skipped)
    ),
    config$files$stage4_summary
  )
}

detect_gpu_availability <- function() {
  nvidia_path <- Sys.which("nvidia-smi")
  if (!identical(nvidia_path, "")) return(list(usable = TRUE, detail = "nvidia-smi detected"))

  if (requireNamespace("torch", quietly = TRUE)) {
    cuda_flag <- tryCatch(torch::cuda_is_available(), error = function(e) FALSE)
    return(list(usable = isTRUE(cuda_flag), detail = paste0("torch cuda_is_available=", cuda_flag)))
  }

  list(usable = FALSE, detail = "No GPU indicator detected in environment")
}

generate_time_series_split <- function(df, train_ratio = 0.7, date_col = "date", ticker_col = "ticker", use_rolling_window = FALSE, rolling_window_size = 252) {
  train_parts <- list(); test_parts <- list()
  for (tk in unique(df[[ticker_col]])) {
    d <- df[df[[ticker_col]] == tk, , drop = FALSE]
    d <- d[order(d[[date_col]]), , drop = FALSE]
    n <- nrow(d); if (n < 2) next
    n_train <- max(1, floor(n * train_ratio)); n_train <- min(n_train, n - 1)
    if (isTRUE(use_rolling_window)) {
      idx_start <- max(1, n_train - rolling_window_size + 1)
      train_parts[[length(train_parts) + 1]] <- d[idx_start:n_train, , drop = FALSE]
    } else {
      train_parts[[length(train_parts) + 1]] <- d[seq_len(n_train), , drop = FALSE]
    }
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
