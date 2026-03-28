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
  k_values <- get_stage4_k_values(config, min_predictors, max_predictors)
  if (length(k_values) == 0) stop("No valid k values for Stage 4 search")
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
    if (nrow(pva) > 0) safe_write_csv(pva, config$files$stage4_pred_vs_actual)

    write_stage4_runtime_summary(config, stage4_start, Sys.time(), nrow(cached_models), cached_models$valid_rmse[1], skipped = TRUE, filtered_count = NA_integer_, total_combinations = NA_integer_)
    return(list(best_model = fit, best_formula = fm, best_predictors = bp, best_metrics = cached_models[1, , drop = FALSE], all_models = cached_models, complexity_summary = if (file.exists(config$files$stage4_model_complexity_summary)) utils::read.csv(config$files$stage4_model_complexity_summary, stringsAsFactors = FALSE) else data.frame(), selected_k = length(bp), split_info = list(train_n = nrow(train_df), valid_n = nrow(valid_df), train_ratio = config$train_ratio)))
  }

  combos_by_k <- vapply(k_values, function(k) choose(length(avail), k), numeric(1))
  total_combos <- as.integer(sum(combos_by_k))
  write_stage_log("stage4", paste0("Total predictor combinations to evaluate: ", total_combos), config)

  progress_every <- max(1L, as.integer(config$stage4_progress_every))
  partial_save_every <- max(1L, as.integer(config$stage4_partial_save_every))
  results <- list(); models <- list(); mid <- 1L; combo_counter <- 0L; filtered_counter <- 0L
  complexity_stats <- vector("list", length(k_values))
  names(complexity_stats) <- as.character(k_values)
  for (k in k_values) complexity_stats[[as.character(k)]] <- list(best_rmse = Inf, best_ic = NA_real_, model_count = 0L)

  for (k in k_values) {
    combos_total_k <- as.integer(choose(length(avail), k))
    combos_tested_k <- 0L
    combos_filtered_k <- 0L
    combos <- combn(avail, k, simplify = FALSE)
    for (i in seq_along(combos)) {
      combo_counter <- combo_counter + 1L
      combos_tested_k <- combos_tested_k + 1L
      preds <- combos[[i]]
      if (!is_valid_feature_group_combo(preds, feature_map, required_groups, max_per_group = 3L)) {
        filtered_counter <- filtered_counter + 1L
        combos_filtered_k <- combos_filtered_k + 1L
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
      k_stat <- complexity_stats[[as.character(k)]]
      k_stat$model_count <- k_stat$model_count + 1L
      if (sc$rmse < k_stat$best_rmse || (isTRUE(all.equal(sc$rmse, k_stat$best_rmse)) && !is.na(sc$ic) && (is.na(k_stat$best_ic) || sc$ic > k_stat$best_ic))) {
        k_stat$best_rmse <- sc$rmse
        k_stat$best_ic <- sc$ic
      }
      complexity_stats[[as.character(k)]] <- k_stat
      mid <- mid + 1L

      if (combo_counter %% progress_every == 0L) {
        write_stage_log("stage4", paste0("Progress: evaluated ", combo_counter, "/", total_combos, " combinations; valid_models=", length(results)), config)
      }
      if (length(results) > 0 && combo_counter %% partial_save_every == 0L) {
        safe_write_csv(do.call(rbind, results), config$files$stage4_all_models_partial)
      }
    }
    write_stage_log(
      "stage4",
      paste0("k=", k, " combinations_total=", combos_total_k, ", combinations_tested=", combos_tested_k, ", filtered=", combos_filtered_k, ", valid_models=", complexity_stats[[as.character(k)]]$model_count),
      config
    )
  }

  write_stage_log("stage4", paste0("Feature-group filtered combinations: ", filtered_counter, "/", total_combos), config)
  if (length(results) == 0) stop("No valid model fitted")

  complexity_summary <- do.call(
    rbind,
    lapply(k_values, function(k) {
      s <- complexity_stats[[as.character(k)]]
      data.frame(
        k = k,
        best_rmse = if (is.finite(s$best_rmse)) s$best_rmse else NA_real_,
        best_ic = s$best_ic,
        model_count = s$model_count,
        stringsAsFactors = FALSE
      )
    })
  )
  safe_write_csv(complexity_summary, config$files$stage4_model_complexity_summary)
  k_choice <- choose_optimal_k(complexity_summary, config)
  selected_k <- as.integer(k_choice$k)
  write_stage_log("stage4", paste0("Selected predictor count k=", selected_k, " reason: ", k_choice$reason), config)

  all_models <- do.call(rbind, results)
  all_models <- all_models[order(all_models$valid_rmse, -all_models$ic), , drop = FALSE]
  best_candidates <- all_models[all_models$n_predictors == selected_k, , drop = FALSE]
  if (nrow(best_candidates) == 0) {
    write_stage_log("stage4", paste0("No valid model found for selected k=", selected_k, "; falling back to global best"), config)
    best_candidates <- all_models
  }
  best_candidates <- best_candidates[order(best_candidates$valid_rmse, -best_candidates$ic), , drop = FALSE]

  best_id <- as.character(best_candidates$model_id[1]); best_obj <- models[[best_id]]
  bp <- best_obj$predictors
  va_best <- valid_df[complete.cases(valid_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
  pred <- as.numeric(predict(best_obj$model, newdata = va_best))
  pva <- data.frame(actual = va_best[[target_col]], predicted = pred)

  safe_write_csv(all_models, config$files$stage4_all_models)
  safe_write_csv(data.frame(predictor = best_obj$predictors), config$files$stage4_best_predictors)
  safe_write_csv(data.frame(train_n = nrow(train_df), valid_n = nrow(valid_df), train_ratio = config$train_ratio), config$files$stage4_split_info)
  safe_write_csv(pva, config$files$stage4_pred_vs_actual)
  safe_write_lines(c(paste0("selected_k: ", selected_k), paste0("selection_reason: ", k_choice$reason), paste0("best_formula: ", deparse(best_obj$formula)), paste0("best_rmse: ", best_candidates$valid_rmse[1]), paste0("best_adj_r2: ", best_candidates$adj_r_squared[1]), paste0("best_aic: ", best_candidates$aic[1]), paste0("best_bic: ", best_candidates$bic[1]), paste0("best_ic: ", best_candidates$ic[1])), config$files$stage4_best_model_summary)
  write_stage4_runtime_summary(config, stage4_start, Sys.time(), nrow(all_models), best_candidates$valid_rmse[1], skipped = FALSE, filtered_count = filtered_counter, total_combinations = total_combos)

  save_stage_plot(function() plot(seq_len(nrow(all_models)), all_models$valid_rmse, type = "l", col = "steelblue", xlab = "Model rank", ylab = "RMSE", main = "RMSE by model rank"), config$files$stage4_plot_rmse)
  top_n <- min(config$top_n_chart, nrow(all_models))
  save_stage_plot(function() barplot(rev(all_models$valid_rmse[1:top_n]), names.arg = rev(paste0("M", all_models$model_id[1:top_n])), horiz = TRUE, las = 1, col = "darkseagreen3", main = "Top models"), config$files$stage4_plot_top20)
  if (nrow(pva) > 1) save_stage_plot(function() { plot(pva$actual, pva$predicted, pch = 16, col = rgb(0,0,1,0.4), xlab = "Actual", ylab = "Predicted", main = "Predicted vs Actual"); abline(0,1,col="red",lwd=2) }, config$files$stage4_plot_scatter)

  write_stage_log("stage4", paste0("Stage 4 completed: models=", nrow(all_models), ", best_rmse=", best_candidates$valid_rmse[1], ", selected_k=", selected_k), config)

  list(best_model = best_obj$model, best_formula = best_obj$formula, best_predictors = best_obj$predictors, best_metrics = best_candidates[1, , drop = FALSE], all_models = all_models, complexity_summary = complexity_summary, selected_k = selected_k, split_info = list(train_n = nrow(train_df), valid_n = nrow(valid_df), train_ratio = config$train_ratio))
}

load_stage4_result <- function(dataset, config, target_col = "target_21d", date_col = "date", ticker_col = "ticker") {
  if (!file.exists(config$files$stage4_all_models) || !file.exists(config$files$stage4_best_predictors)) {
    stop("Cannot load Stage 4 result: cached files are missing")
  }

  split <- generate_time_series_split(
    dataset, config$train_ratio, date_col, ticker_col,
    use_rolling_window = isTRUE(config$stage4_use_rolling_window),
    rolling_window_size = as.integer(config$stage4_rolling_window_size)
  )
  train_df <- split$train
  cached_models <- utils::read.csv(config$files$stage4_all_models, stringsAsFactors = FALSE)
  bp <- utils::read.csv(config$files$stage4_best_predictors, stringsAsFactors = FALSE)$predictor
  bp <- bp[bp %in% names(dataset)]
  if (length(bp) == 0) stop("Cannot load Stage 4 result: no valid predictors found in cache")

  fm <- as.formula(paste(target_col, "~", paste(bp, collapse = " + ")))
  tr <- train_df[complete.cases(train_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
  fit <- fit_ols_model(tr, fm)
  list(
    best_model = fit,
    best_formula = fm,
    best_predictors = bp,
    best_metrics = cached_models[1, , drop = FALSE],
    all_models = cached_models,
    complexity_summary = if (file.exists(config$files$stage4_model_complexity_summary)) utils::read.csv(config$files$stage4_model_complexity_summary, stringsAsFactors = FALSE) else data.frame(),
    selected_k = length(bp),
    split_info = list(train_n = nrow(split$train), valid_n = nrow(split$test), train_ratio = config$train_ratio)
  )
}

get_stage4_k_values <- function(config, min_predictors, max_predictors) {
  seq(min_predictors, max_predictors)
}

choose_optimal_k <- function(complexity_summary, config) {
  valid <- complexity_summary[!is.na(complexity_summary$best_rmse) & complexity_summary$model_count > 0, , drop = FALSE]
  if (nrow(valid) == 0) return(list(k = complexity_summary$k[1], reason = "Fallback: no valid per-k models"))

  best_row <- valid[order(valid$best_rmse, -valid$best_ic, valid$k), ][1, , drop = FALSE]
  rmse_margin <- as.numeric(config$stage4_rmse_marginal_threshold)
  ic_tol <- as.numeric(config$stage4_ic_drop_tolerance)

  rmse_cutoff <- as.numeric(best_row$best_rmse) * (1 + rmse_margin)
  ic_floor <- as.numeric(best_row$best_ic) - ic_tol
  candidates <- valid[valid$best_rmse <= rmse_cutoff & (is.na(valid$best_ic) | is.na(ic_floor) | valid$best_ic >= ic_floor), , drop = FALSE]
  if (nrow(candidates) == 0) {
    return(list(k = best_row$k[1], reason = "Selected global RMSE winner (no low-complexity candidate met IC/RMSE thresholds)"))
  }

  chosen <- candidates[order(candidates$k, candidates$best_rmse, -candidates$best_ic), ][1, , drop = FALSE]
  if (chosen$k[1] == best_row$k[1]) {
    list(k = chosen$k[1], reason = "Selected by lowest RMSE")
  } else {
    list(k = chosen$k[1], reason = paste0("Selected smaller k with marginal RMSE loss (<= ", rmse_margin, ") and acceptable IC drop (<= ", ic_tol, ")"))
  }
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
  safe_write_lines(
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
