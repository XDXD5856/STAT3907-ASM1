# model_search.R
# Stage 4

search_best_model <- function(dataset, config, target_col = "target_log10_return", candidate_predictors = NULL, date_col = "signal_date", ticker_col = "ticker", min_predictors = 1, max_predictors = NULL, compute_ic = TRUE) {
  stage4_start <- Sys.time()
  write_stage_log("stage4", "Stage 4 started", config)
  write_stage_log("stage4", "Stage 4 note: exhaustive base R lm() search is CPU-based by design", config)
  gpu_info <- detect_gpu_availability()
  write_stage_log("stage4", paste0("GPU usable: ", gpu_info$usable, "; detail: ", gpu_info$detail), config)

  if (is.null(max_predictors)) max_predictors <- as.integer(config$max_predictors)
  pred_sign <- if (isTRUE(config$invert_prediction_sign)) -1 else 1

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
    valid_ratio = as.numeric(config$valid_ratio),
    use_rolling_window = isTRUE(config$stage4_use_rolling_window),
    rolling_window_size = as.integer(config$stage4_rolling_window_size)
  )
  train_df <- split$train; valid_df <- split$valid; test_df <- split$test

  if (isTRUE(config$stage4_use_fixed_model)) {
    fixed_preds <- intersect(config$stage4_fixed_predictors, names(dataset))
    if (length(fixed_preds) == 0) stop("stage4_use_fixed_model=TRUE but no fixed predictors found in dataset")
    fm <- as.formula(paste(target_col, "~", paste(fixed_preds, collapse = " + ")))
    tr <- train_df[complete.cases(train_df[, c(target_col, fixed_preds), drop = FALSE]), c(target_col, fixed_preds), drop = FALSE]
    va <- valid_df[complete.cases(valid_df[, c(target_col, fixed_preds), drop = FALSE]), c(target_col, fixed_preds), drop = FALSE]
    te <- test_df[complete.cases(test_df[, c(target_col, fixed_preds), drop = FALSE]), c(target_col, fixed_preds), drop = FALSE]
    fit <- fit_ols_model(tr, fm)
    sc <- if (nrow(va) > 1) score_model(fit, va, target_col, compute_ic, pred_sign = pred_sign) else list(rmse = NA_real_, ic = NA_real_)
    bt <- if (nrow(te) > 1) score_model(fit, te, target_col, compute_ic, pred_sign = pred_sign) else list(rmse = NA_real_, ic = NA_real_)
    all_models <- data.frame(
      model_id = 1L,
      predictors = paste(fixed_preds, collapse = ", "),
      n_predictors = length(fixed_preds),
      n_train = nrow(tr),
      n_valid = nrow(va),
      adj_r_squared = unname(summary(fit)$adj.r.squared),
      aic = AIC(fit),
      bic = BIC(fit),
      valid_rmse = sc$rmse,
      ic = sc$ic,
      stringsAsFactors = FALSE
    )
    pva <- if (nrow(va) > 0) data.frame(actual = va[[target_col]], predicted = pred_sign * as.numeric(predict(fit, newdata = va))) else data.frame(actual = numeric(0), predicted = numeric(0))
    bt_pva <- if (nrow(te) > 0) data.frame(actual = te[[target_col]], predicted = pred_sign * as.numeric(predict(fit, newdata = te))) else data.frame(actual = numeric(0), predicted = numeric(0))
    complexity_summary <- data.frame(k = length(fixed_preds), best_rmse = sc$rmse, best_ic = sc$ic, model_count = 1L)
    safe_write_csv(all_models, config$files$stage4_all_models)
    safe_write_csv(complexity_summary, config$files$stage4_model_complexity_summary)
    safe_write_csv(data.frame(predictor = fixed_preds), config$files$stage4_best_predictors)
    safe_write_csv(data.frame(train_n = nrow(train_df), valid_n = nrow(valid_df), test_n = nrow(test_df), train_ratio = config$train_ratio, valid_ratio = config$valid_ratio), config$files$stage4_split_info)
    safe_write_csv(pva, config$files$stage4_pred_vs_actual)
    safe_write_csv(bt_pva, config$files$stage4_backtest)
    coef_path <- file.path(dirname(config$files$stage4_best_model_summary), "model_coefficients.csv")
    safe_write_csv(data.frame(term = rownames(summary(fit)$coefficients), summary(fit)$coefficients, row.names = NULL, check.names = FALSE), coef_path)
    safe_write_lines(c(paste0("selected_k: ", length(fixed_preds)), "selection_reason: fixed reference model", paste0("best_formula: ", deparse(fm)), paste0("best_rmse: ", sc$rmse), paste0("best_ic: ", sc$ic), paste0("backtest_rmse: ", bt$rmse), paste0("backtest_ic: ", bt$ic)), config$files$stage4_best_model_summary)
    write_stage_log("stage4", paste0("Using fixed reference model with predictors: ", paste(fixed_preds, collapse = ", ")), config)
    return(list(best_model = fit, best_formula = fm, best_predictors = fixed_preds, best_metrics = all_models[1, , drop = FALSE], backtest_metrics = data.frame(test_rmse = bt$rmse, test_ic = bt$ic), all_models = all_models, complexity_summary = complexity_summary, selected_k = length(fixed_preds), split_info = list(train_n = nrow(train_df), valid_n = nrow(valid_df), test_n = nrow(test_df), train_ratio = config$train_ratio, valid_ratio = config$valid_ratio)))
  }

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
    pred <- pred_sign * as.numeric(predict(fit, newdata = va_best))
    pva <- data.frame(actual = va_best[[target_col]], predicted = pred)
    if (nrow(pva) > 0) safe_write_csv(pva, config$files$stage4_pred_vs_actual)

    write_stage4_runtime_summary(config, stage4_start, Sys.time(), nrow(cached_models), cached_models$valid_rmse[1], skipped = TRUE, filtered_count = NA_integer_, total_combinations = NA_integer_)
    test_best <- test_df[complete.cases(test_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
    bt_pred <- if (nrow(test_best) > 0) pred_sign * as.numeric(predict(fit, newdata = test_best)) else numeric(0)
    bt_pva <- data.frame(actual = test_best[[target_col]], predicted = bt_pred)
    if (nrow(bt_pva) > 0) safe_write_csv(bt_pva, config$files$stage4_backtest)
    bt_sc <- if (nrow(test_best) > 1) score_model(fit, test_best, target_col, compute_ic, pred_sign = pred_sign) else list(rmse = NA_real_, ic = NA_real_)
    coef_path <- file.path(dirname(config$files$stage4_best_model_summary), "model_coefficients.csv")
    safe_write_csv(data.frame(term = rownames(summary(fit)$coefficients), summary(fit)$coefficients, row.names = NULL, check.names = FALSE), coef_path)
    return(list(best_model = fit, best_formula = fm, best_predictors = bp, best_metrics = cached_models[1, , drop = FALSE], backtest_metrics = data.frame(test_rmse = bt_sc$rmse, test_ic = bt_sc$ic), all_models = cached_models, complexity_summary = if (file.exists(config$files$stage4_model_complexity_summary)) utils::read.csv(config$files$stage4_model_complexity_summary, stringsAsFactors = FALSE) else data.frame(), selected_k = length(bp), split_info = list(train_n = nrow(train_df), valid_n = nrow(valid_df), test_n = nrow(test_df), train_ratio = config$train_ratio, valid_ratio = config$valid_ratio)))
  }

  combos_by_k <- vapply(k_values, function(k) choose(length(avail), k), numeric(1))
  total_combos <- as.integer(sum(combos_by_k))
  write_stage_log("stage4", paste0("Total predictor combinations to evaluate: ", total_combos), config)

  progress_every <- max(1L, as.integer(config$stage4_progress_every))
  partial_save_every <- max(1L, as.integer(config$stage4_partial_save_every))
  buffer_size <- max(1L, as.integer(config$stage4_checkpoint_buffer_size))
  results_buffer <- list(); mid <- 1L; combo_counter <- 0L; filtered_counter <- 0L; flushed_model_count <- 0L
  if (file.exists(config$files$stage4_all_models_partial)) file.remove(config$files$stage4_all_models_partial)
  cache_df <- load_stage4_model_cache(config)
  cache_map <- setNames(rep(TRUE, nrow(cache_df)), cache_df$model_id)
  cache_hits <- 0L
  new_models_evaluated <- 0L
  cache_buffer <- list()
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
      if (!is_valid_feature_group_combo(preds, feature_map, required_groups, max_per_group = as.integer(config$stage4_max_per_group))) {
        filtered_counter <- filtered_counter + 1L
        combos_filtered_k <- combos_filtered_k + 1L
        next
      }
      cache_model_id <- paste(sort(preds), collapse = "|")
      cols <- c(target_col, preds)
      tr <- train_df[complete.cases(train_df[, cols, drop = FALSE]), cols, drop = FALSE]
      va <- valid_df[complete.cases(valid_df[, cols, drop = FALSE]), cols, drop = FALSE]
      if (nrow(tr) < (length(preds) + 2L) || nrow(va) < 2L) next

      fm <- as.formula(paste(target_col, "~", paste(preds, collapse = " + ")))
      if (cache_model_id %in% names(cache_map)) {
        cache_row <- cache_df[cache_df$model_id == cache_model_id, , drop = FALSE]
        rmse_val <- as.numeric(cache_row$rmse[1])
        ic_val <- as.numeric(cache_row$ic[1])
        aic_val <- as.numeric(cache_row$aic[1])
        bic_val <- as.numeric(cache_row$bic[1])
        adjr_val <- NA_real_
        fit <- NULL
        cache_hits <- cache_hits + 1L
      } else {
        fit <- tryCatch(fit_ols_model(tr, fm), error = function(e) NULL)
        if (is.null(fit)) next

        sc <- score_model(fit, va, target_col, compute_ic, pred_sign = pred_sign)
        if (!is.finite(sc$rmse)) next
        s <- summary(fit)
        rmse_val <- sc$rmse
        ic_val <- sc$ic
        aic_val <- AIC(fit)
        bic_val <- BIC(fit)
        adjr_val <- unname(s$adj.r.squared)
        new_models_evaluated <- new_models_evaluated + 1L
        cache_map[[cache_model_id]] <- TRUE
        cache_buffer[[length(cache_buffer) + 1L]] <- data.frame(
          model_id = cache_model_id,
          rmse = rmse_val,
          ic = ic_val,
          aic = aic_val,
          bic = bic_val,
          n_obs = nrow(tr),
          stringsAsFactors = FALSE
        )
      }

      results_buffer[[length(results_buffer) + 1L]] <- data.frame(model_id = mid, predictors = paste(preds, collapse = ", "), n_predictors = length(preds), n_train = nrow(tr), n_valid = nrow(va), adj_r_squared = adjr_val, aic = aic_val, bic = bic_val, valid_rmse = rmse_val, ic = ic_val, stringsAsFactors = FALSE)
      k_stat <- complexity_stats[[as.character(k)]]
      k_stat$model_count <- k_stat$model_count + 1L
      if (rmse_val < k_stat$best_rmse || (isTRUE(all.equal(rmse_val, k_stat$best_rmse)) && !is.na(ic_val) && (is.na(k_stat$best_ic) || ic_val > k_stat$best_ic))) {
        k_stat$best_rmse <- rmse_val
        k_stat$best_ic <- ic_val
      }
      complexity_stats[[as.character(k)]] <- k_stat
      mid <- mid + 1L

      if (combo_counter %% progress_every == 0L) {
        write_stage_log("stage4", paste0("Progress: evaluated ", combo_counter, "/", total_combos, " combinations; valid_models=", flushed_model_count + length(results_buffer)), config)
      }
      if (length(results_buffer) >= buffer_size || (combo_counter %% partial_save_every == 0L && length(results_buffer) > 0)) {
        flushed_chunk <- do.call(rbind, results_buffer)
        append_checkpoint_table(flushed_chunk, config$files$stage4_all_models_partial)
        flushed_model_count <- flushed_model_count + nrow(flushed_chunk)
        write_stage_log("stage4", paste0("Checkpoint saved: current model count=", flushed_model_count), config)
        results_buffer <- list()
      }
      if (length(cache_buffer) >= buffer_size) {
        cache_chunk <- do.call(rbind, cache_buffer)
        append_checkpoint_table(cache_chunk, config$files$stage4_model_cache)
        cache_buffer <- list()
      }
    }
    write_stage_log(
      "stage4",
      paste0("k=", k, " combinations_total=", combos_total_k, ", combinations_tested=", combos_tested_k, ", filtered=", combos_filtered_k, ", valid_models=", complexity_stats[[as.character(k)]]$model_count),
      config
    )
  }

  write_stage_log("stage4", paste0("Feature-group filtered combinations: ", filtered_counter, "/", total_combos), config)
  write_stage_log("stage4", paste0("Model-cache stats: cache_hits=", cache_hits, ", new_models_evaluated=", new_models_evaluated), config)
  if (length(cache_buffer) > 0) append_checkpoint_table(do.call(rbind, cache_buffer), config$files$stage4_model_cache)
  cache_df <- load_stage4_model_cache(config)
  if (nrow(cache_df) > 0) safe_write_csv(cache_df, config$files$stage4_model_cache)
  if (length(results_buffer) > 0) {
    flushed_chunk <- do.call(rbind, results_buffer)
    append_checkpoint_table(flushed_chunk, config$files$stage4_all_models_partial)
    flushed_model_count <- flushed_model_count + nrow(flushed_chunk)
    write_stage_log("stage4", paste0("Checkpoint saved: current model count=", flushed_model_count), config)
    results_buffer <- list()
  }
  if (!file.exists(config$files$stage4_all_models_partial)) stop("No valid model fitted")

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

  all_models <- utils::read.csv(config$files$stage4_all_models_partial, stringsAsFactors = FALSE)
  all_models <- all_models[order(-all_models$ic, all_models$valid_rmse), , drop = FALSE]
  best_candidates <- all_models[all_models$n_predictors == selected_k, , drop = FALSE]
  if (nrow(best_candidates) == 0) {
    write_stage_log("stage4", paste0("No valid model found for selected k=", selected_k, "; falling back to global best"), config)
    best_candidates <- all_models
  }
  best_candidates <- best_candidates[order(-best_candidates$ic, best_candidates$valid_rmse), , drop = FALSE]

  best_row <- best_candidates[1, , drop = FALSE]
  bp <- strsplit(as.character(best_row$predictors[1]), ",\\s*")[[1]]
  best_formula <- as.formula(paste(target_col, "~", paste(bp, collapse = " + ")))
  tr_best <- train_df[complete.cases(train_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
  best_model_fit <- fit_ols_model(tr_best, best_formula)
  va_best <- valid_df[complete.cases(valid_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
  pred <- pred_sign * as.numeric(predict(best_model_fit, newdata = va_best))
  pva <- data.frame(actual = va_best[[target_col]], predicted = pred)
  test_best <- test_df[complete.cases(test_df[, c(target_col, bp), drop = FALSE]), c(target_col, bp), drop = FALSE]
  test_pred <- if (nrow(test_best) > 0) pred_sign * as.numeric(predict(best_model_fit, newdata = test_best)) else numeric(0)
  backtest_pva <- data.frame(actual = test_best[[target_col]], predicted = test_pred)
  backtest_sc <- if (nrow(test_best) > 1) score_model(best_model_fit, test_best, target_col, compute_ic, pred_sign = pred_sign) else list(rmse = NA_real_, ic = NA_real_)

  safe_write_csv(all_models, config$files$stage4_all_models)
  safe_write_csv(data.frame(predictor = bp), config$files$stage4_best_predictors)
  safe_write_csv(data.frame(train_n = nrow(train_df), valid_n = nrow(valid_df), test_n = nrow(test_df), train_ratio = config$train_ratio, valid_ratio = config$valid_ratio), config$files$stage4_split_info)
  safe_write_csv(pva, config$files$stage4_pred_vs_actual)
  safe_write_csv(backtest_pva, config$files$stage4_backtest)
  coef_path <- file.path(dirname(config$files$stage4_best_model_summary), "model_coefficients.csv")
  safe_write_csv(data.frame(term = rownames(summary(best_model_fit)$coefficients), summary(best_model_fit)$coefficients, row.names = NULL, check.names = FALSE), coef_path)
  safe_write_lines(c(paste0("selected_k: ", selected_k), paste0("selection_reason: ", k_choice$reason), paste0("best_formula: ", deparse(best_formula)), paste0("best_rmse: ", best_candidates$valid_rmse[1]), paste0("best_adj_r2: ", best_candidates$adj_r_squared[1]), paste0("best_aic: ", best_candidates$aic[1]), paste0("best_bic: ", best_candidates$bic[1]), paste0("best_ic: ", best_candidates$ic[1]), paste0("backtest_rmse: ", backtest_sc$rmse), paste0("backtest_ic: ", backtest_sc$ic)), config$files$stage4_best_model_summary)
  write_stage4_runtime_summary(config, stage4_start, Sys.time(), nrow(all_models), best_candidates$valid_rmse[1], skipped = FALSE, filtered_count = filtered_counter, total_combinations = total_combos)

  save_stage_plot(function() plot(seq_len(nrow(all_models)), all_models$valid_rmse, type = "l", col = "steelblue", xlab = "Model rank", ylab = "RMSE", main = "RMSE by model rank"), config$files$stage4_plot_rmse)
  top_n <- min(config$top_n_chart, nrow(all_models))
  save_stage_plot(function() barplot(rev(all_models$valid_rmse[1:top_n]), names.arg = rev(paste0("M", all_models$model_id[1:top_n])), horiz = TRUE, las = 1, col = "darkseagreen3", main = "Top models"), config$files$stage4_plot_top20)
  if (nrow(pva) > 1) save_stage_plot(function() { plot(pva$actual, pva$predicted, pch = 16, col = rgb(0,0,1,0.4), xlab = "Actual", ylab = "Predicted", main = "Predicted vs Actual"); abline(0,1,col="red",lwd=2) }, config$files$stage4_plot_scatter)

  write_stage_log("stage4", paste0("Stage 4 completed: models=", nrow(all_models), ", best_rmse=", best_candidates$valid_rmse[1], ", selected_k=", selected_k), config)

  list(best_model = best_model_fit, best_formula = best_formula, best_predictors = bp, best_metrics = best_candidates[1, , drop = FALSE], backtest_metrics = data.frame(test_rmse = backtest_sc$rmse, test_ic = backtest_sc$ic), all_models = all_models, complexity_summary = complexity_summary, selected_k = selected_k, split_info = list(train_n = nrow(train_df), valid_n = nrow(valid_df), test_n = nrow(test_df), train_ratio = config$train_ratio, valid_ratio = config$valid_ratio))
}

load_stage4_result <- function(dataset, config, target_col = "target_log10_return", date_col = "signal_date", ticker_col = "ticker") {
  if (!file.exists(config$files$stage4_all_models) || !file.exists(config$files$stage4_best_predictors)) {
    stop("Cannot load Stage 4 result: cached files are missing")
  }

  split <- generate_time_series_split(
    dataset, config$train_ratio, date_col, ticker_col,
    valid_ratio = as.numeric(config$valid_ratio),
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
    backtest_metrics = data.frame(test_rmse = NA_real_, test_ic = NA_real_),
    split_info = list(train_n = nrow(split$train), valid_n = nrow(split$valid), test_n = nrow(split$test), train_ratio = config$train_ratio, valid_ratio = config$valid_ratio)
  )
}

get_stage4_k_values <- function(config, min_predictors, max_predictors) {
  seq(min_predictors, max_predictors)
}

load_stage4_model_cache <- function(config) {
  cache_path <- config$files$stage4_model_cache
  if (!file.exists(cache_path)) {
    return(data.frame(model_id = character(0), rmse = numeric(0), ic = numeric(0), aic = numeric(0), bic = numeric(0), n_obs = integer(0), stringsAsFactors = FALSE))
  }
  out <- utils::read.csv(cache_path, stringsAsFactors = FALSE)
  req <- c("model_id", "rmse", "ic", "aic", "bic", "n_obs")
  missing_cols <- setdiff(req, names(out))
  for (mc in missing_cols) out[[mc]] <- NA
  out <- out[, req, drop = FALSE]
  out <- out[!duplicated(out$model_id), , drop = FALSE]
  out
}

upsert_stage4_model_cache <- function(existing_cache, new_rows) {
  if (is.null(new_rows) || nrow(new_rows) == 0) return(existing_cache)
  combined <- rbind(existing_cache, new_rows[, names(existing_cache), drop = FALSE])
  combined <- combined[!duplicated(combined$model_id), , drop = FALSE]
  rownames(combined) <- NULL
  combined
}

append_checkpoint_table <- function(df, path) {
  if (is.null(df) || nrow(df) == 0) return(invisible(FALSE))
  ensure_parent_dir(path)
  write_header <- !file.exists(path)
  utils::write.table(df, file = path, sep = ",", row.names = FALSE, col.names = write_header, append = !write_header, quote = TRUE)
  invisible(TRUE)
}

choose_optimal_k <- function(complexity_summary, config) {
  valid <- complexity_summary[!is.na(complexity_summary$best_rmse) & complexity_summary$model_count > 0, , drop = FALSE]
  if (nrow(valid) == 0) return(list(k = complexity_summary$k[1], reason = "Fallback: no valid per-k models"))

  best_row <- valid[order(-valid$best_ic, valid$best_rmse, valid$k), ][1, , drop = FALSE]
  rmse_margin <- as.numeric(config$stage4_rmse_marginal_threshold)
  ic_tol <- as.numeric(config$stage4_ic_drop_tolerance)

  rmse_cutoff <- as.numeric(best_row$best_rmse) * (1 + rmse_margin)
  ic_floor <- as.numeric(best_row$best_ic) - ic_tol
  candidates <- valid[(is.na(valid$best_ic) | is.na(ic_floor) | valid$best_ic >= ic_floor) & valid$best_rmse <= rmse_cutoff, , drop = FALSE]
  if (nrow(candidates) == 0) {
    return(list(k = best_row$k[1], reason = "Selected global IC winner (no simpler candidate met IC/RMSE thresholds)"))
  }

  chosen <- candidates[order(candidates$k, -candidates$best_ic, candidates$best_rmse), ][1, , drop = FALSE]
  if (chosen$k[1] == best_row$k[1]) {
    list(k = chosen$k[1], reason = "Selected by highest IC")
  } else {
    list(k = chosen$k[1], reason = paste0("Selected smaller k with acceptable IC drop (<= ", ic_tol, ") and marginal RMSE loss (<= ", rmse_margin, ")"))
  }
}

get_feature_group_map <- function(config, available_predictors) {
  default_feature_map <- c(
    mom_1m = "momentum",
    mom_3m = "momentum",
    mom_6m = "momentum",
    mom_12m_1m = "momentum",
    reversal_1w = "mean_reversion",
    vol_1m = "volatility",
    drawdown_3m = "price_structure",
    beta_3m = "price_structure",
    volume_trend_1m = "volume",
    liquidity_proxy = "volume",
    rel_strength_3m = "price_structure",
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

generate_time_series_split <- function(df, train_ratio = 0.7, date_col = "date", ticker_col = "ticker", valid_ratio = 0.15, use_rolling_window = FALSE, rolling_window_size = 252) {
  train_parts <- list(); valid_parts <- list(); test_parts <- list()
  for (tk in unique(df[[ticker_col]])) {
    d <- df[df[[ticker_col]] == tk, , drop = FALSE]
    d <- d[order(d[[date_col]]), , drop = FALSE]
    n <- nrow(d); if (n < 3) next
    n_train <- max(1, floor(n * train_ratio)); n_train <- min(n_train, n - 2)
    n_valid <- max(1, floor(n * valid_ratio)); n_valid <- min(n_valid, n - n_train - 1)
    valid_start <- n_train + 1
    valid_end <- n_train + n_valid
    test_start <- valid_end + 1
    if (isTRUE(use_rolling_window)) {
      idx_start <- max(1, n_train - rolling_window_size + 1)
      train_parts[[length(train_parts) + 1]] <- d[idx_start:n_train, , drop = FALSE]
    } else {
      train_parts[[length(train_parts) + 1]] <- d[seq_len(n_train), , drop = FALSE]
    }
    valid_parts[[length(valid_parts) + 1]] <- d[valid_start:valid_end, , drop = FALSE]
    test_parts[[length(test_parts) + 1]] <- d[test_start:n, , drop = FALSE]
  }
  list(train = do.call(rbind, train_parts), valid = do.call(rbind, valid_parts), test = do.call(rbind, test_parts))
}

fit_ols_model <- function(train_df, formula_obj) lm(formula_obj, data = train_df)
score_model <- function(model, valid_df, target_col, compute_ic = TRUE, pred_sign = 1) {
  pred <- pred_sign * as.numeric(predict(model, newdata = valid_df)); actual <- as.numeric(valid_df[[target_col]])
  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  ic <- if (isTRUE(compute_ic) && length(pred) > 1) suppressWarnings(cor(pred, actual, use = "complete.obs")) else NA_real_
  list(rmse = rmse, ic = ic)
}
