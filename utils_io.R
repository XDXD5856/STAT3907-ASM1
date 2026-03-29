# utils_io.R

create_output_dirs <- function(config) {
  file_dirs <- unique(dirname(unlist(config$files, use.names = FALSE)))
  run_nested_dirs <- file.path(
    config$paths$results,
    c("results", "report", "submission", "logs")
  )
  dirs <- unique(c(
    config$paths$data_raw,
    config$paths$data_processed,
    config$paths$results,
    config$paths$report,
    config$paths$submission,
    config$paths$logs,
    file_dirs,
    run_nested_dirs
  ))
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  invisible(dirs)
}

ensure_parent_dir <- function(path) {
  parent <- dirname(path)
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  invisible(parent)
}

safe_write_csv <- function(df, path) {
  ensure_parent_dir(path)
  utils::write.csv(df, path, row.names = FALSE)
}

safe_write_lines <- function(lines, path) {
  ensure_parent_dir(path)
  writeLines(lines, path)
}

list_run_ids <- function(results_root) {
  if (!dir.exists(results_root)) return(character(0))
  runs <- list.dirs(results_root, recursive = FALSE, full.names = FALSE)
  runs[grepl("^run_[0-9]{3}$", runs)]
}

get_latest_run_id <- function(results_root) {
  runs <- list_run_ids(results_root)
  if (length(runs) == 0) return(NULL)
  runs[order(runs, decreasing = TRUE)][1]
}

get_next_run_id <- function(results_root) {
  runs <- list_run_ids(results_root)
  if (length(runs) == 0) return("run_001")
  ids <- as.integer(sub("^run_", "", runs))
  sprintf("run_%03d", max(ids, na.rm = TRUE) + 1L)
}

stage_done_marker_map <- function(config) {
  list(
    stage12 = file.path(config$paths$results, "stage1_2.done"),
    stage3 = file.path(config$paths$results, "stage3.done"),
    stage4 = file.path(config$paths$results, "stage4.done"),
    stage5 = file.path(config$paths$results, "stage5.done"),
    stage6 = file.path(config$paths$results, "stage6.done")
  )
}

is_stage_done <- function(stage_key, config) {
  markers <- stage_done_marker_map(config)
  file.exists(markers[[stage_key]])
}

mark_stage_done <- function(stage_key, config) {
  markers <- stage_done_marker_map(config)
  safe_write_lines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), markers[[stage_key]])
}

is_run_complete <- function(config) {
  all(vapply(c("stage12", "stage3", "stage4", "stage5", "stage6"), is_stage_done, logical(1), config = config))
}

get_latest_unfinished_run_id <- function(base_config) {
  results_root <- base_config$paths$results
  runs <- list_run_ids(results_root)
  if (length(runs) == 0) return(NULL)
  runs <- runs[order(runs, decreasing = TRUE)]
  for (rid in runs) {
    cfg <- apply_run_paths(base_config, rid)
    if (!is_run_complete(cfg)) return(rid)
  }
  NULL
}

apply_run_paths <- function(config, run_id) {
  base_paths <- config$paths
  shared_root <- if (!is.null(config$shared_data_path)) config$shared_data_path else "data"
  shared_raw <- file.path(shared_root, "raw")
  shared_processed <- file.path(shared_root, "processed")
  shared_model_cache <- file.path(shared_root, "model_cache", "model_results.csv")
  run_results <- file.path(base_paths$results, run_id)
  run_logs <- file.path(base_paths$logs, run_id)
  run_report <- file.path(base_paths$report, run_id)
  run_submission <- file.path(base_paths$submission, run_id)

  config$run_id <- run_id
  manifest_path <- file.path(run_results, "run_manifest.txt")
  existing_start <- NA_character_
  if (file.exists(manifest_path)) {
    mf <- readLines(manifest_path, warn = FALSE)
    st <- mf[grepl("^start_time: ", mf)]
    if (length(st) > 0) existing_start <- sub("^start_time: ", "", st[1])
  }

  config$run <- list(
    run_id = run_id,
    manifest_path = manifest_path,
    started_at = if (!is.na(existing_start) && nzchar(existing_start)) existing_start else format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  config$paths$results <- run_results
  config$paths$logs <- run_logs
  config$paths$report <- run_report
  config$paths$submission <- run_submission

  config$logs <- list(
    master = file.path(run_logs, "pipeline_master.log"),
    stage12 = file.path(run_logs, "stage1_2_data_download.log"),
    stage3 = file.path(run_logs, "stage3_features.log"),
    stage4 = file.path(run_logs, "stage4_model_search.log"),
    stage5 = file.path(run_logs, "stage5_predictions.log"),
    stage6 = file.path(run_logs, "stage6_report.log")
  )

  config$files <- list(
    stage12_universe = file.path(shared_raw, "universe_tickers.csv"),
    stage12_raw_panel = file.path(shared_raw, "hk_stock_price_data.csv"),
    stage12_failed = file.path(shared_raw, "failed_stocks.csv"),
    stage12_ticker_dir = file.path(shared_raw, "tickers"),

    stage3_ticker_dir = file.path(shared_processed, "tickers_features"),
    stage3_model_panel = file.path(shared_processed, "feature_panel.csv"),
    stage3_summary = file.path(shared_processed, "feature_summary.csv"),
    stage3_plot_target = file.path(run_results, "stage3_target_21d_distribution.png"),
    stage3_plot_ret1d = file.path(run_results, "stage3_ret_1d_distribution.png"),

    stage4_all_models = file.path(run_results, "all_models.csv"),
    stage4_model_cache = shared_model_cache,
    stage4_best_model_summary = file.path(run_results, "best_model_summary.txt"),
    stage4_summary = file.path(run_results, "stage4_summary.txt"),
    stage4_model_complexity_summary = file.path(run_results, "stage4_model_complexity_summary.csv"),
    stage4_best_predictors = file.path(run_results, "best_predictors.csv"),
    stage4_split_info = file.path(run_results, "split_info.csv"),
    stage4_backtest = file.path(run_results, "backtest_pred_vs_actual.csv"),
    stage4_pred_vs_actual = file.path(run_results, "predicted_vs_actual.csv"),
    stage4_plot_rmse = file.path(run_results, "rmse_by_model_rank.png"),
    stage4_plot_top20 = file.path(run_results, "top20_models_bar.png"),
    stage4_plot_scatter = file.path(run_results, "best_model_pred_vs_actual.png"),
    stage4_all_models_partial = file.path(run_results, "outputs/stage4_model_search/all_models_partial.csv"),

    stage5_predictions = file.path(run_results, "predictions_latest.csv"),
    stage5_latest_obs = file.path(run_results, "latest_observations_used.csv"),
    stage5_hist = file.path(run_results, "predictions_histogram.png"),
    stage5_top20 = file.path(run_results, "top20_predicted_chart.png"),

    stage6_ranked = file.path(run_report, "ranked_stocks.csv"),
    stage6_top_pick = file.path(run_report, "top_pick.csv"),
    stage6_final_report = file.path(run_report, "final_report.txt"),
    stage6_cumlog10_plot = file.path(run_report, "cumulative_log10_return.png"),
    stage6_monthly_return_plot = file.path(run_report, "monthly_return.png"),
    stage6_backtest_results = file.path(run_report, "backtest_results.csv"),
    stage6_backtest_summary = file.path(run_report, "backtest_summary.csv"),
    submission_ranked = file.path(run_submission, "ranked_stocks.csv"),
    submission_top_pick = file.path(run_submission, "top_pick.csv"),
    submission_report = file.path(run_submission, "final_report.txt")
  )

  config
}

initialize_run_dirs <- function(config) {
  if (!is.null(config$shared_data_path)) {
    config$paths$data_raw <- file.path(config$shared_data_path, "raw")
    config$paths$data_processed <- file.path(config$shared_data_path, "processed")
  }
  results_root <- if (!is.null(config$run_output_path)) config$run_output_path else config$paths$results
  config$paths$results <- results_root
  if (!dir.exists(results_root)) dir.create(results_root, recursive = TRUE, showWarnings = FALSE)

  selected_run <- NULL
  if (isTRUE(config$start_new_run)) {
    selected_run <- get_next_run_id(results_root)
  } else if (!is.null(config$run_id)) {
    selected_run <- config$run_id
    run_cfg <- apply_run_paths(config, selected_run)
    if (dir.exists(run_cfg$paths$results) && isTRUE(is_run_complete(run_cfg)) && !isTRUE(config$resume_run)) {
      selected_run <- get_next_run_id(results_root)
    }
  } else {
    unfinished <- if (isTRUE(config$resume_run)) get_latest_unfinished_run_id(config) else NULL
    selected_run <- if (!is.null(unfinished)) unfinished else get_next_run_id(results_root)
  }

  run_config <- apply_run_paths(config, selected_run)
  create_output_dirs(run_config)
  run_config
}

write_run_manifest <- function(config, end_time = NULL) {
  markers <- stage_done_marker_map(config)
  lines <- c(
    paste0("run_id: ", config$run_id),
    paste0("start_time: ", config$run$started_at),
    paste0("end_time: ", if (is.null(end_time)) "" else format(end_time, "%Y-%m-%d %H:%M:%S")),
    paste0("stage1_2_done: ", file.exists(markers$stage12)),
    paste0("stage3_done: ", file.exists(markers$stage3)),
    paste0("stage4_done: ", file.exists(markers$stage4)),
    paste0("stage5_done: ", file.exists(markers$stage5)),
    paste0("stage6_done: ", file.exists(markers$stage6)),
    paste0("stage12_raw_panel: ", config$files$stage12_raw_panel),
    paste0("stage3_model_panel: ", config$files$stage3_model_panel),
    paste0("stage4_all_models: ", config$files$stage4_all_models),
    paste0("stage5_predictions: ", config$files$stage5_predictions),
    paste0("stage6_report: ", config$files$stage6_final_report),
    paste0("submission_ranked: ", config$files$submission_ranked)
  )
  safe_write_lines(lines, config$run$manifest_path)
}

is_console_important_message <- function(stage_key, message_text) {
  if (identical(stage_key, "master")) {
    return(
      grepl("^Pipeline (started|finished)$", message_text) ||
      grepl("^Running Stage [0-9+]+$", message_text) ||
      grepl("^Stage [0-9+].* done:", message_text)
    )
  }

  grepl("^Stage [0-9+].*(started|completed)", message_text) || grepl("^Progress:", message_text)
}

append_log_line <- function(path, msg, to_console = TRUE) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  if (isTRUE(to_console)) cat(line, "\n")
  ensure_parent_dir(path)
  cat(line, "\n", file = path, append = TRUE)
}

write_stage_log <- function(stage_key, message_text, config) {
  echo <- isTRUE(config$verbose_console) || is_console_important_message(stage_key, message_text)
  append_log_line(config$logs[[stage_key]], message_text, to_console = echo)
}

write_master_log <- function(message_text, config) {
  echo <- isTRUE(config$verbose_console) || is_console_important_message("master", message_text)
  append_log_line(config$logs$master, message_text, to_console = echo)
}

save_stage_plot <- function(plot_expr, file_path, width = 1000, height = 700) {
  ensure_parent_dir(file_path)
  grDevices::png(file_path, width = width, height = height)
  try(plot_expr(), silent = TRUE)
  grDevices::dev.off()
}
