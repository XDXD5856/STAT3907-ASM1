# utils_io.R

create_output_dirs <- function(config) {
  dirs <- c(
    config$paths$data_raw,
    config$paths$data_processed,
    config$paths$results,
    config$paths$report,
    config$paths$submission,
    config$paths$logs,
    config$files$stage12_ticker_dir,
    config$files$stage3_ticker_dir,
    dirname(config$files$stage4_all_models_partial)
  )
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  invisible(dirs)
}

append_log_line <- function(path, msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  cat(line, "\n")
  cat(line, "\n", file = path, append = TRUE)
}

write_stage_log <- function(stage_key, message_text, config) append_log_line(config$logs[[stage_key]], message_text)
write_master_log <- function(message_text, config) append_log_line(config$logs$master, message_text)

save_stage_plot <- function(plot_expr, file_path, width = 1000, height = 700) {
  grDevices::png(file_path, width = width, height = height)
  try(plot_expr(), silent = TRUE)
  grDevices::dev.off()
}
