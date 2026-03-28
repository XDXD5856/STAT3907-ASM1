# utils_io.R
# Shared I/O utilities for stage outputs and logs

create_output_dirs <- function(config) {
  dirs <- c(
    config$paths$logs,
    config$paths$outputs_root,
    config$paths$stage1,
    config$paths$stage2,
    config$paths$stage3,
    config$paths$stage4,
    config$paths$stage5,
    config$paths$stage6
  )

  for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(dirs)
}

append_log_line <- function(path, msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  cat(line, "\n")
  cat(line, "\n", file = path, append = TRUE)
}

write_stage_log <- function(stage_name, message_text, config) {
  log_path <- config$logs[[stage_name]]
  append_log_line(log_path, message_text)
}

write_master_log <- function(message_text, config) {
  append_log_line(config$logs$master, message_text)
}

save_stage_plot <- function(plot_expr, file_path, width = 1000, height = 700) {
  grDevices::png(filename = file_path, width = width, height = height)
  try(plot_expr(), silent = TRUE)
  grDevices::dev.off()
}
