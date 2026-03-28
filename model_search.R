# model_search.R
# Stage 3: Search best OLS model with for-loops and time-series split.

search_best_model <- function(dataset, config) {
  # TODO:
  # 1) Time-series split (no random split)
  # 2) Define candidate feature sets
  # 3) Use for-loop to fit lm() models
  # 4) Compare by validation metric

  stop("search_best_model() not implemented yet.")
}

generate_time_series_split <- function(df, train_ratio = 0.7) {
  # TODO: Return train/validation index split preserving time order.
  stop("generate_time_series_split() not implemented yet.")
}

fit_ols_model <- function(train_df, formula_obj) {
  # TODO: Fit linear model and return model object.
  stop("fit_ols_model() not implemented yet.")
}

score_model <- function(model, valid_df, target_col) {
  # TODO: Compute evaluation metric (e.g., RMSE/MAE/IC).
  stop("score_model() not implemented yet.")
}
