# HKEX Stock Selection Pipeline (R)

This project builds a full Stage 1-6 pipeline for Hong Kong stock selection:

1. Universe construction (HKEX official list -> qualified stocks)
2. Yahoo Finance data download (`quantmod::getSymbols(..., src = "yahoo")`)
3. Feature engineering + `target_21d`
4. OLS model selection via exhaustive predictor combinations
5. Next-period prediction per ticker (latest observation)
6. Ranking and final top-stock investment choice

## Run

```r
install.packages(c("quantmod", "tidyverse", "lubridate", "broom", "zoo"))
```

```bash
Rscript main.R
```

## Stage I/O

- Stage 1 output: qualified stock universe (`data.frame` with `ticker`)
- Stage 2 output: raw panel with columns:
  `ticker, date, open, high, low, close, volume, adjusted_close`
- Stage 3 output: model-ready panel with required columns:
  `ticker, date, target_21d` + numeric predictors
- Stage 4 output: best model object + model metrics table (`all_models`)
- Stage 5 output: one row per ticker:
  `ticker, prediction_date, predicted_return_21d`
- Stage 6 output:
  ranked table + top-pick table (HKD 1,000,000 all-in single stock)

## Notes

- Model uses OLS (`lm`) only.
- Predictor combinations are tested with explicit for-loops.
- Time-series split is used (no random shuffle).
- `max_predictors` is read from config (default 15).
