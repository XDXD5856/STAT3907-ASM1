# STAT3907-ASM1 (R Project Skeleton)

This repository currently contains a modular skeleton for a Hong Kong stock selection pipeline.

## Current Status
- Stage files and function skeletons are created.
- Full stage logic is intentionally not implemented yet.

## Run Locally
1. Install R (recommended 4.2+).
2. Install required packages:

```r
install.packages(c("quantmod", "tidyverse", "lubridate", "broom"))
```

3. Run from project root:

```bash
Rscript main.R
```

Expected current output:
- `Starting pipeline...`
- `Pipeline structure created. Logic implementation pending.`

## Project Files
- `main.R` — entrypoint and stage orchestration order
- `config.R` — package checks, global config, directory initialization
- `data_loader.R` — Stage 1 skeleton
- `feature_engineering.R` — Stage 2 skeleton
- `model_search.R` — Stage 3 skeleton
- `predict.R` — Stage 4/5 skeleton
- `report.R` — Stage 6 skeleton
