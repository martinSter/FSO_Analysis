# FSO Analysis

R pipeline that models premature apprenticeship-contract termination
("Lehrvertragsauflösung", LVA) using Swiss Federal Statistical Office (FSO)
cohort data, and validates the resulting models against a separate TREE
survey testset. This repository is one of two companion repositories for
the associated publication; the other holds the TREE-based analysis.

## Pipeline

Run the scripts in order:

| Script | Description |
| --- | --- |
| `01_import_FSO_data.R` | Imports and cleans the 2018/2019 FSO cohort Excel files, imputes suppressed ("\*") cells. |
| `02_Bayesian_estimation.R` | Pooled Bayesian and Empirical Bayes estimation of termination probability. |
| `03_explore_FSO_data.R` | Exploratory analysis and figures on the FSO data. |
| `04_Naive_Bayes.R` | Naive Bayes model over duration, gender, nationality, profession. |
| `05_Tree-Augmented_Naive_Bayes.R` | Tree-augmented Naive Bayes variant. |
| `06_Explainable_Predictions.R` | Log-odds decomposition plot for individual predictions. |
| `07_Evaluate_TREE_Testset.R` | Validates all FSO-derived models against the TREE train/test set. |

Each script reads its inputs from `Data/` and/or `Results/`, and writes
intermediate `.rds` results to `Results/`.

## Data

- `Data/su-d-15.10.03-06-Kohorte2018.xlsx`, `Data/su-d-15.10.03-06-Kohorte2019.xlsx`
  — FSO cohort tables, included in this repository.
- `Data/train_BFS.rds`, `Data/test_BFS.rds` — TREE train/test split,
  produced by the companion TREE repository and **not included here**.
  Copy them into `Data/` before running `07_Evaluate_TREE_Testset.R`.

`Results/` is created automatically by the scripts (`dir.create("Results", ...)`)
and populated by running them in order; it is not tracked in git (see
`.gitignore`).

## Requirements

R packages: `tidyverse`, `tidymodels` (only used in `07_Evaluate_TREE_Testset.R`),
`readxl`, `zoo`, `grid`, `pROC`.

## Note

Plot export (`ggsave(...)`) calls are commented out by default; uncomment
them to regenerate the figures used in the publication (a `Plots/` folder
is expected in that case).
