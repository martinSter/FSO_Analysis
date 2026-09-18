# FSO Analysis

R pipeline that models premature apprenticeship-contract termination
("Lehrvertragsauflösung", LVA) using Swiss Federal Statistical Office (FSO)
cohort data, and validates the resulting models against a separate TREE2
survey test set. This repository is one of two companion repositories for
the associated publication. The other holds the TREE2-based analysis.

## Pipeline

Run the scripts in order:

| Script | Description |
| --- | --- |
| `01_import_FSO_data.R` | Imports and cleans the 2018/2019 FSO cohort Excel files, imputes privacy-related ("\*") cells. |
| `02_Bayesian_estimation.R` | Pooled Bayesian and Empirical Bayes estimation of termination probability, validates predictions against the 2019 cohort. |
| `03_explore_FSO_data.R` | Exploratory analysis and figures of the FSO data. |
| `04_Naive_Bayes.R` | Naive Bayes model over duration, gender, nationality, profession. |
| `05_Tree_Augmented_Naive_Bayes.R` | Tree-augmented Naive Bayes variant. |
| `06_Explainable_Predictions.R` | Log-odds decomposition plot for individual predictions. |
| `07_Evaluate_TREE_Testset.R` | Validates all FSO-derived models (baseline, Empirical Bayes, Naive Bayes, TAN) against the TREE2 train/test set: ROC AUC, average precision, Brier/log score, ROC curve, and bootstrap CIs. |

Each script reads its inputs from `Data/` and/or `Results/`, and writes
intermediate `.rds` results to `Results/`. Scripts `01`–`05` create the
`Results/` folder automatically (`dir.create("Results", ...)`) if it
doesn't exist yet.

## Data

- `Data/su-d-15.10.03-06-Kohorte2018.xlsx`, `Data/su-d-15.10.03-06-Kohorte2019.xlsx`
  — FSO cohort tables, included in this repository.
- `Data/train_BFS.rds`, `Data/test_BFS.rds` — TREE train/test split. This
  data cannot be shared publicly. Request the data from the TREE study team
  (https://www.tree.unibe.ch/index_eng.html) and follow the code in the companion repository.

`Results/` is populated by running the scripts in order. It is not tracked
in git (see `.gitignore`).

## Requirements

Analysis was run with **R 4.5.3** (2026-03-11) on Windows. Package
versions:

| Package | Version | Used in |
| --- | --- | --- |
| tidyverse | 2.0.0 | all scripts |
| readxl | 1.5.0 | `01`, `03`, `04`, `05` |
| zoo | 1.9-0 | `01`, `03` |
| scales | 1.4.0 | `02`, `03` |
| grid | 4.5.3 (base R) | `03` |
| tidymodels | 1.5.0 | `07` |

## Note

Plot export (`ggsave(...)`) calls in `02_Bayesian_estimation.R`, `03_explore_FSO_data.R`, and
`06_Explainable_Predictions.R` are commented out by default. Uncomment
them to export figures to `Results/`.
