# *****************************************************
# -------------------- VET 2025 -----------------------
#
# Fachhochschule Nordwestschweiz
# Riggenbachstrasse 16
# 4600 Olten
#
# Author: Martin Sterchi
# Date: 09.04.2026
# Description: Validate all FSO models on TREE testset
#
# *****************************************************
# 1. Prepare setup ------------------------------------

# Clean up workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(tidymodels)

# Custom equal error rate function
eer <- function(truth, prob) {
  roc_obj <- pROC::roc(truth, prob, quiet = TRUE)
  coords <- pROC::coords(roc_obj, x = "all", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  fpr <- 1 - coords$specificity
  fnr <- 1 - coords$sensitivity
  idx <- which.min(abs(fpr - fnr))
  eer <- mean(c(fpr[idx], fnr[idx]))
  return(eer)
}

# Import TREE testset
train <- read_rds("Data/train_BFS.rds")
test <- read_rds("Data/test_BFS.rds")

# Import estimated models (based on FSO data)
out <- read_rds("RES/out.rds") # Tree-augmented Naive Bayes
out_TNB <- read_rds("RES/out_TNB.rds") # True Naive Bayes
out_EB <- read_rds("RES/df2018_eb.rds") # Empirical Bayes

# Sufficient statistics (based on FSO data).
# n1: number of premature terminations in 2018 cohort
# n0: number of successful completions in 2018 cohort
# The numbers here are slightly different than when
# you sum the columns in 'df2018' because of the 
# imputation (due to small rounding errors).
n1 <- 13039
n0 <- 40311

# MAP estimate of Beta posterior (analytical result)
(theta_map <- n1 / (n0 + n1))


# *****************************************************
# 2. Match TREE testset to FSO terminology ------------

# Update some job titles in 'test' so they match FSO data.
train$Job[train$Job == "Carrossier Lackiererei EFZ"] <- "Carrosserielackierer EFZ"
train$Job[train$Job == "Carrossier Spenglerei EFZ"] <- "Carrosseriespengler EFZ"
test$Job[test$Job == "Carrossier Lackiererei EFZ"] <- "Carrosserielackierer EFZ"
test$Job[test$Job == "Carrossier Spenglerei EFZ"] <- "Carrosseriespengler EFZ"

# Change job titles so they match with 'valid' (gender forms).
out$Job <- gsub("/in", "", out$Job)
out$Job <- gsub("/-frau", "", out$Job)
out$Job <- gsub("/-euse", "", out$Job)
out$Job <- gsub("/-technologin", "", out$Job)
out$Job <- gsub("/-polsterin", "", out$Job)

# Adjust exceptions.
out$Job[out$Job == "Koch/Köchin EFZ"] <- "Koch EFZ"
out$Job[out$Job == "Zimmermann/Zimmerin EFZ"] <- "Zimmermann EFZ"
out$Job[out$Job == "Tiermedizinische/r Praxisassistent EFZ"] <- "Tiermedizinischer Praxisassistent EFZ"
out$Job[out$Job == "Hotellerieangestellte/r EBA"] <- "Hotellerieangestellter EBA"
out$Job[out$Job == "Restaurationsangestellte/r EBA"] <- "Restaurationsangestellter EBA"
out$Job[out$Job == "Medizinische/r Praxisassistent EFZ"] <- "Medizinischer Praxisassistent EFZ"
out$Job[out$Job == "Küchenangestellte/r EBA"] <- "Küchenangestellter EBA"
out$Job[out$Job == "Podologe/Podologin EFZ"] <- "Podologe EFZ"

# Change job titles so they match with 'valid' (gender forms).
out_TNB$Job <- gsub("/in", "", out_TNB$Job)
out_TNB$Job <- gsub("/-frau", "", out_TNB$Job)
out_TNB$Job <- gsub("/-euse", "", out_TNB$Job)
out_TNB$Job <- gsub("/-technologin", "", out_TNB$Job)
out_TNB$Job <- gsub("/-polsterin", "", out_TNB$Job)

# Adjust exceptions.
out_TNB$Job[out_TNB$Job == "Koch/Köchin EFZ"] <- "Koch EFZ"
out_TNB$Job[out_TNB$Job == "Zimmermann/Zimmerin EFZ"] <- "Zimmermann EFZ"
out_TNB$Job[out_TNB$Job == "Tiermedizinische/r Praxisassistent EFZ"] <- "Tiermedizinischer Praxisassistent EFZ"
out_TNB$Job[out_TNB$Job == "Hotellerieangestellte/r EBA"] <- "Hotellerieangestellter EBA"
out_TNB$Job[out_TNB$Job == "Restaurationsangestellte/r EBA"] <- "Restaurationsangestellter EBA"
out_TNB$Job[out_TNB$Job == "Medizinische/r Praxisassistent EFZ"] <- "Medizinischer Praxisassistent EFZ"
out_TNB$Job[out_TNB$Job == "Küchenangestellte/r EBA"] <- "Küchenangestellter EBA"
out_TNB$Job[out_TNB$Job == "Podologe/Podologin EFZ"] <- "Podologe EFZ"

# Same for 'out_EB' (it contains p(y|x_P) predictions)
out_EB$Jobs <- gsub("/in", "", out_EB$Jobs)
out_EB$Jobs <- gsub("/-frau", "", out_EB$Jobs)
out_EB$Jobs <- gsub("/-euse", "", out_EB$Jobs)
out_EB$Jobs <- gsub("/-technologin", "", out_EB$Jobs)
out_EB$Jobs <- gsub("/-polsterin", "", out_EB$Jobs)

out_EB$Jobs[out_EB$Jobs == "Koch/Köchin EFZ"] <- "Koch EFZ"
out_EB$Jobs[out_EB$Jobs == "Zimmermann/Zimmerin EFZ"] <- "Zimmermann EFZ"
out_EB$Jobs[out_EB$Jobs == "Tiermedizinische/r Praxisassistent EFZ"] <- "Tiermedizinischer Praxisassistent EFZ"
out_EB$Jobs[out_EB$Jobs == "Hotellerieangestellte/r EBA"] <- "Hotellerieangestellter EBA"
out_EB$Jobs[out_EB$Jobs == "Restaurationsangestellte/r EBA"] <- "Restaurationsangestellter EBA"
out_EB$Jobs[out_EB$Jobs == "Medizinische/r Praxisassistent EFZ"] <- "Medizinischer Praxisassistent EFZ"
out_EB$Jobs[out_EB$Jobs == "Küchenangestellte/r EBA"] <- "Küchenangestellter EBA"
out_EB$Jobs[out_EB$Jobs == "Podologe/Podologin EFZ"] <- "Podologe EFZ"


# *****************************************************
# 3. Join dataframes and deal with missing Nat. -------

# Left join of model predictions for complete observations.
train1 <- train |> 
  filter(!is.na(Nationality)) |> 
  # Join predictions of not-so Naive Bayes.
  left_join(out |> select("Type", "Gender", "Nationality", "Job", "pred_LVA"), 
            by = c("Type", "Gender", "Nationality", "Job")) |> 
  # Join predictions of truly Naive Bayes.
  left_join(out_TNB |> select("Type", "Gender", "Nationality", "Job", "pred_LVA"), 
            by = c("Type", "Gender", "Nationality", "Job"),
            suffix = c("_NB", "_TNB"))

# Left join of model predictions for observations with missing 'Nationality'.
train2 <- train |> 
  filter(is.na(Nationality)) |> 
  # Join predictions of not-so Naive Bayes.
  left_join(out |> select("Type", "Gender", "Job", "pred_LVA_missingNat") |> distinct(), 
            by = c("Type", "Gender", "Job")) |> 
  # Join predictions of truly Naive Bayes.
  left_join(out_TNB |> select("Type", "Gender", "Job", "pred_LVA_missingNat") |> distinct(), 
            by = c("Type", "Gender", "Job")) |>
  rename(pred_LVA_NB = pred_LVA_missingNat.x,
         pred_LVA_TNB = pred_LVA_missingNat.y)

# Put them back together.
train <- train1 |> 
  bind_rows(train2)

# ------------------------------------------------------------------------------

# Left join of model predictions for complete observations.
test1 <- test |> 
  filter(!is.na(Nationality)) |> 
  # Join predictions of not-so Naive Bayes.
  left_join(out |> select("Type", "Gender", "Nationality", "Job", "pred_LVA"), 
            by = c("Type", "Gender", "Nationality", "Job")) |> 
  # Join predictions of truly Naive Bayes.
  left_join(out_TNB |> select("Type", "Gender", "Nationality", "Job", "pred_LVA"), 
            by = c("Type", "Gender", "Nationality", "Job"),
            suffix = c("_NB", "_TNB"))

# Left join of model predictions for observations with missing 'Nationality'.
test2 <- test |> 
  filter(is.na(Nationality)) |> 
  # Join predictions of not-so Naive Bayes.
  left_join(out |> select("Type", "Gender", "Job", "pred_LVA_missingNat") |> distinct(), 
            by = c("Type", "Gender", "Job")) |> 
  # Join predictions of truly Naive Bayes.
  left_join(out_TNB |> select("Type", "Gender", "Job", "pred_LVA_missingNat") |> distinct(), 
            by = c("Type", "Gender", "Job")) |>
  rename(pred_LVA_NB = pred_LVA_missingNat.x,
         pred_LVA_TNB = pred_LVA_missingNat.y)

# Put them back together.
test <- test1 |> 
  bind_rows(test2)

# ------------------------------------------------------------------------------

# Two jobs in 'train' and 'test' cannot be matched:
# 1. "Im Sommer 2018 werden erstmals Lernende ihre Ausbildung 
# im neuen Beruf „ICT Fachfrau/Fachmann EFZ“ starten. 
# Die neue 3-jährige Lehre ersetzt den Beruf 
# „Informatikpraktiker/in EBA“."
# 2. Holzhandwerker EFZ gibt es in den BFS Daten nicht.
# We remove the corresponding rows in 'valid'.
train <- train[!(train$Job %in% c("Informatikpraktiker EBA", "Holzhandwerker EFZ")), ]
test <- test[!(test$Job %in% c("Informatikpraktiker EBA", "Holzhandwerker EFZ")), ]

# Baseline model
train$pred_BL <- theta_map
test$pred_BL <- theta_map

# Hierarchical Bayes for x_P
train <- train |> 
  left_join(out_EB |> select(Jobs, eb_posterior_mean),
            by = c("Job" = "Jobs"))

test <- test |> 
  left_join(out_EB |> select(Jobs, eb_posterior_mean),
            by = c("Job" = "Jobs"))

# Make sure there are no more missing values.
# The "Nationality" missings are not a problem.
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))


# *****************************************************
# 4. Evaluate models on training set ------------------

# Make a factor with terminations being the first level,
# so it matches default event level of yardstick.
train$LVA <- factor(train$LVA, levels = c(1, 0), 
                    labels = c("Discontinue before completion", "Complete it and pass"))

# Imbalance
prop.table(table(train$LVA))

# ROC AUC
roc_auc(train, LVA, pred_BL)
roc_auc(train, LVA, eb_posterior_mean)
roc_auc(train, LVA, pred_LVA_NB)
roc_auc(train, LVA, pred_LVA_TNB)

# Average precision
average_precision(train, LVA, pred_BL)
average_precision(train, LVA, eb_posterior_mean)
average_precision(train, LVA, pred_LVA_NB)
average_precision(train, LVA, pred_LVA_TNB)

# Equal error rate
eer(train$LVA, train$pred_BL)
eer(train$LVA, train$eb_posterior_mean)
eer(train$LVA, train$pred_LVA_NB)
eer(train$LVA, train$pred_LVA_TNB)


# *****************************************************
# 5. Evaluate models on test set ----------------------

# Make a factor with terminations being the first level,
# so it matches default event level of yardstick.
test$LVA <- factor(test$LVA, levels = c(1, 0), 
                    labels = c("Discontinue before completion", "Complete it and pass"))

# Imbalance
prop.table(table(test$LVA))

# ROC AUC
roc_auc(test, LVA, pred_BL)
roc_auc(test, LVA, eb_posterior_mean)
roc_auc(test, LVA, pred_LVA_NB)
roc_auc(test, LVA, pred_LVA_TNB)

# Average precision
average_precision(test, LVA, pred_BL)
average_precision(test, LVA, eb_posterior_mean)
average_precision(test, LVA, pred_LVA_NB)
average_precision(test, LVA, pred_LVA_TNB)

# Equal error rate
eer(test$LVA, test$pred_BL)
eer(test$LVA, test$eb_posterior_mean)
eer(test$LVA, test$pred_LVA_NB)
eer(test$LVA, test$pred_LVA_TNB)

# Encode target numerically for Brier score.
target_numeric <- ifelse(test$LVA == "Discontinue before completion", 1, 0)

# Brier score
mean((0.5 - target_numeric)^2)
mean((test$pred_BL - target_numeric)^2)
mean((test$eb_posterior_mean - target_numeric)^2)
mean((test$pred_LVA_NB - target_numeric)^2)
mean((test$pred_LVA_TNB - target_numeric)^2)

# Log score
mean(ifelse(target_numeric == 1, log(test$pred_BL), log(1 - test$pred_BL)))
mean(ifelse(target_numeric == 1, log(test$eb_posterior_mean), log(1 - test$eb_posterior_mean)))
mean(ifelse(target_numeric == 1, log(test$pred_LVA_NB), log(1 - test$pred_LVA_NB)))
mean(ifelse(target_numeric == 1, log(test$pred_LVA_TNB), log(1 - test$pred_LVA_TNB)))

# ROC curves
roc_curve(test, LVA, pred_LVA_NB) |> 
  mutate(model = "Tree-augm. Naive Bayes") |> 
  bind_rows(
    roc_curve(test, LVA, pred_LVA_TNB) |> 
      mutate(model = "Naive Bayes")
  ) |> 
  bind_rows(
    roc_curve(test, LVA, eb_posterior_mean) |> 
      mutate(model = "Empirical Bayes")
  ) |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(linewidth = 1, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6) +
  theme_bw()

# Precision-Recall curve
autoplot(pr_curve(test, LVA, pred_LVA_NB))

# Function to do it for all three relevant measures.
boot_metrics <- function(dat, pred_label, R = 2000, seed = 1) {
  set.seed(seed)
  p <- dat[[pred_label]]
  y <- dat$LVA
  y01 <- as.integer(y == "Discontinue before completion")
  n <- nrow(dat)
  
  map_dfr(seq_len(R), \(b) {
    i <- sample.int(n, replace = TRUE)
    tibble(
      auc   = roc_auc_vec(y[i], p[i], event_level = "first"),
      ap    = average_precision_vec(y[i], p[i], event_level = "first"),
      brier = mean((p[i] - y01[i])^2)
    )
  })
}

# Run the bootstrap
bm <- boot_metrics(test, "pred_BL")
bm <- boot_metrics(test, "eb_posterior_mean")
bm <- boot_metrics(test, "pred_LVA_NB")
bm <- boot_metrics(test, "pred_LVA_TNB")

# Get the CI limits and SE.
bm |>
  pivot_longer(everything(), names_to = "metric") |>
  group_by(metric) |>
  summarise(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    se    = sd(value),
    one_half = se * 1.96
  )

