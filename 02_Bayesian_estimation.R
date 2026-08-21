# *****************************************************
# -------------------- VET Study ----------------------
#
# Fachhochschule Nordwestschweiz
# Riggenbachstrasse 16
# 4600 Olten
#
# Author: Martin Sterchi
# Date: 21.08.2026
# Description: Bayesian Estimation
#
# *****************************************************
# 1. Prepare setup ------------------------------------

# Clean up workspace
rm(list = ls())

# Load libraries
library(tidyverse)

# Create output directory if it does not exist yet.
dir.create("Results", showWarnings = FALSE)

# Load data from previous step.
df2018 <- read_rds("Results/df2018.rds")
df2019 <- read_rds("Results/df2019.rds")

# *****************************************************
# 2. Simple (pooled) model ----------------------------

# *****************************************************
# 2.1 Estimation --------------------------------------

# Parameter values of Beta prior (results in uniform prior).
a <- 1; b <- 1

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

# Posterior mean of Beta posterior (analytical result)
(theta_mean <- (a + n1) / (a + b + n0 + n1))

# Posterior variance and standard deviations (analytical result)
(posterior_var <- ((a + n1) * (b + n0)) / ((a + n1 + b + n0)^2 * (a + n1 + b + n0 + 1)))
(posterior_sd <- sqrt(posterior_var))

# Simple plot of posterior.
plot(x = seq(0, 1, .001), 
     y = dbeta(seq(0, 1, .001), a + n1, b + n0), 
     type = "l", xlab = "Theta", ylab = "Density")

# As a check: approximation via SEM of sample proportion
sqrt(theta_mean * (1 - theta_mean) / (n0 + n1))

# *****************************************************
# 2.2 Validation --------------------------------------

# Set seed for reproducibility
set.seed(42)

# Draw samples from posterior predictive distribution.
samp <- rbinom(1e6, size = 52937, prob = rbeta(1e6, a + n1, b + n0))

# Compute 95% credible interval
quantile(samp, probs = c(0.025, 0.975))


# *****************************************************
# 3. Empirical Bayes ----------------------------------

# *****************************************************
# 3.1 Find optimal prior parameters -------------------

# Function to be optimized.
# Note: partial help by ChatGPT
log_likelihood <- function(par, data) {
  
  # Get the hyperparameters of the prior
  a <- par[1]
  b <- par[2]
  
  # Get the data
  n <- data$Total
  x <- data$LVA
  
  # Prior constraints to ensure positivity
  if (a <= 0 || b <= 0) return(-Inf)
  
  # Log-likelihood computation
  log_likelihood <- sum(lbeta(x + a, n - x + b) - lbeta(a, b))
  
  # The minus is necessery because the default is minimization
  return(-log_likelihood)
  
}

# Initial parameter values for prior parameters a and b.
x0 <- c(1, 1)

# Optimization with a lower bound on parameters
result <- optim(x0, log_likelihood, data = df2018, method = "L-BFGS-B", lower = c(0.001, 0.001))

# Extract optimal parameter values
(a_hat <- result$par[1])
(b_hat <- result$par[2])

# *****************************************************
# 3.2 Estimation of parameters ------------------------

# We add them all to a new dataframe 'df2018_eb'.
df2018_eb <- df2018

# Simplest approach: assume they are all the same.
# Also called parameter tying or pooled MLE.
df2018_eb$pooled_mle <- sum(df2018_eb$LVA) / sum(df2018_eb$Total)

# Estimate them all separately.
# Problem: sparse data problem.
df2018_eb$indiv_mle <- df2018_eb$LVA / df2018_eb$Total

# Better solution: hierarchical (two-level) model
df2018_eb$eb_posterior_mean <- (a_hat + df2018_eb$LVA) / (a_hat + b_hat + df2018_eb$Total)
df2018_eb$eb_posterior_median <- (a_hat + df2018_eb$LVA - 1/3) / (a_hat + b_hat + df2018_eb$Total - 2/3)

# *****************************************************
# 4. Visualization of results -------------------------

# First we get the data for the next few plots ready.
dfplot <- df2018_eb |>
  # Merge the data from the 2019 cohort.
  left_join(df2019, by = "Jobs", suffix = c(".2018", ".2019")) |>
  # Only keep occupational fields where LVA is not missing in 2019.
  filter(!is.na(LVA.2019)) |> 
  # Sort by posterior mean.
  arrange(eb_posterior_mean) |> 
  # Make the longest job names shorter.
  mutate(Jobs = case_when(
    Jobs == "Fachmann/-frau Bewegungs- und Gesundheitsförderung EFZ" ~ "Fachmann/-frau Bew.- und Ges.förd. EFZ",
    Jobs == "Fachmann/-frau Information und Dokumentation EFZ" ~ "Fachmann/-frau Inf. und Dok. EFZ",
    Jobs == "Chemie- und Pharmatechnologe/-technologin EFZ" ~ "Chemie- und Pharmatechn./-in EFZ",
    TRUE ~ Jobs         # Keep other values unchanged
  )) |> 
  # Make the variable 'Jobs' into a factor.
  mutate(Jobs = factor(Jobs, levels = Jobs)) |> 
  # Compute the 95% credible intervals.
  mutate(
    lo = qbeta(0.025, a_hat + LVA.2018, b_hat + Total.2018 - LVA.2018),
    hi = qbeta(0.975, a_hat + LVA.2018, b_hat + Total.2018 - LVA.2018)
  ) |> 
  # Add variables that help with the alternate shading.
  mutate(
    row_id = row_number(),
    shade = row_id %% 2 != 0
  ) |> 
  # Compute predictions for 2019 cohort.
  mutate(
    pred_pooled_mle = Total.2019 * pooled_mle,
    pred_indiv_mle = Total.2019 * indiv_mle,
    pred_eb_posterior_mean = Total.2019 * eb_posterior_mean,
    pred_eb_posterior_median = Total.2019 * eb_posterior_median
  ) |> 
  # Compute absolute percentage error.
  mutate(
    rel_error = (LVA.2019 - pred_eb_posterior_median) / LVA.2019
  ) |> 
  # Mark rows where EB works better than indiv. MLE
  mutate(
    better = ifelse(abs(LVA.2019 - pred_eb_posterior_median) < abs(LVA.2019 - pred_indiv_mle), 1, 0)
  )

# Markers for legend
markers <- c("Empirical Bayes" = 16, "Indiv. MLE" = 4)

# Visualize them
p1 <- ggplot(data = dfplot, aes(x = Jobs, y = eb_posterior_mean)) +
  # Add credible intervals
  geom_segment(aes(xend = Jobs, y = lo, yend = hi), linewidth = 0.2, color = "grey40") +
  # Add point estimates
  geom_point(aes(shape = "Empirical Bayes"), size = 1, color = "grey20") +
  geom_point(aes(y = indiv_mle, shape = "Indiv. MLE"), size = 1, color = "grey20") +
  geom_rect(data = dfplot %>% filter(shade),
            aes(
              ymin = -Inf,
              ymax = Inf,
              xmin = as.numeric(row_id) - 0.5,
              xmax = as.numeric(row_id) + 0.5
            ),
            fill = "#D41159", alpha = 0.2) +
  geom_rect(data = dfplot %>% filter(!shade),
            aes(
              ymin = -Inf,
              ymax = Inf,
              xmin = as.numeric(row_id) - 0.5,
              xmax = as.numeric(row_id) + 0.5
            ),
            fill = "#1A85FF", alpha = 0.2) +
  # Add pooled MLE as line
  geom_hline(yintercept = 0.24, col = "grey30", linetype = "dashed") +
  # Axis limits
  scale_y_continuous(limits = c(0.05, .55), breaks = seq(0.1, .5, .1), expand = c(0, 0)) +
  scale_shape_manual(NULL, values = markers) +
  # Flip coordinates
  coord_flip() +
  # Theme
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "grey30", linewidth = 0.3),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.3),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 6, colour = "grey30"),
    axis.text.y =  element_text(size = 6, colour = rep(c("#D41159", "#1A85FF"), length.out = nrow(dfplot))),
    axis.title.x =  element_text(size = 9, colour = "grey30"),
    legend.position = "inside",
    legend.position.inside = c(0.81, 0.04),
    legend.background = element_rect(fill = "grey100", color = "grey15", linewidth = 0.2),
    legend.text =  element_text(size = 8, colour = "grey20"),
    legend.margin = margin(c(1, 2, 1.5, 1))
  ) +
  # Axis labels
  labs(
    x = NULL, 
    y = "Probability of premature term."
  )

p1

# Export
# ggsave("Plots/eb.pdf", plot = p1, width = 14,  height = 30,  units = "cm")

# Frequencies
p2 <- ggplot(data = dfplot, aes(x = Jobs, y = Total.2018)) +
  geom_bar(stat = "identity", fill = rep(c("#D41159", "#1A85FF"), length.out = nrow(dfplot)), alpha = 0.4) +
  # Axis limits
  scale_y_continuous(limits = c(0., 7500), breaks = seq(0, 7500, 1500), expand = c(0, 0)) +
  # Flip coordinates
  coord_flip() +
  # Theme
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "grey30", linewidth = 0.3),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.3),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.y =  element_blank(),
    axis.text.x =  element_text(size = 6, colour = "grey30"),
    axis.title.x =  element_text(size = 9, colour = "grey30")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Absolute frequency")

p2

# Export
# ggsave("Plots/eb_freq.pdf", plot = p2, width = 5,  height = 30,  units = "cm")

# Absolute percentage errors
p3 <- ggplot(data = dfplot, aes(x = Jobs, y = rel_error, alpha = better)) +
  geom_bar(stat = "identity", fill = rep(c("#D41159", "#1A85FF"), length.out = nrow(dfplot))) +
  scale_alpha(range = c(0.3, 1)) +
  # Axis limits
  scale_y_continuous(limits = c(-2.2, 0.5), breaks = seq(-2, 0.5, 0.5), expand = c(0, 0), labels = scales::percent) +
  geom_hline(yintercept = 0, col = "grey30", linewidth = 0.3) +
  # Flip coordinates
  coord_flip() +
  # Theme
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "grey30", linewidth = 0.3),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.3),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.y =  element_blank(),
    axis.text.x =  element_text(size = 6, colour = "grey30"),
    axis.title.x =  element_text(size = 9, colour = "grey30")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Percentage error") +
  guides(alpha = "none")

p3

# Export
# ggsave("Plots/eb_rel_error.pdf", plot = p3, width = 5,  height = 30,  units = "cm")

# *****************************************************
# 5. Validation --------------------------------------

# Now we compute the deviations between true label and predictions.
dfplot <- dfplot |> 
  mutate(
    perc_err_pooled_mle = (LVA.2019 - pred_pooled_mle) / LVA.2019,
    perc_err_indiv_mle = (LVA.2019 - pred_indiv_mle) / LVA.2019,
    perc_err_eb_posterior_mean = (LVA.2019 - pred_eb_posterior_mean) / LVA.2019,
    perc_err_eb_posterior_median = (LVA.2019 - pred_eb_posterior_median) / LVA.2019
  )

# Evaluate the predictions with MAPE.
100 * mean(abs(dfplot$perc_err_pooled_mle))
100 * mean(abs(dfplot$perc_err_indiv_mle))
100 * mean(abs(dfplot$perc_err_eb_posterior_mean))
100 * mean(abs(dfplot$perc_err_eb_posterior_median))

sum((dfplot$LVA.2019 - dfplot$pred_pooled_mle)^2) / sum((dfplot$LVA.2019 - mean(dfplot$LVA.2019))^2)
sum((dfplot$LVA.2019 - dfplot$pred_indiv_mle)^2) / sum((dfplot$LVA.2019 - mean(dfplot$LVA.2019))^2)
sum((dfplot$LVA.2019 - dfplot$pred_eb_posterior_mean)^2) / sum((dfplot$LVA.2019 - mean(dfplot$LVA.2019))^2)
sum((dfplot$LVA.2019 - dfplot$pred_eb_posterior_median)^2) / sum((dfplot$LVA.2019 - mean(dfplot$LVA.2019))^2)

# Evaluate the predictions with MSE.
sqrt(mean(dfplot$perc_err_pooled_mle^2))
sqrt(mean(dfplot$perc_err_indiv_mle^2))
sqrt(mean(dfplot$perc_err_eb_posterior_mean^2))
sqrt(mean(dfplot$perc_err_eb_posterior_median^2))

# *****************************************************
# 6. Save results -------------------------------------

# Save result as RDS file.
write_rds(df2018_eb, "Results/df2018_eb.rds")
