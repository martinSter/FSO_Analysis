# *****************************************************
# -------------------- VET Study ----------------------
#
# Fachhochschule Nordwestschweiz
# Riggenbachstrasse 16
# 4600 Olten
#
# Author: Martin Sterchi
# Date: 21.08.2026
# Description: Naive Bayes
#
# *****************************************************
# 1. Prepare setup ------------------------------------

# Clean up workspace
rm(list = ls())

# Load libraries
library(tidyverse)

# Load data from previous step (the tree-augmented NB).
out <- read_rds("Results/out.rds")


# *****************************************************
# 2. Prior computation --------------------------------

# Sufficient statistics (based on FSO data).
# n1: number of premature terminations in 2018 cohort
# n0: number of successful completions in 2018 cohort
# The numbers here are slightly different than when
# you sum the columns in 'df2018' because of the 
# imputation (due to small rounding errors).
n1 <- 13039
n0 <- 40311

# This is the prior probability of prem. term. with
# Laplace smoothing (which is not really necessary here).
prior_LVA <- (n1 + 1) / (n0 + n1 + 2)


# *****************************************************
# 3. Prepare data for plot ----------------------------

# Choose observation index here (the three obs. I used
# so far are given already).
# i <- 2079 # Results in lowest posterior prob.
i <- 1914 # Results in highest posterior prob.

# Prepare the data for explainable pred. plot.
dfPlot <- out[i, ] |> 
  # Get only the conditional probabilities.
  select(LVA.type:no_LVA.profession) |> 
  # Convert to long format.
  pivot_longer(
    cols = everything(),
    names_to = c("target", "variable"),
    names_pattern = "(.*)\\.(.*)",
    values_to = "prob"
  ) |> 
  # Join the actual feature values.
  left_join(
    out[i, ] |> 
      select(Type:Job) |> 
      rename("type" = "Type", "gender" = "Gender", "nationality" = "Nationality", "profession" = "Job") |> 
      pivot_longer(everything(), names_to = "variable", values_to = "value"),
    by = "variable"
  ) |> 
  # Spread by LVA vs. no LVA
  pivot_wider(names_from = target, values_from = prob) |> 
  # For each feature, compute log of ratio of LVA prob. vs. no LVA prob.
  mutate(LogRatio = log(LVA / no_LVA))

# More prep. for visualization.
dfPlot <- dfPlot |> 
  # Add a row with total, i.e., the sum of Log-Ratios.
  rbind(list("sum", NA, NA, NA, sum(dfPlot$LogRatio))) |> 
  # Add a new column that adds the variable information.
  mutate(variable = factor(variable, levels = c("profession", "nationality", "gender", "type", "sum"))) |> 
  # Add a new column that later helps with coloring the bars.
  mutate(highlight = ifelse(variable == "sum", "0", ifelse(LogRatio < 0, "1", "2")))

# Shorten some job titles.
dfPlot$value[dfPlot$value == "Fachmann/-frau Information und Dokumentation EFZ"] <- "Fachmann/-frau Inf. und Dok. EFZ"


# *****************************************************
# 4. Plot ---------------------------------------------

# Function to go from log-odds back to prob.
log_odds_prob <- function(x) exp(x) / (1 + exp(x))

# Compute log-odds for the prior.
log_odds_prior <- log(prior_LVA / (1 - prior_LVA))

# Create the plot.
dfPlot |> 
  ggplot() +
  geom_hline(yintercept = log_odds_prior, colour = "grey30", linewidth = 0.3) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3, linetype = "dotted") +
  geom_hline(yintercept = dfPlot$LogRatio[dfPlot$variable == "sum"] + log_odds_prior, colour = "#DC267F", linewidth = 0.3, linetype = "dotted") +
  geom_segment(aes(x = variable, xend = variable, y = log_odds_prior, yend = log_odds_prior + LogRatio, color = highlight), linewidth = 18) +
  geom_text(aes(x = variable, y = log_odds_prior, label = value), hjust = 0, nudge_x = -0.05, nudge_y = 0.05, angle = 90, size = 2.5, colour = "grey30", na.rm = TRUE) +
  scale_color_manual(values = c("0" = "grey", "1" = "#1A85FF", "2" = "#DC267F"), guide = "none") +
  scale_x_discrete(name = NULL, labels = c(
    expression(paste(x[O]," | ",x[D])), 
    expression(paste(x[M]," | ",x[D])),
    expression(paste(x[G]," | ",x[D])),
    expression(x[D]),
    "Sum")
  ) +
  scale_y_continuous(
    name = "log-odds components",
    limits = c(-3, 1.5), 
    breaks = seq(-3, 1, 1),
    expand = c(0, 0),
    sec.axis = sec_axis(
      transform = ~exp(.) / (1 + exp(.)), 
      name = "Posterior probability",
      breaks = c(seq(0.05, 0.95, 0.1), log_odds_prob(dfPlot$LogRatio[dfPlot$variable == "sum"] + log_odds_prior)),
      labels = c(seq(0.05, 0.95, 0.1), round(log_odds_prob(dfPlot$LogRatio[dfPlot$variable == "sum"] + log_odds_prior), 2))
    )
  ) +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 0.4),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.line.y = element_line(colour = "grey30", linewidth = 0.4),
    axis.line.y.right = element_line(colour = "#DC267F", linewidth = 0.4),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    axis.ticks.y = element_line(colour = "grey30", linewidth = 0.3),
    axis.ticks.y.right = element_line(colour = "#DC267F", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "grey30"),
    axis.text.y =  element_text(size = 8, colour = "grey30"),
    axis.text.y.right =  element_text(size = 8, colour = "#DC267F"),
    axis.title.y =  element_text(size = 9, colour = "grey30"),
    axis.title.y.right = element_text(color = "#DC267F", size = 9)
  )

# Export
# ggsave("Results/plotLogOdds.pdf", width = 10,  height = 9, units = "cm")

