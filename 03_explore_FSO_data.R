# *****************************************************
# -------------------- VET 2025 -----------------------
#
# Fachhochschule Nordwestschweiz
# Riggenbachstrasse 16
# 4600 Olten
#
# Author: Martin Sterchi
# Date: 13.03.2025
# Description: Exploration and plots of FSO data
#
# *****************************************************
# 1. Prepare setup ------------------------------------

# Clean up workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(infotheo)
library(readxl)
library(zoo)
library(grid)
library(cowplot)
library(patchwork)

# Create output directory if it does not exist yet.
dir.create("Results", showWarnings = FALSE)

# Load data from previous step.
df2018 <- read_rds("Results/df2018.rds")

# *****************************************************
# 2. Duration -----------------------------------------

# Cond. probability p(Duration | Term. or no term.)
dur <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                  sheet = "T1", 
                  range = "A11:F19", 
                  col_names = c("Duration", "Form", "Total", "LVA_Total", "LVA_Contract", "LVA_Person")) |> 
  # Fill up missings in Duration.
  mutate(Duration = na.locf(Duration, na.rm = FALSE)) |> 
  # Only keep the rows for "Dual".
  filter(Form == "Dual") |> 
  # Reduce to relevant columns
  select(Duration, Total, LVA_Person) |> 
  # Compute the number of no premature terminations
  mutate(no_LVA_Person = Total - LVA_Person) |> 
  # Remove total
  select(-Total) |> 
  # Wide-to-long
  pivot_longer(!Duration, names_to = "LVA", values_to = "Counts") |> 
  # Rename values in columns
  mutate(Duration = case_when(
    Duration == "Eintrittskohorte EBA (2 Jahre)" ~ "EBA",
    Duration == "Eintrittskohorte EFZ 3 Jahre" ~ "EFZ-3",
    Duration == "Eintrittskohorte EFZ 4 Jahre" ~ "EFZ-4",
    TRUE ~ Duration
  )) |> 
  mutate(LVA = case_when(
    LVA == "LVA_Person" ~ "y = 1",
    LVA == "no_LVA_Person" ~ "y = 0",
    TRUE ~ LVA
  ))

# Visualization
p_dur_freq <- dur |> 
  summarize(freq = sum(Counts)/sum(dur$Counts), .by = Duration) |> 
  ggplot(mapping = aes(x = reorder(Duration, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#1A85FF", color = "white", linewidth = 0.25, alpha = 0.8, width = 0.95) +
  scale_y_continuous(limits = c(0., 0.7), breaks = seq(0.1, 0.7, 0.2), expand = c(0, 0), labels = scales::percent) +
  theme_bw() +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_blank(),
    axis.title.x =  element_text(size = 8, colour = "black")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Frequency")

p_dur_freq

# Export
ggsave("Plots/dur_marginal.pdf", plot = p_dur_freq, width = 7,  height = 3,  units = "cm")

# Cond. probability p(Duration | Term. or no term.)
p_dur_odds <- dur |> 
  pivot_wider(names_from = "LVA", values_from = Counts) |> 
  mutate(
    `y = 1` = `y = 1` / sum(`y = 1`),
    `y = 0` = `y = 0` / sum(`y = 0`)
  ) |> 
  mutate(
    odds = `y = 1` / `y = 0`,
    odds1 = odds - 1,
    color = ifelse(odds1 < 0, "neg", "pos")
  ) |> 
  ggplot(mapping = aes(x = reorder(Duration, odds1), y = odds1, fill = color)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.4) +
  geom_bar(stat = "identity", linewidth = 0.25, alpha = 0.8, width = 0.95) +
  scale_y_continuous(limits = c(-.2, .8), breaks = seq(-.2, .8, 0.2), expand = c(0, 0), labels = seq(-.2, .8, 0.2) + 1) +
  scale_fill_manual(NULL, values = c("#1A85FF", "#DC267F")) +
  theme_bw() +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_blank(),
    axis.title.x =  element_text(size = 8, colour = "black")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Posterior-prior odds ratio") +
  guides(fill = "none")
  
p_dur_odds

# Export
ggsave("Plots/dur_odds.pdf", plot = p_dur_odds, width = 7,  height = 5,  units = "cm")









# *****************************************************
# 3. Gender -------------------------------------------

# Cond. probability p(Duration | Term. or no term.)
gen <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                  sheet = "T3", 
                  range = "A11:F19", 
                  col_names = c("Duration", "Gender", "Total", "LVA_Total", "LVA_Contract", "LVA_Person")) |> 
  # Fill up missings in Duration.
  mutate(Duration = na.locf(Duration, na.rm = FALSE)) |> 
  # Only keep the rows for "Dual".
  filter(Gender != "Total") |> 
  # Reduce to relevant columns
  select(Duration, Gender, Total, LVA_Person) |> 
  # Compute the number of no premature terminations
  mutate(no_LVA_Person = Total - LVA_Person) |> 
  # Remove total
  select(-Total) |> 
  # Wide-to-long
  pivot_longer(!Duration:Gender, names_to = "LVA", values_to = "Counts") |> 
  # Rename values in columns
  mutate(Duration = case_when(
    Duration == "Eintrittskohorte EBA (2 Jahre)" ~ "EBA",
    Duration == "Eintrittskohorte EFZ 3 Jahre" ~ "EFZ-3",
    Duration == "Eintrittskohorte EFZ 4 Jahre" ~ "EFZ-4",
    TRUE ~ Duration
  )) |> 
  mutate(LVA = case_when(
    LVA == "LVA_Person" ~ "y = 1",
    LVA == "no_LVA_Person" ~ "y = 0",
    TRUE ~ LVA
  )) |> 
  mutate(Gender = case_when(
    Gender == "Männer" ~ "Male",
    Gender == "Frauen" ~ "Female",
    TRUE ~ Gender
  ))

# Visualization
p_gen_freq <- gen |> 
  summarize(freq = sum(Counts)/sum(dur$Counts), .by = Gender) |> 
  ggplot(mapping = aes(x = Gender, y = freq)) +
  geom_bar(stat = "identity", fill = "#1A85FF", color = "white", linewidth = 0.25, alpha = 0.8, width = 0.95) +
  scale_y_continuous(limits = c(0., 0.6), breaks = seq(0.2, 0.6, 0.2), expand = c(0, 0), labels = scales::percent) +
  theme_bw() +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_blank(),
    axis.title.x =  element_text(size = 8, colour = "black")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Frequency")

p_gen_freq

# Export
ggsave("Plots/gen_marginal.pdf", plot = p_gen_freq, width = 7,  height = 3,  units = "cm")


# Cond. probability p(Duration | Term. or no term.)
p_gen_odds <- gen |> 
  pivot_wider(names_from = "LVA", values_from = Counts) |> 
  mutate(
    `y = 1` = `y = 1` / sum(`y = 1`),
    `y = 0` = `y = 0` / sum(`y = 0`)
  ) |> 
  mutate(
    odds = `y = 1` / `y = 0`,
    odds1 = odds - 1,
    color = ifelse(odds1 < 0, "neg", "pos"),
    Var = paste(Gender, Duration, sep = ", ")
  ) |> 
  ggplot(mapping = aes(x = reorder(Var, odds1), y = odds1, fill = color)) +
  geom_bar(stat = "identity", linewidth = 0.25, alpha = 0.8, width = 0.95) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.4) +
  scale_y_continuous(limits = c(-.2, .8), breaks = seq(-.2, .8, 0.2), expand = c(0, 0), labels = seq(-.2, .8, 0.2) + 1) +
  scale_fill_manual(NULL, values = c("#1A85FF", "#DC267F")) +
  theme_bw() +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_blank(),
    axis.title.x =  element_text(size = 8, colour = "black")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Posterior-prior odds ratio") +
  guides(fill = "none")

p_gen_odds

# Export
ggsave("Plots/gen_odds.pdf", plot = p_gen_odds, width = 7,  height = 5,  units = "cm")


# *****************************************************
# 4. Migration background -----------------------------

# Cond. probability p(Duration | Term. or no term.)
mig <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                  sheet = "T5", 
                  range = "A13:F27", 
                  col_names = c("Duration", "Migration", "Total", "LVA_Total", "LVA_Contract", "LVA_Person")) |> 
  # Fill up missings in Duration.
  mutate(Duration = na.locf(Duration, na.rm = FALSE)) |> 
  # Only keep the rows for "Dual".
  filter(Migration != "Total") |> 
  # Reduce to relevant columns
  select(Duration, Migration, Total, LVA_Person) |> 
  # Compute the number of no premature terminations
  mutate(no_LVA_Person = Total - LVA_Person) |> 
  # Remove total
  select(-Total) |> 
  # Wide-to-long
  pivot_longer(!Duration:Migration, names_to = "LVA", values_to = "Counts") |> 
  # Rename values in columns
  mutate(Duration = case_when(
    Duration == "Eintrittskohorte EBA (2 Jahre)" ~ "EBA",
    Duration == "Eintrittskohorte EFZ 3 Jahre" ~ "EFZ-3",
    Duration == "Eintrittskohorte EFZ 4 Jahre" ~ "EFZ-4",
    TRUE ~ Duration
  )) |> 
  mutate(LVA = case_when(
    LVA == "LVA_Person" ~ "y = 1",
    LVA == "no_LVA_Person" ~ "y = 0",
    TRUE ~ LVA
  )) |> 
  mutate(Migration = case_when(
    Migration == "Lernende schweizerischer Staatsangehörigkeit" ~ "Swiss",
    Migration == "Lernende ausländischer Staatsangehörigkeit, in CH geboren" ~ "Foreign, born in CH",
    Migration == "Lernende ausländischer Staatsangehörigkeit, im Ausland geboren" ~ "Foreign, born abroad",
    Migration == "Unbekannt" ~ "Unknown",
    TRUE ~ Migration
  ))

# Visualization
p_mig_freq <- mig |> 
  summarize(freq = sum(Counts)/sum(dur$Counts), .by = Migration) |> 
  ggplot(mapping = aes(x = reorder(Migration, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#1A85FF", color = "white", linewidth = 0.25, alpha = 0.8, width = 0.95) +
  scale_y_continuous(limits = c(0., 0.8), breaks = seq(0.2, 0.8, 0.2), expand = c(0, 0), labels = scales::percent) +
  theme_bw() +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_blank(),
    axis.title.x =  element_text(size = 8, colour = "black")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Frequency")

p_mig_freq

# Export
ggsave("Plots/mig_marginal.pdf", plot = p_mig_freq, width = 7, height = 3,  units = "cm")

# Cond. probability p(Duration | Term. or no term.)
p_mig_odds <- mig |> 
  filter(Migration != "Unknown") |> 
  pivot_wider(names_from = "LVA", values_from = Counts) |> 
  mutate(
    `y = 1` = `y = 1` / sum(`y = 1`),
    `y = 0` = `y = 0` / sum(`y = 0`)
  ) |> 
  mutate(
    odds = `y = 1` / `y = 0`,
    odds1 = odds - 1,
    color = ifelse(odds1 < 0, "neg", "pos"),
    Var = paste(Migration, Duration, sep = ", ")
  ) |> 
  ggplot(mapping = aes(x = reorder(Var, odds1), y = odds1, fill = color)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.4) +
  geom_bar(stat = "identity", linewidth = 0.25, alpha = 0.8, width = 0.95) +
  scale_y_continuous(limits = c(-.2, .8), breaks = seq(-.2, .8, 0.2), expand = c(0, 0), labels = seq(-.2, .8, 0.2) + 1) +
  scale_fill_manual(NULL, values = c("#1A85FF", "#DC267F")) +
  theme_bw() +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_blank(),
    axis.title.x =  element_text(size = 8, colour = "black")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Posterior-prior odds ratio") +
  guides(fill = "none")

p_mig_odds

# Export
ggsave("Plots/mig_odds.pdf", plot = p_mig_odds, width = 7, height = 5,  units = "cm")




# *****************************************************
# 5. Number of term. ----------------------------------

# Cond. probability p(Duration | Term. or no term.)
numterm <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                      sheet = "T2", 
                      range = "A10:D16", 
                      col_names = c("Number", "Total", "Total_Perc", "Total_Dual")) |> 
  # Only keep the rows for "Dual".
  filter(!(Number %in% c("Total", "0"))) |> 
  # Reduce to relevant columns
  select(Number, Total_Dual) |> 
  mutate(Number = case_when(
    Number == "5 und mehr" ~ "5+",
    TRUE ~ Number
  ))

# Visualization
p_nt <- numterm |> 
  mutate(freq = Total_Dual / sum(Total_Dual)) |> 
  ggplot(mapping = aes(x = Number, y = freq)) +
  geom_bar(stat = "identity", fill = "#1A85FF", color = "white", linewidth = 0.25, alpha = 0.8, width = 0.95) +
  scale_y_continuous(limits = c(0., 0.8), breaks = seq(0.2, 0.8, 0.2), expand = c(0, 0), labels = scales::percent) +
  theme_bw() +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_text(size = 8, colour = "black"),
    plot.title =  element_text(size = 8, colour = "black"),
    axis.title.x =  element_text(size = 8, colour = "black"),
    axis.title.y =  element_text(size = 8, colour = "black")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = "# Terminations", y = "Frequency")

p_nt

# Export
ggsave("Plots/number_term.pdf", plot = p_nt, width = 7,  height = 4,  units = "cm")


# *****************************************************
# 6. Time of termination ------------------------------

# Cond. probability p(Duration | Term. or no term.)
time <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                      sheet = "T11", 
                      range = "A21:B24", 
                      col_names = c("Time", "Count")) |> 
  mutate(Duration = "EBA") |> 
  bind_rows(
    read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
               sheet = "T11", 
               range = "A32:B35", 
               col_names = c("Time", "Count")) |> 
      mutate(Duration = "EFZ-3")
  ) |> 
  bind_rows(
    read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
               sheet = "T11", 
               range = "A43:B46", 
               col_names = c("Time", "Count")) |> 
      mutate(Duration = "EFZ-4")
  ) |> 
  mutate(Time = case_when(
    Time == "Probezeit (1.-3. Monat)*" ~ "Month 1-3",
    Time == "1. Lehrjahr (4.-12. Monat)" ~ "Month 4-12",
    Time == "2. Lehrjahr" ~ "Second year",
    Time == "> 2. Lehrjahr" ~ "> Second year",
    TRUE ~ Time
  )) |> 
  mutate(Time = factor(Time, levels = rev(c("Month 1-3", "Month 4-12", "Second year", "> Second year"))))

# Visualization
p_time <- time |> 
  ggplot(mapping = aes(x = Duration, y = Count, fill = Time)) +
  geom_bar(stat = "identity", position = "fill", color = "white", linewidth = 0.25) +
  scale_y_continuous(breaks = seq(0.25, 1, 0.25), expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(NULL, values = c("#648FFF","#785EF0","#DC267F","#FE6100")) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_text(size = 8, colour = "black"),
    axis.title.x =  element_text(size = 8, colour = "black"),
    axis.title.y =  element_text(size = 8, colour = "black"),
    plot.title = element_text(size = 8, color = "black"),
    legend.position = "bottom",
    legend.text =  element_text(size = 7, colour = "black"),
    legend.margin = margin(c(1, 2, 1.5, 1)),
    legend.key.size = unit(0.4, "cm")
  ) +
  # Wir wollen keine Achsentitel.
  labs(title = "Timing of termination", x = NULL, y = "Frequency")

p_time

# Export
ggsave("Plots/term_time.pdf", plot = p_time, width = 8,  height = 5,  units = "cm")




# *****************************************************
# 7. Number of re-entries -----------------------------

# Cond. probability p(Duration | Term. or no term.)
reentry <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                   sheet = "T12", 
                   range = "A23:B29", 
                   col_names = c("Reentry", "Count")) |> 
  mutate(Duration = "EBA") |> 
  bind_rows(
    read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
               sheet = "T12", 
               range = "A36:B42", 
               col_names = c("Reentry", "Count")) |> 
      mutate(Duration = "EFZ-3")
  ) |> 
  bind_rows(
    read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
               sheet = "T12", 
               range = "A49:B55", 
               col_names = c("Reentry", "Count")) |> 
      mutate(Duration = "EFZ-4")
  ) |> 
  filter(!(Reentry %in% c("... davon Anzahl Wiedereinstiege", "Mit Wiedereinstieg"))) |> 
  mutate(Reentry = case_when(
    Reentry == "Ohne Wiedereinstieg" ~ "No re-entry",
    Reentry == "1 Wiedereinstieg" ~ "1",
    Reentry == "2 Wiedereinstiege" ~ "2",
    Reentry == "3 Wiedereinstiege" ~ "3",
    Reentry == ">3 Wiedereinstiege" ~ "4+",
    TRUE ~ Reentry
  )) |> 
  mutate(Reentry = factor(Reentry, levels = rev(c("No re-entry", "1", "2", "3", "4+"))))

# Visualization
p_reentry <- reentry |> 
  ggplot(mapping = aes(x = Duration, y = Count, fill = Reentry)) +
  geom_bar(stat = "identity", position = "fill", color = "white", linewidth = 0.25) +
  scale_y_continuous(breaks = seq(0.25, 1, 0.25), expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(NULL, values = c("#648FFF","#785EF0","#DC267F","#FE6100", "#FFB000")) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_text(size = 8, colour = "black"),
    axis.title.y =  element_text(size = 8, colour = "black"),
    plot.title = element_text(size = 8, color = "black"),
    legend.position = "bottom",
    legend.text =  element_text(size = 7, colour = "black"),
    legend.margin = margin(c(1, 2, 1.5, 1)),
    legend.key.size = unit(0.4, "cm")
  ) +
  # Wir wollen keine Achsentitel.
  labs(title = "Number of re-entries", x = NULL, y = "Frequency")

p_reentry

# Export
ggsave("Plots/reentry.pdf", plot = p_reentry, width = 8,  height = 5,  units = "cm")

# *****************************************************
# 8. Re-entry: what next after first term. ------------

# Cond. probability p(Duration | Term. or no term.)
reentry_next <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                      sheet = "T20", 
                      range = "A10:B17", 
                      col_names = c("Next", "Count")) |>
  mutate(Next = case_when(
    Next == "Neubeginn eines Lehrvertrags: kein Betriebswechsel\r\n(gleicher Lehrberuf, gleiches Ausbildungsfeld, gleiche Standardausbildungsdauer)" ~ "Re-entry: same occ. field, new contract",
    Next == "Neubeginn eines Lehrvertrags: Betriebswechsel\r\n(gleicher Lehrberuf, gleiches Ausbildungsfeld, gleiche Standardausbildungsdauer)" ~ "Re-entry: same occ. field, new training workplace",
    Next == "Änderung des Lehrberufs: gleiche Standardausbildungsdauer, anderes Ausbildungsfeld" ~ "Re-entry: new occ. field in new industry",
    Next == "Änderung des Lehrberufs: gleiche Standardausbildungsdauer, gleiches Ausbildungsfeld" ~ "Re-entry: new occ. field in same industry",
    Next == "Änderung des Lehrberufs: andere Standardausbildungsdauer, gleiches Ausbildungsfeld" ~ "Re-entry: new occ. field in same industry, diff. duration",
    Next == "Änderung des Lehrberufs: andere Standardausbildungsdauer, anderes Ausbildungsfeld" ~ "Re-entry: new occ. field in new industry, diff. duration",
    Next == "Neubeginn eines Lehrvertrags: Betriebswechsel unbekannt\r\n(gleicher Lehrberuf, gleiches Ausbildungsfeld, gleiche Standardausbildungsdauer)" ~ "Re-entry: same occ. field, unknown situation",
    Next == "LVA, kein Wiedereinstieg" ~ "No re-entry",
    TRUE ~ Next
  ))

# Visualization
p_reentry <- reentry_next |> 
  mutate(freq = Count / sum(Count)) |>
  ggplot(mapping = aes(x = reorder(Next, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#1A85FF", color = "white", linewidth = 0.25, alpha = 0.8, width = 0.9) +
  scale_y_continuous(limits = c(0, 0.35), breaks = seq(0.05, 0.35, 0.1), expand = c(0, 0), labels = scales::percent) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "grey30", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "grey30", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_blank(),
    axis.title.x =  element_text(size = 8, colour = "black")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Frequency")

p_reentry

# Export
ggsave("Plots/reentry_next.pdf", plot = p_reentry, width = 10,  height = 5,  units = "cm")

# *****************************************************
# 9. Mutual information FSO data ----------------------

compute_mutual_information <- function(tab) {
  # Convert to probabilities
  tab <- tab / sum(tab)
  # Compute marginal probabilities
  px <- rowSums(tab)
  py <- colSums(tab)
  # min_ent <- min(- sum(px * log2(px)), - sum(py * log2(py)))
  # Initialize MI
  mi <- 0
  # Compute MI using the formula
  for (i in seq_along(px)) {
    for (j in seq_along(py)) {
      if (tab[i, j] > 0) { # Avoid log(0)
        mi <- mi + tab[i, j] * log2(tab[i, j] / (px[i] * py[j]))
      }
    }
  }
  return(mi)
}


# tt <- xtabs(Counts ~ Migration + LVA, data = mig) / 53350
# ttx <- rowSums(tt)
# tty <- colSums(tt)
# 
# (entr_dur <- - sum(ttx * log2(ttx)))
# (entr_dur_y <- - sum(tt * log2(tt)) + sum(tty * log2(tty)))
# entr_dur - entr_dur_y

mi_dur <- compute_mutual_information(xtabs(Counts ~ Duration + LVA, data = dur))
mi_gen <- compute_mutual_information(xtabs(Counts ~ Gender + LVA, data = gen))
mi_mig <- compute_mutual_information(xtabs(Counts ~ Migration + LVA, data = mig))
mi_occ <- compute_mutual_information(xtabs(Counts ~ Job + LVA, data = df_occ))

# tt <- xtabs(Counts ~ Migration + LVA, data = mig) / 53350
# ttx <- rowSums(tt)
# tty <- colSums(tt)

# Cond. MI gender
txz <- xtabs(Counts ~ Gender + Duration, data = gen) / 53350
tyz <- xtabs(Counts ~ LVA + Duration, data = gen) / 53350
txyz <- xtabs(Counts ~ Gender + Duration + LVA, data = gen) / 53350
tz <- colSums(txz)

(h_xz <- - sum(txz * log2(txz)) + sum(tz * log2(tz)))
(h_yz <- - sum(tyz * log2(tyz)) + sum(tz * log2(tz)))
(h_xyz <- - sum(txyz * log2(txyz)) + sum(tz * log2(tz)))
mi_gen_cond <- h_xz + h_yz - h_xyz

# Cond. MI migration background
txz <- xtabs(Counts ~ Migration + Duration, data = mig) / 53350
tyz <- xtabs(Counts ~ LVA + Duration, data = mig) / 53350
txyz <- xtabs(Counts ~ Migration + Duration + LVA, data = mig) / 53350
tz <- colSums(txz)

(h_xz <- - sum(txz * log2(txz)) + sum(tz * log2(tz)))
(h_yz <- - sum(tyz * log2(tyz)) + sum(tz * log2(tz)))
(h_xyz <- - sum(txyz * log2(txyz)) + sum(tz * log2(tz)))
mi_mig_cond <- h_xz + h_yz - h_xyz




# Cond. probability p(Profession | Type, Term. or no term.)
# We first concatenate the infos from three sheets so that we have the info about
# which jobs are EBA, EFZ-3, and EFZ-4.
df_occ <- bind_rows(
  read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
             sheet = "T8b", 
             range = "A10:B85", 
             col_names = c("Group", "Job")) |> 
    filter(is.na(Group)) |> 
    mutate(Duration = "Eintrittskohorte EBA (2 Jahre)"),
  read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
             sheet = "T8c", 
             range = "A10:B143", 
             col_names = c("Group", "Job")) |> 
    filter(is.na(Group)) |> 
    mutate(Duration = "Eintrittskohorte EFZ 3 Jahre"),
  read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
             sheet = "T8d", 
             range = "A10:B98", 
             col_names = c("Group", "Job")) |> 
    filter(is.na(Group)) |> 
    mutate(Duration = "Eintrittskohorte EFZ 4 Jahre")
) |> 
  # We remove column "Group
  select(-Group) |> 
  # Now we join the numbers from "df" to get the proper (and already imputed) counts
  left_join(df2018 |> select(-Groups), by = c("Job" = "Jobs")) |> 
  # Compute the number of no premature terminations
  mutate(no_LVA = Total - LVA) |>
  # Remove total
  select(Job, Duration, LVA, no_LVA)

# Correct two mistakes in FSO data.
# Anlagenführer/in and Automatikmonteur/in are both EFZ-3.
df_occ$Duration[df_occ$Job == "Anlagenführer/in EFZ"] <- "Eintrittskohorte EFZ 3 Jahre"
df_occ$Duration[df_occ$Job == "Automatikmonteur/in EFZ"] <- "Eintrittskohorte EFZ 3 Jahre"

# Now we would like to have a row for every Job-Type combinations (even impossible ones)
df_occ <- df_occ |> 
  # For this we do a full joing with the cartesian product of jobs and types
  full_join(
    expand.grid(df_occ$Job, unique(df_occ$Duration)) |> rename(Job = Var1, Duration = Var2),
    by = c("Job", "Duration")
  )

df_occ <- df_occ |> 
  # Wide-to-long
  pivot_longer(!Job:Duration, names_to = "LVA", values_to = "Counts") |> 
  # Rename values in columns
  mutate(Duration = case_when(
    Duration == "Eintrittskohorte EBA (2 Jahre)" ~ "EBA",
    Duration == "Eintrittskohorte EFZ 3 Jahre" ~ "EFZ-3",
    Duration == "Eintrittskohorte EFZ 4 Jahre" ~ "EFZ-4",
    TRUE ~ Duration
  )) |> 
  mutate(LVA = case_when(
    LVA == "LVA_Person" ~ "y = 1",
    LVA == "no_LVA_Person" ~ "y = 0",
    TRUE ~ LVA
  )) |> 
  replace_na(list(Counts = 0))


# Cond. MI migration background
txz <- xtabs(Counts ~ Job + Duration, data = df_occ) / 53350
tyz <- xtabs(Counts ~ LVA + Duration, data = df_occ) / 53350
txyz <- xtabs(Counts ~ Job + Duration + LVA, data = df_occ) / 53350
tz <- colSums(txz)

(h_xz <- - sum(txz[txz > 0] * log2(txz[txz > 0])) + sum(tz * log2(tz)))
(h_yz <- - sum(tyz * log2(tyz)) + sum(tz * log2(tz)))
(h_xyz <- - sum(txyz[txyz > 0] * log2(txyz[txyz > 0])) + sum(tz * log2(tz)))
mi_occ_cond <- h_xz + h_yz - h_xyz



# Create a dataframe in which we compute the mutual information.
df_mi <- data.frame(
  Var = c(rep("Duration", 2), rep("Gender", 2), rep("Mig. backgr.", 2), rep("Occ. field", 2)),
  Type = rep(c("Unconditional", "Conditional on duration"), 4),
  MI = c(
    mi_dur, NA,
    mi_gen, mi_gen_cond,
    mi_mig, mi_mig_cond,
    mi_occ, mi_occ_cond
  )
)

# Visualization of MI results.
p_mi <- df_mi |> 
  ggplot(mapping = aes(x = Var, y = MI, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", linewidth = 0.25) +
  scale_fill_manual(NULL, values = c("#648FFF","#DC267F")) +
  scale_y_continuous(limits = c(0., 0.03), breaks = seq(0.01, 0.03, 0.01), expand = c(0, 0)) +
  # coord_flip() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.y = element_line(colour = "grey30", linewidth = 0.4),
    axis.line.x = element_blank(),
    axis.ticks.y = element_line(colour = "grey30", linewidth = 0.3),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text.x =  element_text(size = 8, colour = "black"),
    axis.text.y =  element_text(size = 8, colour = "black"),
    axis.title.y =  element_text(size = 8, colour = "black"),
    legend.position = "bottom",
    legend.position.inside = c(0.8, 0.1),
    legend.text =  element_text(size = 7, colour = "black"),
    legend.margin = margin(c(1, 2, 1.5, 1)),
    legend.key.size = unit(0.4, "cm")
  ) +
  # Wir wollen keine Achsentitel.
  labs(x = NULL, y = "Mutual information (with y)")

p_mi

# Export
ggsave("Plots/MI.pdf", plot = p_mi, width = 10,  height = 5,  units = "cm")


# *****************************************************
# 6. Save results -------------------------------------

# Save result as RDS file.
write_rds(df2018_eb, "Results/df2018_eb.rds")
