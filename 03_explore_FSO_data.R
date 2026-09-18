# *****************************************************
# -------------------- VET Study ----------------------
#
# Fachhochschule Nordwestschweiz
# Riggenbachstrasse 16
# 4600 Olten
#
# Author: Martin Sterchi
# Date: 21.08.2026
# Description: Exploration and plots of FSO data
#
# *****************************************************
# 1. Prepare setup ------------------------------------

# Clean up workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(readxl)
library(zoo)
library(grid)

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
ggsave("Results/dur_marginal.pdf", plot = p_dur_freq, width = 7,  height = 3,  units = "cm")

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
ggsave("Results/dur_odds.pdf", plot = p_dur_odds, width = 7,  height = 5,  units = "cm")


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
ggsave("Results/gen_marginal.pdf", plot = p_gen_freq, width = 7,  height = 3,  units = "cm")

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
ggsave("Results/gen_odds.pdf", plot = p_gen_odds, width = 7,  height = 5,  units = "cm")


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
ggsave("Results/mig_marginal.pdf", plot = p_mig_freq, width = 7, height = 3,  units = "cm")

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
ggsave("Results/mig_odds.pdf", plot = p_mig_odds, width = 7, height = 5,  units = "cm")


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
ggsave("Results/number_term.pdf", plot = p_nt, width = 7,  height = 4,  units = "cm")


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
ggsave("Results/term_time.pdf", plot = p_time, width = 8,  height = 5,  units = "cm")


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
ggsave("Results/reentry.pdf", plot = p_reentry, width = 8,  height = 5,  units = "cm")


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
ggsave("Results/reentry_next.pdf", plot = p_reentry, width = 10,  height = 5,  units = "cm")

