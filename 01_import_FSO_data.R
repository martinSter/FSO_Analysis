# *****************************************************
# -------------------- VET 2025 -----------------------
#
# Fachhochschule Nordwestschweiz
# Riggenbachstrasse 16
# 4600 Olten
#
# Author: Martin Sterchi
# Date: 12.03.2025
# Description: Prepare FSO data
#
# *****************************************************
# 1. Prepare setup ------------------------------------

# Clean up workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(tidymodels)
library(infotheo)
library(readxl)
library(zoo)

# Create output directory if it does not exist yet.
dir.create("Results", showWarnings = FALSE)

# *****************************************************
# 2. Import and preprocess 2018 cohort ----------------

# Import the data from Excel file (2018 cohort)
df2018 <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", sheet = "T8a", range = "A8:D272")

# Change column names
colnames(df2018) <- c("Groups", "Jobs", "Total", "LVA")

# Remove first data row (it's the overall total)
df2018 <- df2018[-1, ]

# Replace asterisks by NA
df2018[df2018 == "*"] <- NA

# Change data types
df2018$LVA <- as.numeric(df2018$LVA)

# Get only the groups in a separate dataframe
df_groups <- df2018[!is.na(df2018$Groups), ]

# For one category there is still a missing value, replace it by 0
df_groups[is.na(df_groups)] <- 0

# Fill missing values with the preceding value (locf = last observation carried forward).
# This is so that all professions in the same group have the group identifier.
df2018$Groups <- na.locf(df2018$Groups, na.rm = FALSE)

# Remove the group rows from df
df2018 <- df2018[!(df2018$Jobs %in% df_groups$Jobs), ]

# *****************************************************
# 3. Imputation of "*" cells --------------------------

# Join the LVA sums from df to df_groups and compute the difference.
# Note: we need to know how many more LVA need to be imputed.
df_groups <- df_groups |> 
  left_join(
    df2018 |> summarise(LVA_agg = sum(LVA, na.rm = T), .by = "Groups"),
    by = "Groups"
  ) |> 
  mutate(LVA_diff = LVA - LVA_agg)

# Join the total number of apprenticeships for jobs with NA per group.
# Note: this is needed to compute the appropriate proportion for imputation.
df_groups <- df_groups |> 
  left_join(
    df2018 |> summarise(Total_missing = sum(Total[is.na(LVA)]), .by = Groups),
    by = "Groups"
  )

# Impute values based on totals.
# Formula: (Total Apprenticeships in Profession / Total Apprenticeships with missing info in group) 
#           * Total LVA that need to be imputed
df2018 <- df2018 |> 
  left_join(
    df_groups |> select(Groups, Total_missing, LVA_diff), 
    by = "Groups"
  ) |> 
  mutate(LVA_new = ifelse(is.na(LVA), round((Total / Total_missing) * LVA_diff), LVA)) |> 
  select(Groups, Jobs, Total, LVA_new) |> 
  rename(LVA = LVA_new)

# Plausi check:
# This should be more or less equal to total number of LVA.
# Only more or less cause we round numbers.
df2018 |> summarise(sum(LVA), .by = Groups)

# *****************************************************
# 4. Import and preprocess 2019 cohort ----------------

# Import the data from Excel file
df2019 <- read_excel("Data/su-d-15.10.03-06-Kohorte2019.xlsx", sheet = "T8a", range = "A8:D268")

# Change column names
colnames(df2019) <- c("Groups", "Jobs", "Total", "LVA")

# Remove first data row (it's the overall total)
df2019 <- df2019[-1, ]

# Replace asterisks by NA
df2019[df2019 == "*"] <- NA

# Change data types
df2019$LVA <- as.numeric(df2019$LVA)

# Remove the group rows from df
df2019 <- df2019[is.na(df2019$Groups), ] |> 
  select(-Groups)

# Plausi check: is the sum over totals equal to grand total according to Excel file?
sum(df2019$Total) == 52937

# At least two jobs need to be adjusted because names changed.
# Matches: 2018 cohort --> 2019 cohort
# Drucktechnologe/-technologin EFZ --> Medientechnologe/-technologin EFZ
# Gewebegestalter/in EFZ --> Korb- und Flechtwerkgestalter/in EFZ
# -> WARNING: I do not believe these are the same, they are two different professions.
# Thus, I do not change the name in the 2019 cohort.
# No entry in the 2018 cohort --> Industriekeramiker/in EFZ
# No entry in the  2018 cohort --> Formenpraktiker/in EBA
# Glasapparatebauer/in --> Apparateglasbläser/in EFZ
# No entry in the  2018 cohort --> Geflügelfachmann/-frau EFZ
# new in the 2019 cohort --> Restaurantfachmann/-frau EFZ (ab 2019)
# new in the 2019 cohort --> Restaurantangestellte/r EBA (ab 2019)

# Change names of jobs in 2019 cohort for matching.
# Only for the ones we are certain that they simply have been renamed.
df2019$Jobs[df2019$Jobs == "Medientechnologe/-technologin EFZ"] <- "Drucktechnologe/-technologin EFZ"
df2019$Jobs[df2019$Jobs == "Apparateglasbläser/in EFZ"] <- "Glasapparatebauer/in"

# *****************************************************
# 5. Save results -------------------------------------

# Save result as RDS file.
write_rds(df2018, "Results/df2018.rds")
write_rds(df2019, "Results/df2019.rds")
