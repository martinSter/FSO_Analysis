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

# Create output directory if it does not exist yet.
dir.create("Results", showWarnings = FALSE)

# Load data from previous step.
df2018 <- read_rds("Results/df2018.rds")


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
# 3. Duration -----------------------------------------

# Cond. probability p(Duration | Term. or no term.)
df_type <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                      sheet = "T3", 
                      range = "A11:F19", 
                      col_names = c("Type", "Gender", "Total", "LVA_Total", "LVA_Contract", "LVA")) |> 
  # Only keep the rows for totals (no gender partition necessary here)
  filter(!is.na(Type)) |> 
  # Reduce to relevant columns
  select(Type, Total, LVA) |> 
  # Compute the number of no premature terminations
  mutate(no_LVA = Total - LVA) |> 
  # Remove total
  select(-Total) |> 
  # Compute conditional probabilities so the prob. sum up to 1 for LVA and no LVA
  # Includes Laplace smoothing
  mutate(
    LVA = (LVA + 1) / (sum(LVA) + 3),
    no_LVA = (no_LVA + 1) / (sum(no_LVA) + 3)
  )


# *****************************************************
# 4. Gender -------------------------------------------

# Cond. probability p(Gender | Term. or no term.)
df_gender <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                        sheet = "T3", 
                        range = "B9:F10", 
                        col_names = c("Gender", "Total", "LVA_Total", "LVA_Contract", "LVA")) |> 
  # Compute the number of no premature terminations
  mutate(no_LVA = Total - LVA) |> 
  # Compute conditional probabilities so the prob. sum up to 1 for LVA and no LVA
  # Includes Laplace smoothing
  mutate(
    LVA = (LVA + 1) / (sum(LVA) + 2),
    no_LVA = (no_LVA + 1) / (sum(no_LVA) + 2)
  ) |> 
  select(Gender, LVA, no_LVA)


# *****************************************************
# 5. Migration background -----------------------------

# Cond. probability p(Nationality | Term. or no term.)
df_nationality <- read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
                             sheet = "T5", 
                             range = "B9:F12",
                             col_names = c("Nationality", "Total", "LVA_Total", "LVA_Contract", "LVA")) |> 
  # Compute the number of no premature terminations
  mutate(no_LVA = Total - LVA) |> 
  # IMPORTANT: we remove the category "Unbekannt" cause the counts are low and
  # if feature value is unknown it can actually be marginalized out in NB
  filter(Nationality != "Unbekannt") |> 
  # Compute conditional probabilities so the prob. sum up to 1 for LVA and no LVA
  # Includes Laplace smoothing
  mutate(
    LVA = (LVA + 1) / (sum(LVA) + 3),
    no_LVA = (no_LVA + 1) / (sum(no_LVA) + 3)
  ) |> 
  select(Nationality, LVA, no_LVA)


# *****************************************************
# 6. Occupational field -------------------------------

# Cond. probability p(Profession | Term. or no term.)
# We first concatenate the infos from three sheets so that we have the info about
# which jobs are EBA, EFZ-3, and EFZ-4.
df_profession <- bind_rows(
  read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
             sheet = "T8b", 
             range = "A10:B85", 
             col_names = c("Group", "Job")) |> 
    filter(is.na(Group)) |> 
    mutate(Type = "Eintrittskohorte EBA (2 Jahre)"),
  read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
             sheet = "T8c", 
             range = "A10:B143", 
             col_names = c("Group", "Job")) |> 
    filter(is.na(Group)) |> 
    mutate(Type = "Eintrittskohorte EFZ 3 Jahre"),
  read_excel("Data/su-d-15.10.03-06-Kohorte2018.xlsx", 
             sheet = "T8d", 
             range = "A10:B98", 
             col_names = c("Group", "Job")) |> 
    filter(is.na(Group)) |> 
    mutate(Type = "Eintrittskohorte EFZ 4 Jahre")) |> 
  # We remove column "Group
  select(-Group) |> 
  # Now we join the numbers from "df" to get the proper (and already imputed) counts
  left_join(df2018 |> select(-Groups), by = c("Job" = "Jobs")) |> 
  # Compute the number of no premature terminations
  mutate(no_LVA = Total - LVA) |>
  # Remove total
  select(Job, Type, LVA, no_LVA)

# Correct two mistakes in FSO data.
# Anlagenführer/in and Automatikmonteur/in are both EFZ-3.
df_profession$Type[df_profession$Job == "Anlagenführer/in EFZ"] <- "Eintrittskohorte EFZ 3 Jahre"
df_profession$Type[df_profession$Job == "Automatikmonteur/in EFZ"] <- "Eintrittskohorte EFZ 3 Jahre"

# Now we would like to have a row for all Job-Type combinations (even impossible ones)
df_profession <- df_profession |> 
  # For this we do a full joing with the cartesian product of jobs and types
  full_join(
    expand.grid(df_profession$Job, unique(df_profession$Type)) |> rename(Job = Var1, Type = Var2),
    by = c("Job", "Type")
  )

# Now, we need to join the proper sums for normalization.
df_profession <- df_profession |> 
  left_join(
    # This computes sums over counts by type
    df_profession |> summarize(
      temp1 = sum(LVA, na.rm = T), 
      temp11 = sum(!is.na(LVA)),
      temp2 = sum(no_LVA, na.rm = T), 
      temp21 = sum(!is.na(no_LVA)),
      .by = Type),
    by = "Type"
  ) |> 
  # Compute conditional probabilities so the prob. sum up to 1 for LVA and no LVA
  # Includes Laplace smoothing
  mutate(
    LVA = (LVA + 1) / (temp1 + temp11),
    no_LVA = (no_LVA + 1) / (temp2 + temp21)
  ) |> 
  # Remove temporary variables
  select(-(temp1:temp21))

# Check: probabilities need to sum up to 1 by type
sum(df_profession$LVA[df_profession$Type == "Eintrittskohorte EBA (2 Jahre)"], na.rm = T)
sum(df_profession$LVA[df_profession$Type == "Eintrittskohorte EFZ 3 Jahre"], na.rm = T)
sum(df_profession$LVA[df_profession$Type == "Eintrittskohorte EFZ 4 Jahre"], na.rm = T)


# *****************************************************
# 7. Put it all together ------------------------------

# Importantly, we still consider the duration information
# for the profession variable so that the impossible
# combinations of profession and duration result in NA.

# Here we now compute the NB predictions for all possible feature combinations.
out_TNB <- df_type |> 
  # We always make full joins to get all possible combinations
  merge(df_gender, by = NULL, suffixes = c(".type", ".gender")) |> 
  merge(df_nationality, by = NULL) |> 
  merge(df_profession, by = "Type", suffixes = c(".nationality", ".profession")) |> 
  # Order the columns differently
  select(Type, Gender, Nationality, Job, LVA.type, no_LVA.type, LVA.gender, no_LVA.gender,
         LVA.nationality, no_LVA.nationality, LVA.profession, no_LVA.profession) |> 
  # Compute likelihoods
  mutate(
    temp1 = LVA.type * LVA.gender * LVA.nationality * LVA.profession,
    temp2 = no_LVA.type * no_LVA.gender * no_LVA.nationality * no_LVA.profession,
    temp3 = LVA.type * LVA.gender * LVA.profession,
    temp4 = no_LVA.type * no_LVA.gender * no_LVA.profession
  ) |> 
  # Apply Bayes Theorem
  mutate(
    pred_LVA = (prior_LVA * temp1) / ((prior_LVA * temp1) + ((1 - prior_LVA) * temp2)),
    # Predictions in case of missing nationality information.
    pred_LVA_missingNat = (prior_LVA * temp3) / ((prior_LVA * temp3) + ((1 - prior_LVA) * temp4)),
    marg_lik = (prior_LVA * temp1) + ((1 - prior_LVA) * temp2)
  ) |> 
  # Remove temporary variables
  select(-temp1, -temp2, -temp3, -temp4)

# English labels
out_TNB$Type[out_TNB$Type == "Eintrittskohorte EBA (2 Jahre)"] <- "EBA"
out_TNB$Type[out_TNB$Type == "Eintrittskohorte EFZ 3 Jahre"] <- "EFZ-3"
out_TNB$Type[out_TNB$Type == "Eintrittskohorte EFZ 4 Jahre"] <- "EFZ-4"

out_TNB$Gender[out_TNB$Gender == "Männer"] <- "Male"
out_TNB$Gender[out_TNB$Gender == "Frauen"] <- "Female"

out_TNB$Nationality[out_TNB$Nationality == "Lernende schweizerischer Staatsangehörigkeit"] <- "Swiss"
out_TNB$Nationality[out_TNB$Nationality == "Lernende ausländischer Staatsangehörigkeit, in CH geboren"] <- "Foreign, born in CH"
out_TNB$Nationality[out_TNB$Nationality == "Lernende ausländischer Staatsangehörigkeit, im Ausland geboren"] <- "Foreign, born abroad"


# *****************************************************
# 8. Save results -------------------------------------

# Save result as RDS file.
write_rds(out_TNB, "Results/out_TNB.rds")

