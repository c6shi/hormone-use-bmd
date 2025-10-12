# EDA
library(here)
library(dplyr)

baseline <- here("data", "ICPSR_28762", "DS0001", "28762-0001-Data.rda")
load(baseline)

# Obtain participants with BMD data
baseline_df <- da28762.0001 %>% 
  filter_at(vars(SPBMDT0, HPBMDT0), all_vars(!is.na(.)))

swan_bone_cohort <- da28762.0001 %>% 
  filter_at(vars(SPBMDT0, HPBMDT0), any_vars(!is.na(.)))

swan_bone_cohort2 <- da28762.0001 %>%
  filter(as.integer(RACE) != 5)

# Check all visits
visit1 <- here("data", "ICPSR_29221", "DS0001", "29221-0001-Data.rda")
load(visit1)
visit1_df <- da29221.0001 %>%
  filter_at(vars(SPBMDT1, HPBMDT1), all_vars(!is.na(.)))
