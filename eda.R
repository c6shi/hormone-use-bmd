# EDA
library(here)
library(dplyr)
library(tidyverse)
library(data.table)

# Load all data
all_icpsr <- list.dirs(here("data"), full.names=F, recursive=F)

for (icpsr in all_icpsr) {
  rda <- here("data", icpsr, "DS0001", sprintf("%s-0001-Data.rda", substr(icpsr, 7, 12)))
  load(rda)
}

# Relevant variables
screener_W <- c("RACE", "DEGREE")
visit_W <- c("AGE", "BMI", "OSTEOAR", "OSTEOPR")
# age, bmi, race, education, pre-existing: current/past tobacco use, osteoarthritis, osteoporosis,
# thryoid disease, depressive symptoms (median, IQR), alcohol use, medication use, menopausal status
screener_A <- c()


# Obtain participants with BMD data
baseline_df <- da28762.0001 %>% 
  filter_at(vars(SPBMDT0, HPBMDT0), all_vars(!is.na(.)))

visit1_df <- da29221.0001 %>%
  filter_at(vars(SPBMDT1, HPBMDT1), all_vars(!is.na(.)))

visit2_df <- da29401.0001 %>%
  filter_at(vars(SPBMDT2, HPBMDT2), all_vars(!is.na(.)))

visit3_df <- da29701.0001 %>%
  filter_at(vars(SPBMDT3, HPBMDT3), all_vars(!is.na(.)))

visit4_df <- da30142.0001 %>%
  filter_at(vars(SPBMDT4, HPBMDT4), all_vars(!is.na(.)))

visit5_df <- da30501.0001 %>%
  filter_at(vars(SPBMDT5, HPBMDT5), all_vars(!is.na(.)))

visit6_df <- da31181.0001 %>%
  filter_at(vars(SPBMDT6, HPBMDT6), all_vars(!is.na(.)))

visit7_df <- da31901.0001 %>%
  filter_at(vars(SPBMDT7, HPBMDT7), all_vars(!is.na(.)))

visit8_df <- da32122.0001 %>%
  filter_at(vars(SPBMDT8, HPBMDT8), all_vars(!is.na(.)))

visit9_df <- da32721.0001 %>%
  filter_at(vars(SPBMDT9, HPBMDT9), all_vars(!is.na(.)))

visit10_df <- da32961.0001 %>%
  filter_at(vars(SPBMDT10, HPBMDT10), all_vars(!is.na(.)))

setDT(baseline_df)
setDT(visit1_df)
setDT(visit2_df)
all_visits <- merge(visit1_df[, r1 := .I], visit2_df[, r2 := .I], by="SWANID", all=T)
all_visits <- all_visits[, merge_ := fcase(
  is.na(r1), "2",
  is.na(r2), "1",
  default = "both"
)]

all_visits <- baseline_df %>%
  inner_join(visit1_df, by = "SWANID") %>%
  inner_join(visit2_df, by = "SWANID") %>%
  inner_join(visit3_df, by = "SWANID") %>%
  inner_join(visit4_df, by = "SWANID") %>%
  inner_join(visit5_df, by = "SWANID") %>%
  inner_join(visit6_df, by = "SWANID") %>%
  inner_join(visit7_df, by = "SWANID") %>%
  inner_join(visit8_df, by = "SWANID") %>%
  inner_join(visit9_df, by = "SWANID") %>%
  inner_join(visit10_df, by = "SWANID")
  