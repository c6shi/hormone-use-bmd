# ETL
library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(caret)
library(zoo)


##### LOAD DATA #####
all_icpsr <- list.dirs(here("data"), full.names=F, recursive=F)

for (icpsr in all_icpsr) {
  rda <- here("data", icpsr, "DS0001", sprintf("%s-0001-Data.rda", substr(icpsr, 7, 12)))
  load(rda)
}


##### DEFINE VARIABLES #####

# EXPOSURE
baseline_A <- c("HORMPIL0")
A_prefix <- c("COMBIN1", "ESTROG1", "PROGES1", "ESTRNJ1")

# OUTCOME
Y_prefix <- c("SPBMDT", "HPBMDT")

# BASELINE COVARIATES
depress_prefix <- c("BOTHER", "APPETIT", "BLUES", "GOOD",
                    "KEEPMIN", "DEPRESS", "EFFORT", "HOPEFUL",
                    "FAILURE", "FEARFUL", "RESTLES", "HAPPY",
                    "TALKLES", "LONELY", "UNFRNDL", "ENJOY",
                    "CRYING", "SAD", "DISLIKE", "GETGOIN")
anxiety_prefix <- c("IRRITAB", "NRVOUS", "HARTRAC", "FEARFULA")

baseline_W0_inscreener <- c("DIABETE",
                            "PHY_ACT",
                            "MARITALGP",
                            "DEGREE")

baseline_W0_inbaseline <- c("RACE", 
                            "AGE0", 
                            "HEIGHT0", 
                            "WEIGHT0", 
                            "STATUS0", 
                            "INSULIN0",
                            "SMOKERE0",
                            "ALCHSRV0",
                            sapply(depress_prefix, function(x) paste0(x, 0)),
                            sapply(anxiety_prefix, function(x) paste0(x, 0)))
baseline_W0_inbaseline <- stack(baseline_W0_inbaseline)$values

# TIME-VARYING COVARIATES
visit_W <- c("HEIGHT",
             "WEIGHT",
             "STATUS", 
             "DIABETE",
             "INSULN1",
             "SMOKERE",
             "DRNKBEE",
             "PHYSACT",
             "MARITAL",
             depress_prefix,
             anxiety_prefix)


##### Construct Cohort from Baseline ##########
# We start with N=3302 participants at baseline.

# Add screener variables to baseline dataframe
screener_df <- da04368.0001[da04368.0001$SWANID %in% da28762.0001$SWANID, 
                            c("SWANID", baseline_W0_inscreener)]

# Obtain participants that are 
# 1) pre-menopausal or early peri-menopausal, and 
# 2) have not taken hormone medications in the last month. (N=3243)
baseline_df <- da28762.0001 %>%
  filter(as.integer(STATUS0) %in% c(4, 5)) %>%
  filter(as.integer(HORMPIL0) == 1)

# Remove women with 
# 1) unknown age at menarche, or 
# 2) age at menarche > 18 years. (N=3217)
baseline_df <- baseline_df %>%
  filter(!is.na(as.integer(STARTAG0) <= 18))

# Remove women with 
# 1) NA hormone usage in visit 1 (cannot assess use of hormones between baseline and visit 1), and 
# 2) missing baseline covariates. (N=2574)
visit1_A <- sapply(A_prefix, function(x) paste0(x, 1))
visit1_hormone_df <- da29221.0001 %>% 
  filter_at(vars(visit1_A), all_vars(!is.na(.)))

baseline_df <- baseline_df[baseline_df$SWANID %in% visit1_hormone_df$SWANID, ]

baseline_df <- baseline_df %>%
  filter_at(vars(baseline_W0_inbaseline), all_vars(!is.na(.)))

# Remove women with 
# 1) NA SPBMDT0 and HPBMDT0 because we assume they were null as two sites did not measure BMD (N=1860)
baseline_df <- baseline_df %>%
  filter_at(vars(SPBMDT0, HPBMDT0), all_vars(!is.na(.)))

##### Feature Engineering #####################

# Change ALCHSRV0 to a 0/1 indicator of alcohol use
baseline_df$ALCHSRV0 <- as.numeric(baseline_df$ALCHSRV0 > 0)

# Create depression and anxiety variables
baseline_depress0 <- stack(sapply(depress_prefix, function(x) paste0(x, 0)))$values
baseline_anxiety0 <- stack(sapply(anxiety_prefix, function(x) paste0(x, 0)))$values

# Convert factor variables to numerics - 1
baseline_Y0 <- stack(sapply(Y_prefix, function(x) paste0(x, 0)))$values
baseline_factor_minus1 <- c(baseline_W0_inbaseline[!baseline_W0_inbaseline %in% c("RACE", "AGE0", "HEIGHT0", "WEIGHT0", "STATUS0", "ALCHSRV0")], "HORMUSER0")
baseline_df <- baseline_df %>%
  rename("HORMUSER0" = "HORMPIL0") %>%
  select(all_of(c("SWANID", baseline_W0_inbaseline, "HORMUSER0", baseline_Y0))) %>%
  mutate(across(all_of(baseline_factor_minus1), as.numeric)) %>%
  mutate(across(all_of(baseline_factor_minus1), ~ . -1)) %>%
  rowwise() %>%
  mutate("DEPRESSION0" = sum(c_across(all_of(baseline_depress0))),
         "ANXIETY0" = sum(c_across(all_of(baseline_anxiety0)))) %>%
  ungroup()

# Remove depression and anxiety questions
baseline_df <- baseline_df %>%
  select(-all_of(c(baseline_depress0, baseline_anxiety0)))

# One hot encode RACE
race_one_hot <- model.matrix(~ RACE-1, baseline_df)[, 1:3]
baseline_df <- baseline_df %>%
  select(-"RACE") %>%
  cbind(race_one_hot)

# Reorder columns for LTMLE (L's, A's, Y's)
baseline_W0 <- names(select(baseline_df, -all_of(c("SWANID", "HORMUSER0", baseline_Y0))))
baseline_W0 <- c(tail(baseline_W0, 3), head(baseline_W0, -3))
baseline_df <- baseline_df %>%
  select(all_of(c("SWANID", baseline_W0, "HORMUSER0", baseline_Y0)))

# Get screener variables for individuals in baseline_df
screener_df <- screener_df %>%
  filter_at(vars(baseline_W0_inscreener), all_vars(!is.na(.))) %>%
  select(c(SWANID, baseline_W0_inscreener)) 
# mutate(across(where(is.factor), as.numeric))

# Merge screener dataframe and filtered baseline dataframe
clean_df <- merge(screener_df, baseline_df, by="SWANID")

# Ordinal encoding and fix DIABETE
ordinal_cols <- c("PHY_ACT", "MARITALGP", "DEGREE", "STATUS0")
clean_df <- clean_df %>%
  mutate(across(all_of(c("DIABETE", ordinal_cols)), as.numeric)) %>%
  mutate(across(all_of(c("DIABETE", ordinal_cols)), ~ . -1)) %>%
  rowwise()

# Rename some baseline L's to match visit L's
clean_df <- clean_df %>%
  rename("AGE" = "AGE0",
         "DRNKBEE0" ="ALCHSRV0",
         "PHYSACT0" = "PHY_ACT", 
         "MARITAL0" = "MARITALGP",
         "DIABETE0" = "DIABETE")

# Final reorder
W <- c("AGE", colnames(race_one_hot), "DEGREE")
L0 <- colnames(clean_df)[!colnames(clean_df) %in% W][-c(1, (ncol(clean_df) - length(W) - 2):(ncol(clean_df) - length(W)))]
clean_df <- clean_df %>%
  select(all_of(c("SWANID", W, L0, "HORMUSER0", baseline_Y0)))


##### Add Each Visit ##########################
for (i in 3:length(all_icpsr)) {
  icpsr <- all_icpsr[i]
  code <- substr(icpsr, 7, 12)
  visit <- i-2
  
  print(c(code, visit))
  
  visit_i_W <- paste(visit_W, visit, sep="")
  visit_i_A <- paste(A_prefix, visit, sep="")
  visit_i_Y <- paste(Y_prefix, visit, sep="")
  visit_i_df <- eval(parse(text=paste(sprintf("da%s.0001", code))))
  visit_i_df <- tryCatch(
    visit_i_df <- visit_i_df %>%
      select(all_of(c("SWANID", visit_i_W, visit_i_A, visit_i_Y))),
    error = function(cond) {
      message(conditionMessage(cond))
      visit_i_df <- visit_i_df %>%
        select(any_of(c("SWANID", visit_i_W, visit_i_A, visit_i_Y)))
      visit_i_missing_W <- setdiff(c(visit_i_W, visit_i_A, visit_i_Y), 
                                   colnames(visit_i_df))
      for (missing_col in visit_i_missing_W) {
        visit_i_df[missing_col] <- NA
      }
      message("Imputed missing columns.")
      return(visit_i_df)
      },
    warning = function(cond) {
      message(conditionMessage(cond))
      NULL
    },
    finally = {
      message("Working...")
    }
  )

  
  # Create depression and anxiety variables
  visit_i_depress <- paste(depress_prefix, visit, sep="")
  visit_i_anxiety <- paste(anxiety_prefix, visit, sep="")
  
  # Convert factor variables to numerics - 1
  visit_W_factor <- visit_W[!visit_W %in% c("HEIGHT", "WEIGHT", "STATUS", "PHYSACT", "MARITAL")]
  visit_i_W_factor_minus1 <- paste(visit_W_factor, visit, sep="")
  visit_i_W_factor_minus1 <- c(visit_i_W_factor_minus1, visit_i_A)
  visit_i_df <- visit_i_df %>%
    mutate(across(all_of(visit_i_W_factor_minus1), as.numeric)) %>%
    mutate(across(all_of(visit_i_W_factor_minus1), ~ . -1)) %>%
    rowwise() %>%
    mutate("DEPRESSION#" = sum(c_across(all_of(visit_i_depress))),
           "ANXIETY#" = sum(c_across(all_of(visit_i_anxiety))),
           "HORMUSER#" = case_when(
             sum(c_across(all_of(visit_i_A))) == 0 ~ 0,
             sum(c_across(all_of(visit_i_A))) > 0 ~ 1,
             TRUE ~ NA
           )) %>%
    ungroup() %>%
    rename_with(~gsub("#", visit, .x, fixed=T))
  
  # Remove depression, anxiety, and exposure questions
  visit_i_df <- visit_i_df %>%
    select(-all_of(c(visit_i_depress, visit_i_anxiety, visit_i_A)))
  
  # Ordinal encoding
  ordinal_cols <- setdiff(c(visit_i_W, visit_i_A), visit_i_W_factor_minus1)[-c(1, 2)]
  visit_i_df <- visit_i_df %>%
    mutate(across(all_of(ordinal_cols), as.numeric)) %>%
    mutate(across(all_of(ordinal_cols), ~ . -1)) %>%
    rowwise()
  
  # Create censoring
  visit_i_df[[paste0("D_HORMUSER", visit)]] <- as.numeric(is.na(visit_i_df[[paste0("HORMUSER", visit)]]))
  visit_i_df[[paste0("C_", visit_i_Y[1])]] <- as.numeric(is.na(visit_i_df[[visit_i_Y[1]]]))
  visit_i_df[[paste0("C_", visit_i_Y[2])]] <- as.numeric(is.na(visit_i_df[[visit_i_Y[2]]]))
  
  # Rename INSULN1 to INSULIN and reorder
  Li <- paste(substr(L0, 1, nchar(L0) - 1), visit, sep="")
  visit_i_df <- visit_i_df %>%
    rename(!!paste0("INSULIN", visit) := paste0("INSULN1", visit)) %>%
    select(all_of(c("SWANID", Li, 
                    paste0("D_HORMUSER", visit), paste0("HORMUSER", visit), 
                    paste0("C_", visit_i_Y[1]), visit_i_Y[1], 
                    paste0("C_", visit_i_Y[2]), visit_i_Y[2])))

  # Merge
  clean_df <- merge(clean_df, visit_i_df, by="SWANID", all.x=T)
  
  # Refill D_HORMUSER, C_SPBMDT, C_HPBMDT
  censor_cols <- c(paste0("D_HORMUSER", visit), paste0("C_", visit_i_Y[1]), paste0("C_", visit_i_Y[2]))
  clean_df <- clean_df %>%
    mutate(across(all_of(censor_cols), ~replace_na(.x, 1)))
  
  # LOCF for intermediate covariates
  int_cov <- colnames(visit_i_df)[!colnames(visit_i_df) %in% c("SWANID", censor_cols)]
  
  for (col in int_cov) {
    subtract <- ifelse(visit == 10, 2, 1)
    prev_col <- paste0(substr(col, 1, nchar(col) - subtract), visit-1)
    ind <- which(is.na(clean_df[col]))
    clean_df[col][ind, ] <- clean_df[prev_col][ind, ]
  }
}

write.csv(clean_df, here("data", "both_outcome_no_shift.csv"), row.names=FALSE)
