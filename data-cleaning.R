# EDA
library(here)
library(dplyr)
library(tidyverse)
library(data.table)


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

baseline_W0_inbaseline <- c("RACE", 
                            "AGE0", 
                            "HEIGHT0", 
                            "WEIGHT0", 
                            "STATUS0", 
                            "INSULIN0",
                            "SMOKERE0",
                            # alcohol use "",
                            sapply(depress_prefix, function(x) paste0(x, 0)),
                            sapply(anxiety_prefix, function(x) paste0(x, 0)))
baseline_W0_inbaseline <- stack(baseline_W0_inbaseline)$values

baseline_W0_inscreener <- c("DIABETE",
                            "PHY_ACT",
                            "MARITALGP",
                            "DEGREE")

# TIME-VARYING COVARIATES
visit_W <- c("AGE", 
             "HEIGHT",
             "WEIGHT",
             "STATUS", 
             "DIABETE",
             "INSULN1",
             # "DRNKBEE",
             # "PHYSACT",
             "MARITAL",
             depress_prefix,
             anxiety_prefix)


##### CONSTRUCT COHORT FROM BASELINE #####
# We start with N=3302 participants at baseline. 

# Add screener variables to baseline dataframe
screener_df <- da04368.0001[da04368.0001$SWANID %in% da28762.0001$SWANID, c("SWANID", baseline_W0_inscreener)]

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
# 2) missing baseline covariates. (N = 2689)
visit1_A <- sapply(A_prefix, function(x) paste0(x, 1))
visit1_hormone_df <- da29221.0001 %>% 
  filter_at(vars(visit1_A), all_vars(!is.na(.)))

baseline_df <- baseline_df[baseline_df$SWANID %in% visit1_hormone_df$SWANID, ]

baseline_df <- baseline_df %>%
  filter_at(vars(baseline_W0_inbaseline), all_vars(!is.na(.)))

# Remove women with 
# 1) NA SPBMDT0 and HPBMDT0 because we assume they were null as two sites did not measure BMD
baseline_df <- baseline_df %>%
  filter_at(vars(SPBMDT0, HPBMDT0), all_vars(!is.na(.)))
  

# Rename exposure variable, convert all factors except RACE & STATUS to numerics - 1, create depression & anxiety variable
baseline_depress0 <- stack(sapply(depress_prefix, function(x) paste0(x, 0)))$values
baseline_anxiety0 <- stack(sapply(anxiety_prefix, function(x) paste0(x, 0)))$values
baseline_Y0 <- stack(sapply(Y_prefix, function(x) paste0(x, 0)))$values
baseline_factor_minus1 <- c(baseline_W0_inbaseline[-c(1:5)], "HORMUSER0")
baseline_df <- baseline_df %>%
  rename("HORMUSER0" = "HORMPIL0") %>%
  select(all_of(c("SWANID", baseline_W0_inbaseline, "HORMUSER0", baseline_Y0))) %>%
  mutate(across(all_of(baseline_factor_minus1), as.numeric)) %>%
    mutate(across(all_of(baseline_factor_minus1), ~ . -1)) %>%
  rowwise() %>%
    mutate("DEPRESSION0" = sum(c_across(all_of(baseline_depress0))),
           "ANXIETY0" = sum(c_across(all_of(baseline_anxiety0)))) %>%
    ungroup()

baseline_W0 <- c("RACE", 
                 "AGE0", 
                 "HEIGHT0", 
                 "WEIGHT0", 
                 "STATUS0", 
                 "INSULIN0",
                 "SMOKERE0",
                 # alcohol use "",
                 "DEPRESSION0",
                 "ANXIETY0")

baseline_df <- baseline_df %>%
  select(all_of(c("SWANID", baseline_W0, "HORMUSER0", baseline_Y0)))

screener_df <- screener_df %>%
  filter_at(vars(baseline_W0_inscreener), all_vars(!is.na(.))) %>%
  select(c(SWANID, baseline_W0_inscreener)) 
  # mutate(across(where(is.factor), as.numeric))

# Merge screener dataframe and filtered baseline dataframe
clean_df <- merge(screener_df, baseline_df, by="SWANID")

# Create censoring variable for baseline
clean_df <- clean_df %>%
  mutate("CSPINE0" = if_else(is.na(!!sym(baseline_Y0[1])), 0, 1),
         "CHIP0" = if_else(is.na(!!sym(baseline_Y0[2])), 0, 1)) 

# Same thing as above, another way to do dynamic variables
# clean_df <- clean_df %>%
#   mutate("CSPINE0" = if_else(is.na(.data[[baseline_Y0[1]]]), 0, 1),
#          "CHIP0" = if_else(is.na(.data[[baseline_Y0[2]]]), 0, 1)) 


##### ADDING EACH VISIT #####
for (i in 3:length(all_icpsr)) {
  icpsr <- all_icpsr[i]
  code <- substr(icpsr, 7, 12)
  visit <- i-2
  
  print(c(code, visit))
  
  visit_i_W <- stack(sapply(visit_W, function(x) paste0(x, visit)))$values
  visit_i_A <- stack(sapply(A_prefix, function(x) paste0(x, visit)))$values
  visit_i_Y <- stack(sapply(Y_prefix, function(x) paste0(x, visit)))$values
  visit_i_df <- eval(parse(text=paste(sprintf("da%s.0001", code))))
  visit_i_df <- visit_i_df %>%
    select(all_of(c("SWANID", visit_i_W, visit_i_A, visit_i_Y)))
  
  visit_i_depress <- stack(sapply(depress_prefix, function(x) paste0(x, visit)))$values
  visit_i_anxiety <- stack(sapply(anxiety_prefix, function(x) paste0(x, visit)))$values
  
  visit_i_W_factor_minus1 <- c(visit_i_W[-c(1:4)], visit_i_A)
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
  
  visit_i_W <- c("AGE", 
                 "HEIGHT",
                 "WEIGHT",
                 "STATUS", 
                 "DIABETE",
                 "INSULN1",
                 # "DRNKBEE",
                 # "PHYSACT",
                 "MARITAL",
                 "DEPRESSION",
                 "ANXIETY")
  visit_i_W <- stack(sapply(visit_i_W, function(x) paste0(x, visit)))$values
  
  visit_i_df <- visit_i_df %>%
    select(all_of(c("SWANID", visit_i_W, sprintf("HORMUSER%s", visit), visit_i_Y)))
  
  visit_i_df <- visit_i_df %>%
    mutate("CSPINE#" = if_else(is.na(!!sym(visit_i_Y[1])), 0, 1),
           "CHIP#" = if_else(is.na(!!sym(visit_i_Y[2])), 0, 1)) %>%
    rename_with(~gsub("#", visit, .x, fixed=T))
  
  # left join with baseline
  clean_df <- merge(clean_df, visit_i_df, by="SWANID", all.x=T)
}

###################################

which(colnames(clean_df) == "HPBMDT0") #everything up to the 17th col is baseline covariates

for (i in 1:17){
  print(any(is.na(clean_df[,i])))
} #no one is missing its baseline covariates, so we don't need the delta nodes for that

#dropped the age variables after the baseline 
clean_df <- clean_df %>% select(-grep("AGE", names(clean_df), value=TRUE)[-1])

##### LVCF!! for the intermediate covariates #####

#fixing some column names
clean_df <- rename(clean_df, DIABETE0 = DIABETE)
clean_df <- rename(clean_df, MARITAL0 = MARITALGP)
names(clean_df) <- sub("INSULN\\d?", "INSULIN", names(clean_df)) #fixed insuln to insulin and gets rid of the extra digit

#making the level names of all the status columns the same (they are the same thing just written differently and that is messing with things)
status_cols <- grep("STATUS", names(clean_df), value = TRUE)[-1]
for (col in status_cols){
  levels(clean_df[[col]]) <- levels(clean_df$STATUS0)
}

#vector with all the col names for the intermediate covariates
int_cov <- grep("HORMUSER|SPBMDT|HPBMDT|CSPINE|CHIP", names(clean_df)[-c(1:17)], value = TRUE, invert = TRUE)

#actually doing LVCF
for (cur_col in int_cov){
  i <- as.numeric(regmatches(cur_col, regexpr('\\d+', cur_col))) #extracts the number at the end of the column name
  prev_col <- paste(regmatches(cur_col, regexpr('[[:alpha:]]+', cur_col)), i-1, sep = "") #gets the name of the column from the previous visit
  ind <- which(is.na(clean_df[cur_col])) #index of which values are NA and need to get replaced
  clean_df[cur_col][ind,] <- clean_df[prev_col][ind,] #replaces NAs with value from previous visit
}

any(is.na(clean_df[int_cov])) #yay 

##### adding delta A columns, indicator of whether the exposure was measured ###
treat_cols <- grep("HORMUSER", names(clean_df), value = TRUE)

delta_cols <- sub("USER", "DELTA", treat_cols)
for (i in seq_along(treat_cols)) {
  clean_df <- clean_df %>%
    mutate(
      !!delta_cols[i] := ifelse(is.na(.data[[treat_cols[i]]]), 0, 1)) %>% #makes the delta col
    relocate(
      !!sym(delta_cols[i]), .after = !!sym(treat_cols[i])) #moves it right after the corresponding treatment col
}

##### make baseline diabete into 0/1 
no_diabetes <- levels(clean_df$DIABETE0)[1]
clean_df$DIABETE0 <- ifelse(clean_df$DIABETE0 == no_diabetes, 0, 1)

##### LVCF for the outcome. we only want to censor people who are missing the outcome at visit 10. 
#missing inbetween values will be imputed using LVCF.

#first, let's check if there are people who have an outcome at timepoint 10 who are censored sometime before that
#spine
cspine_cols <- clean_df %>% select(grep("CSPINE", names(clean_df), value = T))
cspine_cols %>% filter(CSPINE10 == 1, rowSums(cspine_cols) != 11) #yes, there are many.
which(cspine_cols$CSPINE10 ==1 & rowSums(cspine_cols) != 11)

#hip
chip_cols <- clean_df %>% select(grep("CHIP", names(clean_df), value = T))
chip_cols %>% filter(CHIP10 == 1, rowSums(chip_cols) != 11) #huh there might be less rows than with the spine?

#now we will do LVCF on the SPBMDT (spine outcome) columns 
spine_cols <- grep("SPBMDT", names(clean_df), value = TRUE)[-1]
test_df <- clean_df
for (cur_col in spine_cols){
  i <- as.numeric(regmatches(cur_col, regexpr('\\d+', cur_col))) #extracts the number at the end of the column name
  prev_col <- paste(regmatches(cur_col, regexpr('[[:alpha:]]+', cur_col)), i-1, sep = "") #gets the name of the column from the previous visit
  ind <- which(is.na(clean_df[cur_col]) & !is.na(clean_df$SPBMDT10)) #index of which values are NA and need to get replaced, but only if the outcome at the last timepoint is not missing
  test_df[cur_col][ind,] <- clean_df[prev_col][ind,] #replaces NAs with value from previous visit
}

View(test_df)


View(clean_df %>% select(grep("SPBMDT", names(clean_df), value = TRUE)))
View(test_df %>% select(grep("SPBMDT", names(clean_df), value = TRUE)))
## 11/26 notes, need to fix LVCF code to handle two NAs in a row properly, then do the hip columns, then do one-hot encoding

write.csv(clean_df, here("data", "clean_data.csv"), row.names=F)

