# Estimation
library(ltmle)
library(here)
library(dplyr)
library(tidyverse)
library(data.table)

data <- read.csv(here("data", "clean_data.csv"), header=T)
View(clean_df[29,])
colnames(clean_df) %>% sort

View(clean_df)

cols <- grep("HORMUSER", names(clean_df), value = TRUE)

# Find which rows have at least one NA but not all NA among those columns
rows_with_some_na <- which(
  apply(is.na(clean_df[cols]), 1, any) &      # at least one NA
    !apply(is.na(clean_df[cols]), 1, all)       # not all NA
)

clean_df[rows_with_some_na, cols] %>% View()

which(apply(is.na(clean_df[cols]), 1, any))


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




