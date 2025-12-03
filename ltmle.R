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





