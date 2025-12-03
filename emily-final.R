
library(here)
library(dplyr)
library(tidyverse)
library(data.table)

#read data
final_df <- read.csv("both_outcome_w_shift.csv")

#remove c_hip cols
final_df <- final_df %>% select((-starts_with("C_HPBMDT")))
#remove delta_a cols
final_df <- final_df %>% select((-starts_with("D_HORMUSER")))

#fix censoring for c_spine --going to just make everything after the first appearance of censoring to be NA
c_spine_cols <- grep("C_SPBMDT", names(clean_df), value = TRUE)

all_cols <- names(final_df)
test_df <- final_df[1:25,]

test_df <- test_df %>%
  rowwise() %>%
  mutate(
    pos_small = match(1, c_across(all_of(c_spine_cols))), #position of the column with the first censor in c_spine_cols
    pos_big = if (!is.na(pos_small)){
      match(c_spine_cols[pos_small], all_cols) #position within the list of all columns
    } 
      else {
        NA
      }, 
    across(all_cols, ~ if (!is.na(pos_big) && match(dplyr::cur_column(), all_cols) > pos_big) {
        NA #if the current column is after the first censor, set value to NA
      } else {
        . #otherwise do nothing
      })
  ) %>%ungroup() %>% select(-pos_small, -pos_big) #get rid of added position columns

View(test_df)
         

