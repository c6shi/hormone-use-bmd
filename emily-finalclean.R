library(here)
library(dplyr)
library(tidyverse)
library(data.table)

#read data
final_df <- read.csv(here("data", "both_outcome_w_shift.csv"), header=TRUE)

#remove c_hip cols
final_df <- final_df %>% select((-starts_with("C_HPBMDT")))
#remove delta_a cols
final_df <- final_df %>% select((-starts_with("D_HORMUSER")))

#because we doing ltmle on hip with spine as a covariate, the spine cols should come before the A, C, Y
k <- 3   #number of columns to move forward
cols <- names(final_df)
hip_ind <- grep("^HPBMDT", cols) #positions of hip columns
new_order <- seq_along(cols)
for (i in hip_ind) {
  old_pos <- which(new_order == i)
  new_pos <- min(old_pos - k, length(cols)) #to not go off the end lol
  new_order <- append(new_order[-old_pos], old_pos, after = new_pos - 1) #remove from old_pos and reinsert at new_pos
}
final_df <- final_df[, new_order]

#fix censoring for c_spine --going to just make everything after the first appearance of censoring to be NA
c_spine_cols <- grep("C_SPBMDT", names(clean_df), value = TRUE)
all_cols <- names(final_df)
final_df <- final_df %>%
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

#View(final_df)

write.csv(final_df, here("data", "final_df.csv"), row.names=FALSE)
