library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(ltmle)
library(SuperLearner)

source('candus-reproducibility.R')
source('candus-ltmle.R')
source('candus-ctmle.R')

##### 1) Generate clean dataset from pre-specified covariates & run L-TMLE #####
depress_prefix <- c("BOTHER", "APPETIT", "BLUES", "GOOD",
                    "KEEPMIN", "DEPRESS", "EFFORT", "HOPEFUL",
                    "FAILURE", "FEARFUL", "RESTLES", "HAPPY",
                    "TALKLES", "LONELY", "UNFRNDL", "ENJOY",
                    "CRYING", "SAD", "DISLIKE", "GETGOIN")
anxiety_prefix <- c("IRRITAB", "NRVOUS", "HARTRAC", "FEARFULA")

# full variable names for screener and baseline variables
# prefix for visit variables
# can only change covariates to some degree! variable names are hard-coded
#   in the functions since we are data cleaning for this specific question of 
#   interest; see markdown for what the dict MUST INCLUDE
# write code to check dict for variables before running; throw exception/error

sample_variable_dict <- list(
  "screener" = list(
    "covariate" = c("DIABETE", "PHY_ACT", "MARITALGP", "DEGREE"),
    "exposure" = c(),
    "outcome" = c()
  ),
  "baseline" = list(
    "covariate" = c("RACE", "AGE0", "HEIGHT0", "WEIGHT0", "STATUS0", "INSULIN0",
                    "SMOKERE0", 
                    # add alcohol use, 
                    sapply(depress_prefix, function(x) paste0(x, 0)),
                    sapply(anxiety_prefix, function(x) paste0(x, 0))),
    "exposure" = c("HORMPIL0"),
    "outcome" = c("SPBMDT0", "HPBMDT0")
  ), 
  "visit" = list(
    "covariate" = c("HEIGHT", "WEIGHT", "STATUS", "DIABETE", "INSULN1", 
                    # these are missing in some visits: "DRNKBEE", "PHYSACT",
                    "MARITAL", depress_prefix, anxiety_prefix),
    "exposure" = c("COMBIN1", "ESTROG1", "PROGES1", "ESTRNJ1"),
    "outcome" = c("SPBMDT", "HPBMDT")
  )
)

data <- generate.Clean.Data(sample_variable_dict)
# data <- read.csv(here("data", "clean_data.csv"), header=T)

# how to define Anodes, etc. ?
# let's run a test LTMLE with subset of variables that are numeric for now
test_cols_prefix <- c("INSULIN", "DEPRESSION", "ANXIETY", 
                      "HORMUSER", "HORMDELTA", "CSPINE","SPBMDT")
test_cols <- c()
for (i in 0:10) {
  test_cols <- c(test_cols, sapply(test_cols_prefix, function(x) paste0(x, i)))
}
test_cols <- stack(test_cols)$values

test_data <- data %>%
  select(all_of(test_cols))

test_data <- subset(test_data, select=-c(CSPINE0))

censoring_cols <- grep("CSPINE", names(test_data), value=TRUE)
for (col in censoring_cols) {
  test_data[[col]] <- BinaryToCensoring(is.uncensored = test_data[[col]])
}

Anodes <- grep("HORM", names(test_data), value=TRUE)
Ynodes <- grep("SPBMDT", names(test_data), value=TRUE)
Lnodes <- test_cols[!test_cols %in% Anodes & !test_cols %in% Ynodes & !test_cols %in% censoring_cols]

test_data <- test_data %>%
  select("SPBMDT0", "INSULIN0", everything())

# need to fill A node NAs with something
test_data[Anodes][is.na(test_data[Anodes])] <- 0

tmle_fit <- ltmle(data = test_data,
                  Anodes = Anodes,
                  Cnodes = censoring_cols,
                  Lnodes = Lnodes[-c(1,2,3,4)],
                  Ynodes = Ynodes[-c(1)],
                  abar = list(rep(1, length(Anodes)),
                              rep(c(0, 1), length(Anodes) %/% 2)),
                  SL.library = c("SL.glm", "SL.earth"))

summary(tmle_fit)

# do.LTMLE(data, "")

##### 2) Run C-TMLE and obtain data-adaptively selected covariates & run L-TMLE #####


##### Compare! #####