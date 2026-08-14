# Estimation via L-TMLE
library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(ltmle)
library(SuperLearner)
library(ggplot2)
library(stringr)
library(lmtp)
library(progressr)
library(future)
handlers(global = TRUE)

# Read data
spine_df <- read.csv(here("data", "spine_final.csv"), header=TRUE)
hip_df <- read.csv(here("data", "hip_final.csv"), header=TRUE)
dfs <- list("spine" = spine_df, "hip" = hip_df)

# Data with outcome of interest (change to spine or hip)
outcome_of_interest <- "hip"
dfoi <- dfs[[outcome_of_interest]]
censor_prefix_reg <- ifelse(outcome_of_interest == "spine", "^C_SPBMDT", "^C_HPBMDT")
outcome_prefix_reg <- ifelse(outcome_of_interest == "spine", "^SPBMDT", "^HPBMDT")
last_baselinecov <- ifelse(outcome_of_interest == "spine", "HPBMDT0", "SPBMDT0")

# Tweak dataframe to have HORMUSER0 be right before the first Ynode

##### Attempt 1: Static Intervention #####

# Define nodes for L-TMLE
cols <- colnames(dfoi)
Anodes <- grep("^HORMUSER", names(dfoi), value=TRUE)
Cnodes <- grep(censor_prefix_reg, names(dfoi), value=TRUE)
Ynodes <- grep(outcome_prefix_reg, names(dfoi), value=TRUE)
Lnodes <- cols[!cols %in% Anodes & !cols %in% Ynodes & !cols %in% Cnodes]
Lnodes <- Lnodes[-c(1:which(Lnodes == last_baselinecov))] # need to remove baseline covs from Lnodes (check docs)

# Map Cnodes to "censored" vs "uncensored"
for (col in Cnodes) {
    dfoi[col] <- BinaryToCensoring(is.censored = dfoi[col])
}

# Construct intervention nodes (abar)
abar1 <- rep(1, length(Anodes))
abar0 <- rep(0, length(Anodes))

# Bound for positivity
bound <- 5/sqrt(nrow(dfoi))/log(nrow(dfoi))

# L-TMLE with small library and screeners and bound for pos
start_time <- Sys.time()
fit1 <- ltmle(data = dfoi,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              gbounds = c(bound, 1-bound),
              SL.library = list("SL.glm", "SL.earth",
                                c("SL.glm", "screen.corP"),
                                c("SL.earth", "screen.corP"))
              )
end_time <- Sys.time()
print(end_time - start_time)
# 3.86856 mins
summary(fit1)
# for spine BMD:
# Additive Treatment Effect:
# Parameter Estimate:  0.065513 
# Estimated Std Err:  0.010989 
# p-value:  2.4962e-09 
# 95% Conf Interval: (0.043975, 0.087051) 

# for hip BMD:
# Additive Treatment Effect:
# Parameter Estimate:  0.025075
# Estimated Std Err:  0.0088218
# p-value:  0.0044784
# 95% Conf Interval: (0.0077841, 0.042365)

# check influence curve
fit1_IC_df <- data.frame(IC = fit1$IC[,1])
ggplot(fit1_IC_df, aes(x = IC)) + 
  geom_histogram(aes(y=after_stat(density)), color = "black", fill = "#3b5b8a") + 
  labs(title = sprintf("Histogram of ICs for %s BMD Estimate Under Static Intervention", str_to_title(outcome_of_interest)),
       y = "Density") + 
  theme_minimal()

##### Attempt 3: LMTP #####
# first, try to replicate the L-TMLE static intervention results via LMTP_TMLE
d1 <- function(data, a) {
  rep(1, nrow(data))
}

d0 <- function(data, a) {
  rep(0, nrow(data))
}

# need to include Ynodes up to last Y in Lnodes
# and # need to change censoring to be 0 = censored, 1 = uncensored
Lnodes_list <- list()
for (i in 1:10) {
  start_index <- ((14 * (i-1)) + 1)
  stop_index <- 14*i
  Lnodes_list[[i]] <- c(Lnodes[start_index:stop_index], Ynodes[i+1])
  censoring_col <- Cnodes[i]
  dfoi[censoring_col] <- 1 - dfoi[censoring_col]
}
Lnodes_list[[10]] <- Lnodes_list[[10]][1:13]

# do I put the Lnodes 0 as the first in Lnodes_list?
# and no Lnodes 10 (since measured at the same time as Y10)?
# Lnodes_list <- list()
# Lnodes_list[[1]] <- c(cols[2:18], cols[20])
# for (i in 1:9) {
#   start_index <- ((14 * (i-1)) + 1)
#   stop_index <- 14*i
#   Lnodes_list[[i+1]] <- c(Lnodes[start_index:stop_index], Ynodes[i+1])
#   censoring_col <- Cnodes[i]
#   dfoi[censoring_col] <- 1 - dfoi[censoring_col]
# }

learners <- list("SL.glm", "SL.earth",
                 c("SL.glm", "screen.corP"),
                 c("SL.earth", "screen.corP"))

baseline <- c(cols[2:18], cols[20])

plan(multisession, workers = 5)
fit_lmtp_d1 <- lmtp_tmle(
  data = dfoi, 
  trt = Anodes, 
  outcome = Ynodes[11], 
  baseline = baseline,
  time_vary = Lnodes_list, 
  cens = Cnodes, 
  id = "SWANID",
  shift = d1, 
  mtp = TRUE,
  outcome_type = "continuous",
  learners_outcome = learners,
  learners_trt = learners,
  folds = 5,
  control = lmtp_control(.trim = 1-bound)
)

fit_lmtp_d0 <- lmtp_tmle(
  data = dfoi,
  trt = Anodes,
  outcome = Ynodes[11],
  baseline = baseline,
  time_vary = Lnodes_list,
  cens = Cnodes,
  id = "SWANID",
  shift = d0,
  mtp = TRUE,
  outcome_type = "continuous",
  learners_outcome = learners,
  learners_trt = learners,
  folds = 5,
  control = lmtp_control(.trim = 1-bound)
)

lmtp_contrast(fit_lmtp_d1, ref=fit_lmtp_d0)

# LMTP 1: stay on MHTs one visit longer after first use of MHT

# LMTP 3: start and stay on MHTs after reaching late perimenopause or age 55,
# whichever happens first
d_start_lateperi_or_55 <- dfoi
for (i in 1:9) {
  a_col <- sprintf("HORMUSER%s", i)
  age_col <- "AGE"
  status_col <- sprintf("STATUS%s", i)
  current_age <- dfoi[[age_col]] + i
  d_start_lateperi_or_55$current_age <- dfoi[[age_col]] + i
  censor_col <- paste0(substr(censor_prefix_reg, start=2, stop=nchar(censor_prefix_reg)), i)
  
  d_start_lateperi_or_55[[a_col]] <- apply(d_start_lateperi_or_55, 1, function(row) {
    if (row["current_age"] > 55 || row[status_col] == 2) {
      return(1)
    } else {
      return(row[[a_col]])
    }
  })
  
  d_start_lateperi_or_55[censor_col] <- 1
  
  if (i == 9) {
    next_censor_col <- paste0(substr(censor_prefix_reg, start=2, stop=nchar(censor_prefix_reg)), i+1)
    d_start_lateperi_or_55[next_censor_col] <- 1
  }
}

# check the change in distribution! (doing this in python, jupyter notebook)
sum(!(d_start_lateperi_or_55$HORMUSER9 == dfoi$HORMUSER9))

fit_lmtp_d_start_lateperi_or_55 <- lmtp_tmle(
  data = dfoi, 
  trt = Anodes, 
  outcome = Ynodes[11], 
  baseline = baseline,
  time_vary = Lnodes_list, 
  cens = Cnodes, 
  id = "SWANID",
  shifted = d_start_lateperi_or_55, 
  mtp = TRUE,
  outcome_type = "continuous",
  learners_outcome = learners,
  learners_trt = learners,
  folds = 5
)

fit_observed <- lmtp_tmle(
  data = dfoi, 
  trt = Anodes, 
  outcome = Ynodes[11], 
  baseline = baseline, 
  time_vary = Lnodes_list, 
  cens = Cnodes, 
  id = "SWANID",
  shift = NULL,
  mtp = TRUE,
  outcome_type = "continuous",
  learners_outcome = learners,
  learners_trt = learners,
  folds = 5
)

lmtp_contrast(fit_lmtp_d_start_lateperi_or_55, ref=fit_observed)

plan(sequential)
