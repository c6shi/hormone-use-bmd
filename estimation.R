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
# LMTP 1: stay on MHTs one visit longer after first use of MHT
d1 <- function(data, a) {
  rep(1, nrow(data))
}

d0 <- function(data, a) {
  rep(0, nrow(data))
}

Lnodes_list <- list()
for (i in 1:10) {
  start_index <- ((14 * (i-1)) + 1)
  stop_index <- 14*i
  Lnodes_list[[i]] <- Lnodes[start_index:stop_index]
}
Lnodes_list[[10]] <- Lnodes_list[[10]][1:13]

fit_lmtp_d1 <- lmtp_tmle(
  data = dfoi, 
  trt = Anodes, 
  outcome = Ynodes[11], 
  time_vary = Lnodes_list, 
  cens = Cnodes, 
  id = "SWANID",
  shift = d1, 
  mtp = TRUE,
  outcome_type = "continuous",
  folds = 1
)

fit_lmtp_d0 <- lmtp_tmle(
  data = dfoi,
  trt = Anodes,
  outcome = Ynodes[11],
  time_vary = Lnodes_list,
  cens = Cnodes,
  id = "SWANID",
  shift = d0,
  mtp = TRUE,
  outcome_type = "continuous",
  folds = 1
)

lmtp_contrast(fit_lmtp_d1, ref=fit_lmtp_d0)
