# Estimation
library(ltmle)
library(here)
library(dplyr)
library(tidyverse)
library(data.table)

final_df <- read.csv(here("data", "final_df.csv"), header=TRUE)
final_df <- final_df %>% select(-SWANID)

cols <- colnames(final_df)
Anodes <- grep("HORMUSER", cols, value=TRUE)
Cnodes <- sapply(1:10, function(i) paste0("C_SPBMDT", i))
Ynodes <- sapply(0:10, function(i) paste0("SPBMDT", i))
Lnodes <- cols[!cols %in% Anodes & !cols %in% Ynodes & !cols %in% Cnodes]
Lnodes <- Lnodes[-c(1:which(Lnodes == "HPBMDT0"))]

for (col in Cnodes) {
  final_df[col] <- BinaryToCensoring(is.censored = final_df[col])
}

abar1 <- rep(1, length(Anodes))
abar0 <- rep(0, length(Anodes))
bound <- 5/sqrt(nrow(final_df))/log(nrow(final_df))

baselinefit <- ltmle(data = final_df,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              gbounds = c(bound, 1-bound),
              SL.library = list("SL.glm", "SL.mean", c("SL.glm", "screen.corP")))

summary(baselinefit)
hist(baselinefit$IC[,1])
baselinefit$fit$Q[[2]]

#Additive Treatment Effect:
#Parameter Estimate:  0.06231 
#Estimated Std Err:  0.011828 
#p-value:  1.3777e-07 
#95% Conf Interval: (0.039129, 0.085492) 

#influence curve looks ok i think!

