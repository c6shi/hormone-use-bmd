# Estimation
library(ltmle)
library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(SuperLearner)
library(glmnet)
library(earth)
#install.packages("SuperLearner")
#install.packages("glmnet")
install.packages("earth")

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

fit2 <- ltmle(data = final_df,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              gbounds = c(bound, 1-bound),
              SL.library = list("SL.glm", "SL.glmnet", "SL.earth", 
                                c("SL.glm", "screen.corP"), c("SL.glmnet", "screen.corP"),
                                c("SL.earth", "screen.corP")))

summary(fit2)
#Additive Treatment Effect:
#Parameter Estimate:  0.064189 
#Estimated Std Err:  0.011008 
#p-value:  5.5009e-09 
#95% Conf Interval: (0.042614, 0.085763) 

hist(fit2$IC[,1])
fit2$fit$Q[[1]] #after looking at this, gonna remove glmnet and add something else??

#being ambitious
fit3 <- ltmle(data = final_df,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              gbounds = c(bound, 1-bound),
              SL.library <- list(
                "SL.glm", "SL.glm.interaction", "SL.earth", "SL.ranger",
                c("SL.glm", "screen.corP"),
                c("SL.glm.interaction", "screen.corP"),
                c("SL.earth", "screen.corP"),
                c("SL.ranger", "screen.corP")))
              
