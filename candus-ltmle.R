library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(ltmle)
library(SuperLearner)

both_outcome_no_shift <- read.csv(here("data", "both_outcome_no_shift.csv"), header=TRUE)
both_outcome_w_shift <- read.csv(here("data", "both_outcome_w_shift.csv"), header=TRUE)
both_outcome_w_shift <- both_outcome_w_shift %>%
  select(-SWANID)

# LTMLE on spine BMD not using hip BMD as a covariate
spine_w_shift_no_hip <- both_outcome_w_shift %>%
  select(-grep("HPBMDT", names(both_outcome_w_shift), value=TRUE))

cols <- colnames(spine_w_shift_no_hip)
Anodes <- grep("HORMUSER", names(spine_w_shift_no_hip), value=TRUE)
Cnodes <- sapply(1:10, function(i) paste0("C_SPBMDT", i))
Ynodes <- sapply(0:10, function(i) paste0("SPBMDT", i))
Lnodes <- cols[!cols %in% Anodes & !cols %in% Ynodes & !cols %in% Cnodes]
Lnodes <- Lnodes[-c(1:which(Lnodes == "SPBMDT0"))]

for (col in Cnodes) {
  spine_w_shift_no_hip[col] <- BinaryToCensoring(is.censored = spine_w_shift_no_hip[col])
}

abar1 <- c(1, rep(c(0, 1), length(Anodes) %/% 2))
abar0 <- rep(0, length(Anodes))

fit1 <- ltmle(data = spine_w_shift_no_hip,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              SL.library = c("SL.glm", "SL.earth"))

summary(fit1)
hist(fit1$IC[,1])
head(fit1$cum.g.unbounded)
hist(fit1$cum.g.unbounded[,,1][,5])
# ATE Estimate: 0.055003
# Estimated Std Err: 0.014412
# p-value: 0.00013538
# 95% CI: (0.026756, 0.08325)
# Interpretation: using hormone increases BMD
# many positivity violations

# use screeners
fit2 <- ltmle(data = spine_w_shift_no_hip,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              SL.library = list("SL.glm", "SL.earth", 
                                c("SL.glm", "screen.corP"),
                                c("SL.earth", "screen.corP")))
summary(fit2)
hist(fit2$IC[,1])
head(fit2$cum.g.unbounded)
hist(fit2$cum.g.unbounded[,,1][,5])
# ATE Estimate: 0.066273
# Estimated Std Err: 0.0146
# p-value: 5.6408e-06
# 95% CI: (0.037659, 0.094888)
# Interpretation: using hormone increases BMD
# many positivity violations

# LTMLE on spine BMD using hip BMD as a covariate
cols <- colnames(both_outcome_w_shift)
baseline <- cols[1:(which(cols == "HORMUSER0")-1)]
baseline_AY <- c("HPBMDT0", "HORMUSER0", "SPBMDT0")
L_prefix <- Lnodes[1:which(Lnodes=="ANXIETY1")]
L_prefix <- substr(L_prefix, 1, nchar(L_prefix) - 1)
order <- c(L_prefix, "HPBMDT", "D_HORMUSER", "HORMUSER", "C_SPBMDT", "SPBMDT")
order_w_visit <- paste(order, rep(c(1:10), each=length(order)), sep="")
order_w_visit <- order_w_visit[!order_w_visit %in% c("D_HORMUSER10", "HORMUSER10")]

spine_w_shift_w_hip <- both_outcome_w_shift %>%
  select(all_of(c(baseline, baseline_AY, order_w_visit)))

cols <- colnames(spine_w_shift_w_hip)
Anodes <- grep("HORMUSER", names(spine_w_shift_w_hip), value=TRUE)
Cnodes <- sapply(1:10, function(i) paste0("C_SPBMDT", i))
Ynodes <- sapply(0:10, function(i) paste0("SPBMDT", i))
Lnodes <- cols[!cols %in% Anodes & !cols %in% Ynodes & !cols %in% Cnodes]
Lnodes <- Lnodes[-c(1:which(Lnodes == "HPBMDT0"))]

for (col in Cnodes) {
  spine_w_shift_w_hip[col] <- BinaryToCensoring(is.censored = spine_w_shift_w_hip[col])
}

fit3 <- ltmle(data = spine_w_shift_w_hip,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              SL.library = c("SL.glm", "SL.earth"))

summary(fit3)
hist(fit3$IC[,1])
head(fit3$cum.g.unbounded)
hist(fit3$cum.g.unbounded[,,1][,5])
# ATE Estimate: 0.048515
# Estimated Std Err: 0.010987
# p-value: 1.0079e-05
# 95% CI: (0.02698, 0.070049)
# Interpretation: using hormone increases BMD
# many positivity violations

# use screeners
fit4 <- ltmle(data = spine_w_shift_w_hip,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              SL.library = list("SL.glm", "SL.earth", 
                                c("SL.glm", "screen.corP"),
                                c("SL.earth", "screen.corP")))
summary(fit4)
hist(fit4$IC[,1])
head(fit4$cum.g.unbounded)
hist(fit4$cum.g.unbounded[,,1][,5])
# ATE Estimate: 0.051707
# Estimated Std Err: 0.011238
# p-value: 4.202e-06
# 95% CI: (0.029681, 0.073732)
# Interpretation: using hormone increases BMD
# many positivity violations

# Stuff for presentation
fit4$Qstar
fit4$


# try default SL; will exit before completion, this takes so long!
fit5 <- ltmle(data = spine_w_shift_w_hip,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              SL.library = 'default')
summary(fit5)

# try a more tailored SL; uhh so ended up with a larger library :\
fit6 <- ltmle(data = spine_w_shift_w_hip,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              SL.library = list("SL.glm", "SL.glm.interaction", "SL.earth", "SL.ranger",
                                # c("SL.glm", "screen.glmnet"),
                                # c("SL.glm.interaction", "screen.glmnet"),
                                # c("SL.earth", "screen.glmnet"),
                                # c("SL.ranger", "screen.glmnet"),
                                c("SL.glm", "screen.corP"),
                                c("SL.glm.interaction", "screen.corP"),
                                c("SL.earth", "screen.corP"),
                                c("SL.ranger", "screen.corP")
                                # c("SL.glm", "screen.randomForest"),
                                # c("SL.glm.interaction", "screen.randomForest"),
                                # c("SL.earth", "screen.randomForest"),
                                # c("SL.ranger", "screen.randomForest")
                                ))

fit7 <- ltmle(data = final_df,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              gbounds = c(bound, 1-bound),
              SL.library = list(
                "SL.glm", "SL.earth",
                c("SL.glm", "screen.corP"),
                c("SL.earth", "screen.corP")
              ))
summary(fit7)
hist(fit7$IC[,1])
head(fit7$cum.g.unbounded)
hist(fit7$cum.g.unbounded[,,1][,7])

fit8 <- ltmle(data = final_df,
              Anodes = Anodes,
              Cnodes = Cnodes,
              Lnodes = Lnodes,
              Ynodes = Ynodes,
              abar = list(abar1, abar0),
              gbounds = c(bound, 1-bound),
              SL.library = list(
                "SL.glm", "SL.earth", "SL.mean",
                c("SL.glm", "screen.corP"),
                c("SL.earth", "screen.corP"),
                c("SL.mean", "screen.corP")))
                
              