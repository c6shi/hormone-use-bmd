# Estimation
library(ltmle)
library(here)
library(dplyr)
library(tidyverse)
library(data.table)

data <- read.csv(here("data", "clean_data.csv"), header=T)
