setwd("C:\\Users\\julia\\Desktop\\AMM18\\data")

# Packages
library(dplyr)
library(lubridate)

# Read data
data.cola <- read.csv("cola_amm_boston.csv")

# Week_coding 
data.cola <- data.cola %>% mutate(weeknum = factor((week %% 52)+1))

# Subset meaningful chains
data.cola <- cola %>% filter(CHAIN %in% c(33,65))

# To factor 
data.cola$year         <- as.factor(data.cola$year)
data.cola$iri_key      <- as.factor(data.cola$iri_key)
data.cola$display      <- as.factor(data.cola$display)
data.cola$feature      <- as.factor(data.cola$feature)

# filter data frame
bottle = data.cola %>% filter(PACKAGE == "BOTTLE")
can = data.cola %>% filter(PACKAGE == "CAN")

# aggregate over iri_keys and create new dataframe for bottle
bottle_units = bottle %>%
  group_by(week, L5) %>%
  summarize(units = sum(units))

# aggregate over iri_keys and create new dataframe for can
can_units = can %>%
  group_by(week, L5) %>%
  summarize(units = sum(units))