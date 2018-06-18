setwd("C:\\Users\\julia\\Desktop\\AMM18\\data")

# Packages
library(dplyr)
library(lubridate)

# Read data
data.cola <- read.csv("cola_amm_boston.csv")

# Week_coding 
week.coding <- read.csv("IRI week translation_2008_2017.csv", 
                        stringsAsFactors = F)
week.coding[ , 4:6] <- NULL 
colnames(week.coding) <- c("IRI.Week","Start","End")

week.coding$Start <- as.Date(mdy(week.coding$Start), format = "%U %Y")
week.coding$End   <- mdy(week.coding$End)


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

difftime(strptime("03.09.1979", format = "%d.%m.%Y"),
         strptime("31.12.2007", format = "%d.%m.%Y"),units="weeks")
