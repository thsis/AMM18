################################################################################
library("dplyr")

setwd("~/AMM18/")
cola <- read.csv("data/cola_amm_boston.csv")
glimpse(cola)

# Subset data.
cola <- subset(cola, CHAIN %in% c(33, 65))
################################################################################
# Refactor Variables.
cola$iri_key <- factor(cola$iri_key)
cola$year <- factor(cola$year)
cola$week <- factor(cola$week)
cola$VOL_EQ <- factor(cola$VOL_EQ)
cola$display <- factor(cola$display, levels = 0:2)
cola$CHAIN <- factor(cola$CHAIN)

## Tabling the feature variable reveals that there is not much information 
## lost if we recode it into a binary variable.
table(cola$feature)


################################################################################
## Precautions: 
### Are the 'total_vol' and 'total_rev'-columns too highly correlated? 
### Yes, they definitely are. Dropping 'rev'-columns.
cor(cola$total_vol_carbbev, cola$total_rev_carbbev)
cor(cola$total_vol_cola, cola$total_rev_cola)
cor(cola$total_vol_l4, cola$total_rev_l4)

### Are the 'total_vol_carbbev', 'total_vol_cola' and 'total_vol_l4'-
### columns too highly correlated? 
### Jesus, that correlation is too damn high. Keeping only 'total_vol_l4'.
cor(cola$total_vol_carbbev, cola$total_vol_cola)
cor(cola$total_vol_cola, cola$total_vol_l4)
cor(cola$total_vol_carbbev, cola$total_vol_l4)

## Store only relevant columns.
columns = c("CHAIN", "iri_key", "year", "week", "L5", "PACKAGE", "units", 
            "price_deflated", "feature", "display", "total_vol_l4")

################################################################################
# Seasonal dummies
## Christmas + Thanksgiving + 4th of July
christmas= c(1165, 1217, 1269, 1321, 1373, 1425, 1478, 1530, 1582, 1634, 1687)
july4th = c(1139, 1191, 1243, 1295, 1347, 1399, 1451, 1503, 1555, 1607, 1659)
thanksgiving = c(1160, 1213, 1265, 1317, 1369, 1421, 1473, 1526, 1578, 1630, 1682)

## NBA-Finals + Superbowl
super_bowl = c(1117, 1170, 1217, 1274, 1327, 1379, 1431, 1483, 1535, 1588, 1640)

################################################################################

default_formula <- "log(units)~log(price_deflated)+feature+display+feature:display-1"

analyse <- function(chain, l5, pkg, formula){
  sub.cola = subset(
    cola,
    (CHAIN == chain) & (L5 == l5) & (PACKAGE == pkg))
  model = lm(formula, data=sub.cola)
  print(summary(model))
  par(mfrow = c(2, 2))
  plot(model)
  par(par(mfrow = c(1, 1)))
  return(model)
}

## Model 1a: 33 - Diet Pepsi - Can

cola[8999, ] # June 27, 2011: price is $0.01 - still 1 sale
cola[397, ] # March 16, 2009: No idea
cola[431, ] # January 18, 2010: No idea

cola <- cola[-8999, ]
model1a <- analyse(33, 'DIET PEPSI', 'CAN', default_formula)

### Model diagnostics:
### shenanigans at 8999, 431, 397
#### -8999: price of 0.01 -> recommend to remove that observation.
### shenanigans at 10571, 23608, 36013
cola[which(rownames(cola) %in% c("10571", "23603", "36013")), ]

## Model 1b: 33 - Diet Pepsi - Bottle
model1b <- analyse(33, 'DIET PEPSI', 'BOTTLE', default_formula)

## Model 2a: 33 - Pepsi - Can
model2a <- analyse(33, 'PEPSI', 'CAN', default_formula)

## Model 2b: 33 - Pepsi - Bottle
model2b <- analyse(33, 'PEPSI', 'BOTTLE', default_formula)

## Model 3a: 33 - Coke Classic - Can
model3a  <- analyse(33, 'COKE CLASSIC', 'CAN', default_formula)

## Model 3b: 33 - Coke Classic - Bottle
model3b <- analyse(33, 'COKE CLASSIC', 'BOTTLE', default_formula)

## Model 4a: 33 - Diet Coke - Can
model4a <- analyse(33, 'DIET COKE', 'CAN', default_formula)

## Model 4b: 33 - Diet Coke - Bottle
model4b <- analyse(33, 'DIET COKE', 'BOTTLE', default_formula)

## Model 5a: 65 - Coke Classic - Can
model5a <- analyse(65, 'COKE CLASSIC', 'CAN', default_formula)

## Model 5b: 65 - Coke Classic - Bottle
model5b <- analyse(65, 'COKE CLASSIC', 'BOTTLE', default_formula)

## Model 6a: 65 - Diet Coke - Can
model6a <- analyse(65, 'DIET COKE', 'CAN', default_formula)

## Model 6b: 65 - Diet Coke - Bottle
model6b <- analyse(65, 'DIET COKE', 'BOTTLE', default_formula)

## Model 7a: 65 - Diet Pepsi - Can
model7a <- analyse(65, 'DIET PEPSI', 'CAN', default_formula)

## Model 7b: 65 - Diet Pepsi - Bottle
model7b <- analyse(65, 'DIET PEPSI', 'BOTTLE', default_formula)

## Model 8a: 65 - Pepsi - Can
model8a

## Model 8b: 65 - Pepsi - Bottle
model8b