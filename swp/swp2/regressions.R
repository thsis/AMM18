library("dplyr")

setwd("~/AMM18/")
cola <- read.csv("data/cola_amm_boston.csv")

# Subset data.
cola <- subset(cola, CHAIN %in% c(33, 65))
glimpse(cola)

cola$feature = !(cola$feature == "NONE")

cola = cola %>% 
  select(CHAIN, iri_key, year, week, L5, PACKAGE, units, price_deflated, feature, display) %>% 
  group_by(CHAIN, year, week, L5, PACKAGE) %>% 
  summarise(units = sum(units), 
            price_mean = mean(price_deflated),
            price_min = min(price_deflated),
            price_max = max(price_deflated),
            price_med = median(price_deflated),
            feature = mean(feature), 
            display = mean(display))

summary(cola)
cola %>% filter(CHAIN == 33) %>% summary()
cola %>% filter(CHAIN == 65) %>% summary()

glimpse(cola)

# Convert to factors
cola$CHAIN = factor(cola$CHAIN)
cola$year = factor(cola$year)
cola$week = factor(cola$week)

model = "log(units) ~ log(price_mean) + year + feature + display +christmas + july4th + thanksgiving - 1"

analyse = function(chain, l5, package){
  print(paste("CHAIN:", chain, "L5:", l5, "PKG:", package))
  cola.sub = subset(cola, (CHAIN == chain) & (PACKAGE == package) & (L5 == l5))
  cola.model = lm(model, data = cola.sub)
  par(mfrow = c(2, 2))
  print(plot(cola.model))
  par(mfrow = c(1, 1))
  print(summary(cola.model))
  
  return(cola.model)
}

# Add weird dates
## holidays
christmas= c(1165, 1217, 1269, 1321, 1373, 1425, 1478, 1530, 1582, 1634, 1687)
july4th = c(1139, 1191, 1243, 1295, 1347, 1399, 1451, 1503, 1555, 1607, 1659)
thanksgiving = c(1160, 1213, 1265, 1317, 1369, 1421, 1473, 1526, 1578, 1630, 1682)

## NBA-Finals + Superbowl
super_bowl = c(1117, 1170, 1217, 1274, 1327, 1379, 1431, 1483, 1535, 1588, 1640)

cola$christmas = cola$week %in% christmas
cola$july4th = cola$week %in% july4th
cola$thanksgiving = cola$week %in% thanksgiving
cola$super_bowl = cola$week %in% super_bowl

# Regressions for days
model_33_coke_classic_can = analyse(33, 'COKE CLASSIC', 'CAN')

models = c()
for (chain in levels(cola$CHAIN)) {
  for (l5 in levels(cola$L5)) {
    for (package in levels(cola$PACKAGE)) {
      models = c(models, analyse(chain, l5, package))
    }
  }
}
