library("dplyr")

data = read.csv("data/cola_amm_boston.csv")

# Define filters.
time_filter = 2010:2011
 
superbowl_filter = c(1117, 1170, 1217, 1274, 1327, 1379, 1431, 1483, 1535, 1588, 1640)
christmas_filter = c(1165, 1217, 1269, 1321, 1373, 1425, 1478, 1530, 1582, 1634, 1687)
newyearseve_filter = c(1166, 1218, 1270, 1322, 1374, 1426, 1479, 1531, 1583, 1635, 1687)
july4th_filter = c(1139, 1191, 1243, 1295, 1347, 1399, 1451, 1503, 1555, 1607, 1659)
thanksgiving_filter = c(1160, 1213, 1265, 1317, 1369, 1421, 1473, 1526, 1578, 1630, 1682)

redundant_columns = c(
  "MARKET",
  "store_type",
  "L4",
  "VOL_EQ")

# Apply filters and compute shares.
cola = data[, !colnames(data) %in% redundant_columns] %>%
  filter(year %in% time_filter & CHAIN == 65) %>%
  mutate(CHAIN = factor(CHAIN),
         year = factor(year),
         week = factor(week),
         display = factor(display),
         christmas = week %in% christmas_filter,
         newyearseve = week %in% newyearseve_filter,
         superbowl = week %in% superbowl_filter,
         july4th = week %in% july4th_filter,
         thanksgiving = week %in% thanksgiving_filter,
         share_carbbev = dollars / total_rev_carbbev,
         share_cola = dollars / total_rev_cola,
         share_l4 = dollars / total_rev_l4,
         outside_carbbev = 1 - share_carbbev,
         outside_cola = 1 - share_cola,
         outside_l4 = 1 - share_l4) %>% 
  arrange(iri_key, week) %>% 
  mutate(lagged_units = lag(units),
         lagged_price = lag(price))

# Construct model formulas
formulas = c("carbbev", "cola", "l4")
formulas = paste0("log(share_", formulas, ")-log(outside_", formulas,
                  ") ~ price + lagged_price + lagged_units + feature + display + christmas + newyearseve + superbowl + july4th + thanksgiving")

cola.can = cola %>% filter(PACKAGE == "CAN")
cola.bottle = cola %>% filter(PACKAGE == "BOTTLE")

# Compute models
## CANS
model_can_carbbev = lm(formulas[1], data = cola.can)
summary(model_can_carbbev)
plot(model_can_carbbev)

model_can_cola = lm(formulas[2], data = cola.can)
summary(model_can_cola)
plot(model_can_cola)

model_can_l4 = lm(formulas[3], data = cola.can)
summary(model_can_l4)
plot(model_can_l4)

## BOTTLES
model_bottle_carbbev = lm(formulas[1], data = cola.bottle)
summary(model_bottle_carbbev)
plot(model_bottle_carbbev)

model_bottle_cola = lm(formulas[2], data = cola.bottle)
summary(model_bottle_cola)
plot(model_bottle_cola)

model_bottle_l4 = lm(formulas[3], data = cola.bottle)
summary(model_bottle_l4)
plot(model_bottle_l4)

# Compute elasticities
cola.can = cola.can %>%
  mutate(own_elasticity_carbbev = model_can_carbbev$coefficients['price'] * price * (1-share_carbbev),
         own_elasticity_cola = model_can_cola$coefficients['price'] * price * (1-share_cola),
         own_elasticity_l4 = model_can_cola$coefficients['price'] * price * (1-share_l4))

cola.bottle = cola.bottle %>%
  mutate(own_elasticity_carbbev = model_bottle_carbbev$coefficients['price'] * price * (1-share_carbbev),
         own_elasticity_cola = model_bottle_cola$coefficients['price'] * price * (1-share_cola),
         own_elasticity_l4 = model_bottle_cola$coefficients['price'] * price * (1-share_l4))
