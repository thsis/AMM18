library("dplyr")
library("tidyr")

data = read.csv("data/cola_amm_boston.csv", stringsAsFactors = FALSE)

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
         week = factor(week)) %>% 
  group_by(year, week, L5, PACKAGE) %>% 
  summarise(
    units = sum(units),
    dollars = sum(dollars),
    price = mean(price),
    feature = mean(ifelse(feature == "NONE", 0, 1)),
    display = mean(display),
    total_vol_carbbev = sum(total_vol_carbbev),
    total_rev_carbbev = sum(total_rev_carbbev),
    total_vol_cola = sum(total_vol_cola),
    total_rev_cola = sum(total_rev_cola),
    total_vol_l4 = sum(total_vol_l4),
    total_rev_l4 = sum(total_rev_l4)) %>%
  mutate(christmas = week %in% christmas_filter,
         newyearseve = week %in% newyearseve_filter,
         superbowl = week %in% superbowl_filter,
         july4th = week %in% july4th_filter,
         thanksgiving = week %in% thanksgiving_filter,
         share_carbbev = dollars / total_rev_carbbev,
         share_cola = dollars / total_rev_cola,
         outside_carbbev = 1 - share_carbbev,
         outside_cola = 1 - share_cola) %>% 
  group_by(year, L5, PACKAGE) %>% 
  arrange(week) %>% 
  mutate(lagged_units = lag(units),
         lagged_price = lag(price))

get_share = function(var){
  df = cola %>% 
    select_("year", "week", "L5", "PACKAGE", var) %>% 
    spread(L5, var)
  prods = c("_coke", "_diet_coke", "_diet_pepsi", "_pepsi")
  columns = c("year", "week", "PACKAGE", paste0(var, prods))
  colnames(df) = columns
  return(df)
}

cola.price = get_share("price")
cola.share_carbbev = get_share("share_carbbev")
cola.share_cola = get_share("share_cola")

cola = merge(cola, cola.price)
cola = merge(cola, cola.share_carbbev)
cola = merge(cola, cola.share_cola)

# Construct model formulas
formulas = c("carbbev", "cola")
model_vars = "price + lagged_price + lagged_units + feature + display + christmas + superbowl + july4th + thanksgiving"
formulas = paste0("log(share_", formulas, ")-log(outside_", formulas, ") ~ ", model_vars)

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

## BOTTLES
model_bottle_carbbev = lm(formulas[1], data = cola.bottle)
summary(model_bottle_carbbev)
plot(model_bottle_carbbev)

model_bottle_cola = lm(formulas[2], data = cola.bottle)
summary(model_bottle_cola)
plot(model_bottle_cola)

# Compute elasticities
cola.can = cola.can %>%
  mutate(own_elasticity_cola = -abs(model_can_cola$coefficients['price']) * price * (1-share_cola),
         coke_elasticity_cola = -abs(model_can_cola$coefficients['price'] * price_coke * share_cola_coke),
         diet_coke_elasticity_cola = -abs(model_can_cola$coefficients['price'] * price_diet_coke * share_cola_diet_coke),
         pepsi_elasticity_cola = -abs(model_can_cola$coefficients['price'] * price_pepsi * share_cola_pepsi),
         diet_pepsi_elasticity_cola = -abs(model_can_cola$coefficients['price'] * price_diet_pepsi * share_cola_diet_pepsi))

cola.bottle = cola.bottle %>%
  mutate(own_elasticity_cola = -abs(model_bottle_cola$coefficients['price']) * price * (1-share_cola),
         coke_elasticity_cola = -abs(model_bottle_cola$coefficients['price'] * price_coke * share_cola_coke),
         diet_coke_elasticity_cola = -abs(model_bottle_cola$coefficients['price'] * price_diet_coke * share_cola_diet_coke),
         pepsi_elasticity_cola = -abs(model_bottle_cola$coefficients['price'] * price_pepsi * share_cola_pepsi),
         diet_pepsi_elasticity_cola = -abs(model_bottle_cola$coefficients['price'] * price_diet_pepsi * share_cola_diet_pepsi))

elasticities.can = cola.can %>% 
  select(L5, own_elasticity_cola:diet_pepsi_elasticity_cola) %>% 
  group_by(L5) %>%
  summarise(own_el = median(own_elasticity_cola),
            diet_coke_el = median(diet_coke_elasticity_cola),
            coke_el = median(coke_elasticity_cola),
            pepsi_el = median(pepsi_elasticity_cola),
            diet_pepsi_el = median(diet_pepsi_elasticity_cola))

elasticities.bottle = cola.bottle %>% 
  select(L5, own_elasticity_cola:diet_pepsi_elasticity_cola) %>% 
  group_by(L5) %>%
  summarise(own_el = median(own_elasticity_cola),
            diet_coke_el = median(diet_coke_elasticity_cola),
            coke_el = median(coke_elasticity_cola),
            pepsi_el = median(pepsi_elasticity_cola),
            diet_pepsi_el = median(diet_pepsi_elasticity_cola))


extract_elasticities = function(eldf){
  res = diag(eldf$own_el)
  res[1, 2] <- res[2, 1] <- as.double(eldf[1, 3])
  res[1, 3] <- res[3, 1] <- as.double(eldf[1, 5])
  res[1, 4] <- res[4, 1] <- as.double(eldf[1, 4])
  res[2, 3] <- res[3, 2] <- as.double(eldf[2, 6])
  res[4, 2] <- res[2, 4] <- as.double(eldf[4, 3])
  res[4, 3] <- res[3, 4] <- as.double(eldf[4, 6])
  
  res[upper.tri(res)] <- NA
  
  colnames(res) <- rownames(res) <- eldf$L5
  
  return(round(res,2))
}

extract_elasticities(elasticities.bottle)
extract_elasticities(elasticities.can)

################################################################################
# 3 b: compute within shares
# Model: 1st Choice Diet vs Regular, 2nd Choice Coke vs Pepsi
cola.type = cola %>%
  mutate(type = ifelse(L5 %in% c("COKE CLASSIC", "PEPSI"), "REGULAR", "DIET")) %>% 
  group_by(type) %>% 
  summarise(type_total_units = sum(units),
            type_total_revenue = sum(dollars))

cola.pkg = cola %>%
  group_by(PACKAGE) %>% 
  summarise(pkg_total_units = sum(units),
            pkg_total_revenue = sum(dollars))

cola.brand = cola %>%
  mutate(brand = ifelse(L5 %in% c("COKE CLASSIC", "DIET COKE"), "COKE", "PEPSI")) %>% 
  group_by(brand) %>% 
  summarise(brand_total_units = sum(units),
            brand_total_revenue = sum(dollars))

cola = cola %>% 
  mutate(type = ifelse(L5 %in% c("COKE CLASSIC", "PEPSI"), "REGULAR", "DIET"),
         brand = ifelse(L5 %in% c("COKE CLASSIC", "DIET COKE"), "COKE", "PEPSI"))

cola = merge(cola, cola.brand)
cola = merge(cola, cola.pkg)
cola = merge(cola, cola.type)

cola.can = cola %>% filter(PACKAGE == "CAN") %>% 
  mutate(w_share_brand_units = units / brand_total_units,
         w_share_brand_revenue = dollars / brand_total_revenue,
         w_share_pkg_units = units / pkg_total_units,
         w_share_pkg_revenue = dollars / pkg_total_revenue,
         w_share_type_units = units / type_total_units,
         w_share_type_revenue = units / type_total_revenue)

cola.bottle = cola %>% filter(PACKAGE == "BOTTLE") %>%
  mutate(w_share_brand_units = units / brand_total_units,
         w_share_brand_revenue = dollars / brand_total_revenue,
         w_share_pkg_units = units / pkg_total_units,
         w_share_pkg_revenue = dollars / pkg_total_revenue,
         w_share_type_units = units / type_total_units,
         w_share_type_revenue = units / type_total_revenue)


model_vars = "price + feature + display + christmas + superbowl + july4th + thanksgiving"
formulas = c("brand", "pkg", "type")
formulas = paste0("log(share_cola) - log(outside_cola) ~ ",
                  model_vars, " + log(w_share_", formulas, "_units) - 1")

# CANS
cans_nested_logit_brand = lm(formulas[1], data = cola.can)
summary(cans_nested_logit_brand)

cans_nested_logit_pkg = lm(formulas[2], data = cola.can)
summary(cans_nested_logit_pkg)

cans_nested_logit_type = lm(formulas[3], data = cola.can)
summary(cans_nested_logit_type)

# BOTTLES
bottles_nested_logit_brand = lm(formulas[1], data = cola.bottle)
summary(bottles_nested_logit_brand)

bottles_nested_logit_pkg = lm(formulas[2], data = cola.bottle)
summary(bottles_nested_logit_pkg)

bottles_nested_logit_type = lm(formulas[3], data = cola.bottle)
summary(bottles_nested_logit_type)

# Bullshit residuals:
hist(cans_nested_logit_brand$residuals)

# Elasticities
