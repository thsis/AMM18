library("dplyr")
library("reshape")
library("stargazer")

data = read.csv("data/cola_amm_boston.csv")

# Prepare containers for later variable definition.
informative_columns = c('year', 'week', 'L5', 'VOL_EQ', 'PACKAGE',
                        'units', 'dollars', 'price', 'feature',
                        'display', 'total_vol_cola', 'total_rev_carbbev')

superbowl_filter = c(1117, 1170, 1217, 1274, 1327, 1379, 1431, 1483, 1535, 1588, 1640)
christmas_filter = c(1165, 1217, 1269, 1321, 1373, 1425, 1478, 1530, 1582, 1634, 1687)
newyearseve_filter = c(1166, 1218, 1270, 1322, 1374, 1426, 1479, 1531, 1583, 1635, 1687)
july4th_filter = c(1139, 1191, 1243, 1295, 1347, 1399, 1451, 1503, 1555, 1607, 1659)
thanksgiving_filter = c(1160, 1213, 1265, 1317, 1369, 1421, 1473, 1526, 1578, 1630, 1682)

# Subset data.
data = data %>% 
  filter(year %in% 2010:2011 & CHAIN == 65) %>% 
  select(informative_columns) %>% 
  mutate(year = factor(year),
         week = factor(week),
         type = if_else(
           L5 %in% c("DIET COKE", "DIET PEPSI"),
           "DIET", 
           "REGULAR"),
         type = factor(type),
         liters = units * VOL_EQ,
         price_per_liter = price / VOL_EQ,
         display_all = factor(display > 0),
         feature_all = factor(ifelse(feature == "NONE", FALSE, TRUE)),
         superbowl = week %in% superbowl_filter,
         christmas = week %in% christmas_filter,
         july4th = week %in% july4th_filter,
         thanksgiving = week %in% thanksgiving_filter) %>% 
  group_by(L5, PACKAGE) %>% 
  arrange(year, week) %>% 
  # Fuck that shit.
  mutate(lagged_liters = lag(liters),
         lagged_liter_price = lag(price_per_liter))

# Prepare features, calculate shares, define outside good as carbonated-beverages.
cola = data %>%
  group_by(year, week, PACKAGE) %>% 
  summarise(total_liters = sum(liters),
            total_revenue = sum(dollars)) %>% 
  right_join(data, by = c("year", "week", "PACKAGE")) %>%
  mutate(share = (liters / total_liters) * (total_revenue / total_rev_carbbev))

cola = cola %>% 
  group_by(year, week, PACKAGE) %>% 
  summarise(accounted_share = sum(share)) %>% 
  mutate(share_og = 1 - accounted_share) %>%
  right_join(cola, by = c("year", "week", "PACKAGE")) %>% 
  select(c(colnames(cola), 'share_og'))

cola = cola %>% 
  group_by(year, week, PACKAGE, type) %>% 
  summarise(total_type = sum(liters)) %>% 
  right_join(cola, by = c("year", "week", "PACKAGE", "type")) %>% 
  mutate(within_share = liters / total_type) %>% 
  select(c(colnames(cola), "within_share"))
  
# Aggregated Logit Model
formula = 'log(share)-log(share_og) ~ -1+L5+year+price_per_liter+display_all+feature_all+christmas'

agg_log_bottle = lm(formula = formula,
                    data = cola,
                    subset = cola$PACKAGE == "BOTTLE")
agg_log_can = lm(formula = formula,
                 data = cola,
                 subset = cola$PACKAGE == "CAN")

summary(agg_log_bottle)
summary(agg_log_can)

# Compute own elasticities:
own_el_agg = function(data, model_can, model_bottle){
  alpha = switch (data["PACKAGE"],
    "BOTTLE" = model_bottle$coefficients["price_per_liter"],
    "CAN" = model_can$coefficients["price_per_liter"])
  res = - abs(alpha) * as.numeric(data["price_per_liter"]) * (1 - as.numeric(data["share"]))
}

# Compute cross elasticities:
cross_el_agg = function(data, reference, model_can, model_bottle){
  alpha = switch (data["PACKAGE"],
    "BOTTLE" = model_bottle$coefficients["price_per_liter"],
    "CAN" = model_can$coefficients["price_per_liter"])
  res = abs(alpha) * as.numeric(data["price_per_liter"]) * as.numeric(data["share"])
  return(res)
}

cola$agg_own_ela = apply(X = cola, 
                         MARGIN = 1, 
                         FUN = own_el_agg, 
                         model_can=agg_log_can, 
                         model_bottle=agg_log_bottle)
cola$agg_cross_ela = apply(X = cola,
                           MARGIN = 1,
                           FUN = cross_el_agg,
                           model_can = agg_log_can,
                           model_bottle=agg_log_bottle)

get_agg_el_table = function(data, pkg){
  elasticities.agg = data %>%
    filter(PACKAGE == pkg) %>% 
    group_by(L5) %>%
    summarise(own.el = mean(agg_own_ela),
              cross.el = mean(agg_cross_ela))

  own = unlist(elasticities.agg[, "own.el"])
  cross = unlist(elasticities.agg[, "cross.el"])
  
  out = matrix(cross, 4, 4)
  
  diag(out) = own

  rownames(out) = colnames(out) = levels(data$L5)
  return(round(out, 2))
}

(mean.agg.el.bottle = get_agg_el_table(cola, "BOTTLE"))
(mean.agg.el.can = get_agg_el_table(cola, "CAN"))

# Nested Logit Model
# First choice
formula = 'log(share)-log(share_og) ~ -1+L5+log(within_share)+year+price_per_liter+display_all+feature_all+christmas+lagged_liters+lagged_liter_price'
nested_log_bottle = lm(formula = formula,
                       data = cola,
                       subset = cola$PACKAGE == "BOTTLE")
nested_log_can = lm(formula = formula,
                    data = cola,
                    subset = cola$PACKAGE == "CAN")

summary(nested_log_bottle)
summary(nested_log_can)


own_el_nest = function(data, model_can, model_bottle){
  data = unlist(data)
  alpha = switch (data["PACKAGE"],
                  "BOTTLE" = model_bottle$coefficients["price_per_liter"],
                  "CAN" = model_can$coefficients["price_per_liter"] )
  sigma = switch(data["PACKAGE"],
                 "BOTTLE" = model_bottle$coefficients["log(within_share)"],
                 "CAN" = model_can$coefficients["log(within_share)"])
  
  sigma_ = 1 - sigma
  
  withinshare = as.numeric(data["within_share"])
  share = as.numeric(data["share"])
  price = as.numeric(data["price_per_liter"])
  
  res = - 1/sigma_ * (1 - sigma* withinshare - sigma_* share) * abs(alpha) * price
  
  return(res)
}

cross_el_nest = function(data, model_can, model_bottle, in_group = TRUE){
  data = unlist(data)
  alpha = switch (data["PACKAGE"],
    "BOTTLE" = model_bottle$coefficients["price_per_liter"],
    "CAN" = model_can$coefficients["price_per_liter"]
  )
  sigma = switch(data["PACKAGE"],
                 "BOTTLE" = model_bottle$coefficients["log(within_share)"],
                 "CAN" = model_can$coefficients["log(within_share)"])
  sigma_ = 1 - sigma
  withinshare = as.numeric(data["within_share"])
  share = as.numeric(data["share"])
  price = as.numeric(data["price_per_liter"])

  if (in_group) {
    res = 1/sigma_ * (sigma * withinshare + sigma_ * share) * abs(alpha) * price
  } else {
    res = abs(alpha) * price * share
  }
  return(res)
}

cola$nest_own_ela = apply(X = cola, 
                         MARGIN = 1, 
                         FUN = own_el_nest, 
                         model_can=nested_log_can, 
                         model_bottle=nested_log_bottle)
cola$cross_ingroup = apply(X = cola,
                           MARGIN = 1,
                           FUN = cross_el_nest,
                           model_can = nested_log_can,
                           model_bottle=nested_log_bottle)
cola$cross_outgroup = apply(X = cola,
                            MARGIN = 1,
                            FUN = cross_el_nest,
                            model_can = nested_log_can,
                            model_bottle = nested_log_bottle,
                            in_group = FALSE)

get_nest_el_table = function(data, pkg){
  elasticities.nest = data %>%
    filter(PACKAGE == pkg) %>% 
    group_by(L5) %>%
    summarise(own.el = mean(nest_own_ela),
              cross.el.in = mean(cross_ingroup),
              cross.el.out = mean(cross_outgroup))
  
  diet = elasticities.nest$L5 %in% c("DIET COKE", "DIET PEPSI")
  regular = elasticities.nest$L5 %in% c("COKE CLASSIC", "PEPSI")
  group.diet = outer(diet, diet)
  group.reg = outer(regular, regular)
  group = group.diet | group.reg
  
  own = unlist(elasticities.nest[, "own.el"])
  cross.in = unlist(elasticities.nest[, "cross.el.in"])
  cross.out = unlist(elasticities.nest[, "cross.el.out"])
  
  out.in = matrix(cross.in, 4, 4)
  out.out = matrix(cross.out, 4, 4)
  
  out = matrix(NA, 4, 4)
  out[group] = out.in[group]
  out[!group] = out.out[!group]
  diag(out) = own
  
  rownames(out) = colnames(out) = levels(data$L5)
  return(round(out, 2))
}

(mean.nest.el.bottle = get_nest_el_table(cola, "BOTTLE"))
(mean.nest.el.can = get_nest_el_table(cola, "CAN"))


####################################################################
# Simulations

simulate.agg = function(data, model, modifier, brands = c("DIET COKE")){
  df = data %>%
      mutate(
        change_price = L5 %in% brands,
        price_per_liter = price_per_liter + change_price * price_per_liter * modifier)
  
  delta = predict(model, newdata = df)
  df$exp.delta = exp(delta)
  
  df = df %>% 
    group_by(week) %>% 
    summarise(sum.exp.delta = sum(exp.delta)) %>%
    right_join(df, by = "week") %>%
    mutate(predshare = exp.delta / (sum.exp.delta + 1))
  
  return(df$predshare)
}

cola.bottle = cola[cola$PACKAGE == "BOTTLE", ]
cola.can = cola[cola$PACKAGE == "CAN", ]

cola.bottle$agg.predshare.plus20 = simulate.agg(cola.bottle, agg_log_bottle, 0.2)
cola.bottle$agg.predshare.minus20 = simulate.agg(cola.bottle, agg_log_bottle, -0.2)

cola.can$agg.predshare.plus20 = simulate.agg(cola.can, agg_log_can, 0.2)
cola.can$agg.predshare.minus20 = simulate.agg(cola.can, agg_log_can, -0.2)
# Ti kanei o poustis;
# 
# d$delta.ALL.8<-d$CC*est['b1']+d$DC*est['b2']+d$P*est['b4']+d$DP*est['b3']+d$y2011*est['beta0']+d$displayall*est['beta1']+d$featureall*est['beta2']+d$price.sim*est['alpha']
# d$expdelta.ALL.8<-exp(d$delta.ALL.8)
# tmp<-aggregate(cbind(expdelta.ALL.8) ~ MARKET+CHAIN+store_type+week, data = d, sum, na.rm = TRUE)
# tmp<-plyr::rename(tmp, c("expdelta.ALL.8"="sumexpdelta.ALL.8"))
# 
# d <- merge(d,tmp,by=c("MARKET","CHAIN","store_type","week")) 
# head(d)
# d$predshare.ALL.8<-d$expdelta.ALL.8/(1+d$sumexpdelta.ALL.8)
# 
# tmp<-aggregate(cbind(predshare,predshare.DC.8,predshare.ALL.8) ~ MARKET+CHAIN+store_type+L4+L5, data = d, mean, na.rm = TRUE)
# tmp$rall<-tmp$predshare.ALL.8/tmp$predshare
# head(tmp)

data = cola.bottle
# Ti kanei o malakas;
simulate.nest = function(data, brands, modifier, model){
  sigma = model$coefficients["log(within_share)"]
  print(sigma)
  df = data %>%
    mutate(
      change_price = L5 %in% brands,
      price_per_liter = price_per_liter + change_price * price_per_liter * modifier)
  
  delta = predict(model, newdata = df)
  df$Dg = exp(delta/(1 - sigma))
  
  df %>% 
    group_by(year, week) %>% 
    
}

# data.cola.agg$DC.price<-data.cola.agg$price*.2*data.cola.agg$DC
# data.cola.agg$price.sim<-data.cola.agg$price-data.cola.agg$DC.price
# 
# d<-data.cola.agg
# 
# d$delta<-d$CC*est['b1']+d$DC*est['b2']+d$P*est['b4']+d$DP*est['b3']+d$y2011*est['beta0']+d$displayall*est['beta1']+d$featureall*est['beta2']+d$price.sim*est['alpha']
# d$tmp<-d$delta/(1-est['sigma'])
# d$tmp<-exp(d$tmp)
# 
# tmp<-aggregate(cbind(tmp) ~ MARKET+CHAIN+store_type+diet+week, data = d, sum, na.rm = TRUE)
# tmp<-plyr::rename(tmp, c("tmp"="Dg"))
# 
# d <- merge(d,tmp,by=c("MARKET","CHAIN","store_type","diet","week")) 
# d$share.jg<-d$tmp/d$Dg
# d$nom.Dg<-d$Dg^(1-est['sigma'])
# 
# tmp<-aggregate(cbind(nom.Dg) ~ MARKET+CHAIN+store_type+week, data = d, sum, na.rm = TRUE)
# tmp<-plyr::rename(tmp, c("nom.Dg"="denom.Dg"))

# d <- merge(d,tmp,by=c("MARKET","CHAIN","store_type","week")) 

# d$denom.Dg<-d$denom.Dg/2+1
# d$share.g<-d$nom.Dg/d$denom.Dg
# d$share.nl<-d$share.jg*d$share.g
# aggregate(cbind(share.nl) ~ MARKET+CHAIN+store_type+L5, data = d, sum, na.rm = TRUE)

