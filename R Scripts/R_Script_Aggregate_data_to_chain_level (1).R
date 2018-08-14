

# Aggregate Cola Data

data.cola<-read.csv("cola_amm_boston.csv")
#data.cola <- subset(data.cola, data.cola$PACKAGE=="BOTTLE")
#data.cola <- subset(data.cola, data.cola$store_type=="GROC")
data.cola <- subset(data.cola, data.cola$CHAIN==33)
data.cola$yearfactor<-as.factor(data.cola$year)
data.cola$iri_keyfactor<-as.factor(data.cola$iri_key)
data.cola$display_minor<- ifelse(data.cola$display==1,1,0)
data.cola$display_major<- ifelse(data.cola$display==2,1,0)
data.cola$feature_small<- ifelse(data.cola$feature=="C",1,0)
data.cola$feature_medium<- ifelse(data.cola$feature=="B",1,0)
data.cola$feature_large<- ifelse(data.cola$feature=="A",1,0)
data.cola$coupon<- ifelse(data.cola$feature=="A+",1,0)
data.cola$displayall<-data.cola$display_minor+data.cola$display_major
data.cola$featureall<-data.cola$feature_small+data.cola$feature_medium+data.cola$feature_large
data.cola.agg<-data.cola
data.cola.agg$price<-data.cola.agg$price*data.cola.agg$units
data.cola.agg$price_deflated<-data.cola.agg$price_deflated*data.cola.agg$units

data.cola.agg$display_minor<-data.cola.agg$display_minor*data.cola.agg$units
data.cola.agg$display_major<-data.cola.agg$display_major*data.cola.agg$units
data.cola.agg$feature_small<-data.cola.agg$feature_small*data.cola.agg$units
data.cola.agg$feature_medium<-data.cola.agg$feature_medium*data.cola.agg$units
data.cola.agg$feature_large<-data.cola.agg$feature_large*data.cola.agg$units
data.cola.agg$coupon<-data.cola.agg$coupon*data.cola.agg$units
data.cola.agg$displayall<-data.cola.agg$displayall*data.cola.agg$units
data.cola.agg$featureall<-data.cola.agg$featureall*data.cola.agg$units

units.sum<-aggregate(cbind(units, dollars) ~  MARKET+CHAIN+store_type+year+week+L4+L5+VOL_EQ+PACKAGE, 
                     data = data.cola.agg, sum, na.rm = TRUE)
tmp<- aggregate(cbind(price,price_deflated,display_minor,display_major,feature_small,feature_medium,feature_large,
                      coupon,displayall,featureall) ~ MARKET+CHAIN+store_type+year+week+L4+L5+VOL_EQ+PACKAGE, data = data.cola.agg, sum, na.rm = TRUE)

data.cola.agg <- merge(units.sum,tmp,by=c("MARKET","CHAIN","store_type","year","week","L4","L5","VOL_EQ","PACKAGE")) 
data.cola.agg$price<-data.cola.agg$price/data.cola.agg$units

data.cola.agg$price_deflated<-data.cola.agg$price_deflated/data.cola.agg$units


data.cola.agg$display_minor<-data.cola.agg$display_minor/data.cola.agg$units
data.cola.agg$display_major<-data.cola.agg$display_major/data.cola.agg$units
data.cola.agg$feature_small<-data.cola.agg$feature_small/data.cola.agg$units
data.cola.agg$feature_medium<-data.cola.agg$feature_medium/data.cola.agg$units
data.cola.agg$feature_large<-data.cola.agg$feature_large/data.cola.agg$units
data.cola.agg$coupon<-data.cola.agg$coupon/data.cola.agg$units
data.cola.agg$displayall<-data.cola.agg$displayall/data.cola.agg$units
data.cola.agg$featureall<-data.cola.agg$featureall/data.cola.agg$units

head(data.cola.agg,20)

