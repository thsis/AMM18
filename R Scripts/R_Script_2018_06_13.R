setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")
library(psych)
library(plyr)
library(AER)
# Aggregate Cola Data

data.cola<-read.csv("data/cola_amm_boston.csv")
data.cola <- subset(data.cola, data.cola$PACKAGE=="CAN")
data.cola <- subset(data.cola, data.cola$store_type=="GROC")
data.cola <- subset(data.cola, data.cola$CHAIN==65)

head(data.cola[, c("year", "week", "L5", "PACKAGE", "units")],10)
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
head(units.sum[, c("year", "week", "L5", "PACKAGE", "units")],10)
tmp<- aggregate(cbind(price,price_deflated,display_minor,display_major,feature_small,feature_medium,feature_large,
                      coupon,displayall,featureall) ~ MARKET+CHAIN+store_type+year+week+L4+L5+VOL_EQ+PACKAGE, data = data.cola.agg, sum, na.rm = TRUE)
dim(tmp)
temp<-aggregate(cbind(total_vol_carbbev,total_rev_carbbev,total_vol_cola, total_rev_cola,total_vol_l4,
                      total_rev_l4) ~ MARKET+CHAIN+store_type+iri_key+year+week, data = data.cola.agg, mean, na.rm = TRUE)
dim(temp)
total<-aggregate(cbind(total_vol_carbbev,total_rev_carbbev,total_vol_cola, total_rev_cola,total_vol_l4,
                       total_rev_l4) ~ MARKET+CHAIN+store_type+year+week, data = temp, sum, na.rm = TRUE)
dim(total)
data.cola.agg <- merge(units.sum,tmp,by=c("MARKET","CHAIN","store_type","year","week","L4","L5","VOL_EQ","PACKAGE"))
data.cola.agg <- merge(data.cola.agg,total,by=c("MARKET","CHAIN","store_type","year","week"))
dim(data.cola.agg)
# Edw paizei malakia!
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



data.cola.agg$yearfactor<-as.factor(data.cola.agg$year)
data.cola.agg$thanksgiving<- ifelse(data.cola.agg$week==1160,1 ,
                                    ifelse(data.cola.agg$week==1213,1,
                                           ifelse(data.cola.agg$week==1265,1,
                                                  ifelse(data.cola.agg$week==1317,1,
                                                         ifelse(data.cola.agg$week==1369,1,
                                                                ifelse(data.cola.agg$week==1421,1,
                                                                       ifelse(data.cola.agg$week==1473,1,
                                                                              ifelse(data.cola.agg$week==1526,1,
                                                                                     ifelse(data.cola.agg$week==1578,1,
                                                                                            ifelse(data.cola.agg$week==1630,1,
                                                                                                   ifelse(data.cola.agg$week==1682,1,
                                                                                                          0)))))))))))



data.cola.agg$christmas   <- ifelse(data.cola.agg$week==1165,1,
                                    ifelse(data.cola.agg$week==1217,1,
                                           ifelse(data.cola.agg$week==1269,1,
                                                  ifelse(data.cola.agg$week==1321,1,
                                                         ifelse(data.cola.agg$week==1373,1,
                                                                ifelse(data.cola.agg$week==1425,1,
                                                                       ifelse(data.cola.agg$week==1478,1,
                                                                              ifelse(data.cola.agg$week==1530,1,
                                                                                     ifelse(data.cola.agg$week==1582,1,
                                                                                            ifelse(data.cola.agg$week==1634,1,
                                                                                                   ifelse(data.cola.agg$week==1686,1,
                                                                                                          0)))))))))))




data.cola.agg$newyearseve<-  ifelse(data.cola.agg$week==1166,1 ,
                                    ifelse(data.cola.agg$week==1218,1,
                                           ifelse(data.cola.agg$week==1270,1,
                                                  ifelse(data.cola.agg$week==1322,1,
                                                         ifelse(data.cola.agg$week==1374,1,
                                                                ifelse(data.cola.agg$week==1426,1,
                                                                       ifelse(data.cola.agg$week==1479,1,
                                                                              ifelse(data.cola.agg$week==1531,1,
                                                                                     ifelse(data.cola.agg$week==1583,1,
                                                                                            ifelse(data.cola.agg$week==1635,1,
                                                                                                   ifelse(data.cola.agg$week==1687,1,
                                                                                                          0)))))))))))







data_vl<-data.cola.agg

data.cola.agg<-data_vl
head(data.cola.agg,10)

summary(lm(log(units)~-1+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg))




summary(data.cola.agg)


# Calculation of OG
sumunits<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+year+week, data = data.cola.agg, sum, na.rm = TRUE)
head(sumunits)
sumunits<-rename(sumunits, c("units"="sumunits", "dollars"="sumdollars"))
data.cola.agg <- merge(data.cola.agg,sumunits,by=c("MARKET","CHAIN","store_type","year","week"))

data.cola.agg$share1<-data.cola.agg$units/data.cola.agg$sumunits*(data.cola.agg$sumdollars/data.cola.agg$total_rev_cola)
data.cola.agg$share2<-data.cola.agg$units/data.cola.agg$sumunits*(data.cola.agg$sumdollars/data.cola.agg$total_rev_carbbev)
data.cola.agg$share3<-data.cola.agg$units/(data.cola.agg$sumunits+1)
data.cola.agg$share4<-data.cola.agg$units/data.cola.agg$sumunits*(data.cola.agg$sumdollars/(data.cola.agg$total_rev_carbbev*4))
data.cola.agg$share5<-data.cola.agg$units/data.cola.agg$sumunits*(data.cola.agg$sumdollars/(5*25000))

head(data.cola.agg)
withinshare<-aggregate(cbind(share1, share2,share3,share4,share5) ~ MARKET+CHAIN+store_type+year+week, data = data.cola.agg, sum, na.rm = TRUE)
withinshare<-rename(withinshare, c("share1"="shareog1", "share2"="shareog2", "share3"="shareog3", "share4"="shareog4", "share5"="shareog5"))
data.cola.agg <- merge(data.cola.agg,withinshare,by=c("MARKET","CHAIN","store_type","year","week"))
data.cola.agg$shareog1<-(1-data.cola.agg$shareog1)
data.cola.agg$shareog2<-(1-data.cola.agg$shareog2)
data.cola.agg$shareog3<-(1-data.cola.agg$shareog3)
data.cola.agg$shareog4<-(1-data.cola.agg$shareog4)
data.cola.agg$shareog5<-(1-data.cola.agg$shareog5)


t5<-lm(log(share5)-log(shareog5)~-1+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
summary(t1)
summary(t2)
summary(t3)
summary(t4)
summary(t5)


# 2-Level nested logit
data.cola.agg$withinshare<-data.cola.agg$units/data.cola.agg$sumunits

t1<-lm(log(share1)-log(shareog1)~-1+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
nt1<-lm(log(share1)-log(shareog1)~-1+log(withinshare)+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
summary(t1)
summary(nt1)
