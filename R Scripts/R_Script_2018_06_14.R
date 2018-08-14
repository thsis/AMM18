setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")
library(psych)
library(plyr)
library(AER)
# Aggregate Cola Data

data.cola<-read.csv("cola_amm_boston.csv")
data.cola <- subset(data.cola, data.cola$PACKAGE=="CAN")
data.cola <- subset(data.cola, data.cola$store_type=="GROC")
data.cola <- subset(data.cola, data.cola$CHAIN==65)

head(data.cola,10)
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


t1<-lm(log(share1)-log(shareog1)~-1+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
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


# calculation of elasticities
sigma<-nt1$coefficients['log(withinshare)']
alpha.nl<--nt1$coefficients['price']


tmp<-aggregate(cbind(share1,withinshare,price) ~ MARKET+CHAIN+store_type+L4+L5, data = data.cola.agg, mean, na.rm = TRUE)
e.nl<--1/(1-sigma)*alpha.nl*tmp[1,8]*(1-sigma*tmp[1,7]-(1-sigma)*tmp[1,6])
e.nl

tmp<--1/(1-sigma)*alpha.nl*data.cola.agg$price*(1-sigma*data.cola.agg$withinshare-(1-sigma)*data.cola.agg$share1)

data.cola.agg$directela<-tmp
head(data.cola.agg,10)
summary(data.cola.agg$directela)
alpha<--t1$coefficients['price']

aggregate(cbind(directela) ~ MARKET+CHAIN+store_type+year+L4+L5, data = data.cola.agg, mean, na.rm = TRUE)
alpha
sigma
alpha
tmp<-aggregate(cbind(share1,withinshare,price) ~ MARKET+CHAIN+store_type+L4+L5, data = data.cola.agg, mean, na.rm = TRUE)
e.l<--alpha*tmp[1,8]*(1-tmp[1,6])
e.l


# check elasticities from nl wiuth large market size
nt4<-lm(log(share4)-log(shareog4)~-1+log(withinshare)+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
summary(nt4)
sigma<-nt4$coefficients['log(withinshare)']
alpha.nl<--nt4$coefficients['price']
tmp<--1/(1-sigma)*alpha.nl*data.cola.agg$price*(1-sigma*data.cola.agg$withinshare-(1-sigma)*data.cola.agg$share4)
data.cola.agg$directela4<-tmp


# check elasticities from nl with "no outside good"
nt3<-lm(log(share3)-log(shareog3)~-1+log(withinshare)+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
summary(nt3)
sigma<-nt3$coefficients['log(withinshare)']
alpha.nl<--nt3$coefficients['price']
tmp<--1/(1-sigma)*alpha.nl*data.cola.agg$price*(1-sigma*data.cola.agg$withinshare-(1-sigma)*data.cola.agg$share3)
data.cola.agg$directela3<-tmp

aggregate(cbind(directela3,directela, directela4) ~ MARKET+CHAIN+store_type+L4+L5, data = data.cola.agg, mean, na.rm = TRUE)



# cross price elaticities
tmp<-1/(1-sigma)*alpha.nl*data.cola.agg$price*(sigma*data.cola.agg$withinshare+(1-sigma)*data.cola.agg$share3)
data.cola.agg$crossela3<-tmp

aggregate(cbind(directela3,crossela3) ~ MARKET+CHAIN+store_type+L4+L5, data = data.cola.agg, mean, na.rm = TRUE)




# Calculation of OG for 2-level nested logit 
# Choice brand or non-cola
sumunits.L4<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+year+week+L4, data = data.cola.agg, sum, na.rm = TRUE)
sumunits.L4<-rename(sumunits.L4, c("units"="sumunits.L4", "dollars"="sumdollars.L4"))
data.cola.agg <- merge(data.cola.agg,sumunits.L4,by=c("MARKET","CHAIN","store_type","year","week","L4")) 
data.cola.agg$share.L4<-data.cola.agg$units/data.cola.agg$sumunits.L4
summary(data.cola.agg)
head(data.cola.agg,10)


nt2<-lm(log(share2)-log(shareog2)~-1+log(share.L4)+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
summary(nt2)

# Calculation of OG for 2-level nested logit 
# Choice sugar/aspertham or non-cola
data.cola.agg$b1<-ifelse(data.cola.agg$L5=="COKE CLASSIC",1,0)
data.cola.agg$b2<-ifelse(data.cola.agg$L5=="DIET COKE",1,0)
data.cola.agg$b3<-ifelse(data.cola.agg$L5=="PEPSI",1,0)
data.cola.agg$b4<-ifelse(data.cola.agg$L5=="DIET PEPSI",1,0)
data.cola.agg$diet<-data.cola.agg$b2+data.cola.agg$b4
sumunits.diet<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+year+week+diet, data = data.cola.agg, sum, na.rm = TRUE)
sumunits.diet<-rename(sumunits.diet, c("units"="sumunits.diet", "dollars"="sumdollars.diet"))
data.cola.agg <- merge(data.cola.agg,sumunits.diet,by=c("MARKET","CHAIN","store_type","year","week","diet")) 
data.cola.agg$share.diet<-data.cola.agg$units/data.cola.agg$sumunits.diet
summary(data.cola.agg)
head(data.cola.agg,10)



nt2<-lm(log(share2)-log(shareog2)~-1+log(share.diet)+L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
summary(nt2)

sigma<-nt2$coefficients['log(share.diet)']
sigma
alpha.nl<--nt2$coefficients['price']
alpha.nl
head(data.cola.agg)
tmp<--1/(1-sigma)*alpha.nl*data.cola.agg$price*(1-sigma*data.cola.agg$share.diet+(1-sigma)*data.cola.agg$share2)
data.cola.agg$directela.diet<-tmp

tmp<-1/(1-sigma)*alpha.nl*data.cola.agg$price*(sigma*data.cola.agg$share.diet+(1-sigma)*data.cola.agg$share2)
data.cola.agg$crossela.diet<-tmp


tmp<-alpha.nl*data.cola.agg$price*data.cola.agg$share2
data.cola.agg$crossela2.diet<-tmp


aggregate(cbind(directela.diet,crossela.diet,crossela2.diet) ~ MARKET+CHAIN+store_type+L4+L5, data = data.cola.agg, mean, na.rm = TRUE)


