setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")
# Aggregate Cola Data

data.cola<-read.csv("cola_amm_boston.csv")
data.cola <- subset(data.cola, data.cola$PACKAGE=="BOTTLE")
data.cola <- subset(data.cola, data.cola$CHAIN==65)

data.cola$units<-data.cola$units*data.cola$VOL_EQ
data.cola$price<-data.cola$price/data.cola$VOL_EQ

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




data.cola.agg1 <- subset(data.cola.agg, data.cola.agg$year<20066)
data.cola.agg <- subset(data.cola.agg1, data.cola.agg1$year>2009)



# Calculation of OG 
sumunits<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+year+week, data = data.cola.agg, sum, na.rm = TRUE)
head(sumunits)
sumunits<-rename(sumunits, c("units"="sumunits", "dollars"="sumdollars"))
data.cola.agg <- merge(data.cola.agg,sumunits,by=c("MARKET","CHAIN","store_type","year","week")) 

data.cola.agg$share1<-data.cola.agg$units/data.cola.agg$sumunits*(data.cola.agg$sumdollars/data.cola.agg$total_rev_cola)

withinshare<-aggregate(cbind(share1) ~ MARKET+CHAIN+store_type+year+week, data = data.cola.agg, sum, na.rm = TRUE)
withinshare<-rename(withinshare, c("share1"="shareog1"))
data.cola.agg <- merge(data.cola.agg,withinshare,by=c("MARKET","CHAIN","store_type","year","week")) 
data.cola.agg$shareog1<-(1-data.cola.agg$shareog1)


t1<-lm(log(share1)-log(shareog1)~-1+L5+yearfactor+(price)+displayall+featureall,data=data.cola.agg)
summary(t1)

data.cola.agg$CC<-ifelse(data.cola.agg$L5=="COKE CLASSIC",1,0)
data.cola.agg$DC<-ifelse(data.cola.agg$L5=="DIET COKE",1,0)
data.cola.agg$P<-ifelse(data.cola.agg$L5=="PEPSI",1,0)
data.cola.agg$DP<-ifelse(data.cola.agg$L5=="DIET PEPSI",1,0)
data.cola.agg$y2011<-ifelse(data.cola.agg$year==2011,1,0)

data.cola.agg$DC.price<-data.cola.agg$price*.2*data.cola.agg$DC
data.cola.agg$price.sim<-data.cola.agg$price-data.cola.agg$DC.price
est<-t1$coefficients

est<-rename(est, c("L5COKE CLASSIC"="b1", "L5DIET COKE"="b2","L5DIET PEPSI"="b3","L5PEPSI"="b4",
                   "yearfactor2011"="beta0","price"="alpha","displayall"="beta1","featureall"="beta2"))
est<-as.data.frame(est)
str(est)
est['b1']           
d<-data.cola.agg
head(d)
# pred shares at observed prices
d$delta<-d$CC*est['b1']+d$DC*est['b2']+d$P*est['b4']+d$DP*est['b3']+d$y2011*est['beta0']+d$displayall*est['beta1']+d$featureall*est['beta2']+d$price*est['alpha']
d$expdelta<-exp(d$delta)
head(d)
tmp<-aggregate(cbind(expdelta) ~ MARKET+CHAIN+store_type+week, data = d, sum, na.rm = TRUE)
tmp<-rename(tmp, c("expdelta"="sumexpdelta"))

head(tmp)
d <- merge(d,tmp,by=c("MARKET","CHAIN","store_type","week")) 
head(d)
d$predshare<-d$expdelta/(1+d$sumexpdelta)

#pred shre a0 DC  price reduction 20 percent


d$DC.price<-d$price*.2*d$DC
d$price.sim<-d$price-dDC.price

d$delta.DC.8<-d$CC*est['b1']+d$DC*est['b2']+d$P*est['b4']+d$DP*est['b3']+d$y2011*est['beta0']+d$displayall*est['beta1']+d$featureall*est['beta2']+d$price.sim*est['alpha']
d$expdelta.DC.8<-exp(d$delta.DC.8)
tmp<-aggregate(cbind(expdelta.DC.8) ~ MARKET+CHAIN+store_type+week, data = d, sum, na.rm = TRUE)
tmp<-rename(tmp, c("expdelta.DC.8"="sumexpdelta.DC.8"))

d <- merge(d,tmp,by=c("MARKET","CHAIN","store_type","week")) 
head(d)
d$predshare.DC.8<-d$expdelta.DC.8/(1+d$sumexpdelta.DC.8)





tmp<-aggregate(cbind(predshare,predshare.DC.8) ~ MARKET+CHAIN+store_type+L4+L5, data = d, mean, na.rm = TRUE)
tmp$r<-tmp$predshare.DC.8/tmp$predshare
head(tmp)



3.66/1.29*.438


summary(data.cola.agg)

t2<-lm(log(share1)-log(shareog1)~L5+yearfactor+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data.cola.agg)
summary(t2)
r<-t2$residuals
y<-log(data.cola.agg$share1)-log(data.cola.agg$shareog1)
d<-y-mean(y)
d<-crossprod(d)
n<-crossprod(r)
1-n/d
