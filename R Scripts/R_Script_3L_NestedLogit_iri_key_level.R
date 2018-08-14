setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")
# Aggregate Cola Data

data<-read.csv("GingerAleLemonDew_Chain139.csv")
head(data)
str(data)
#library(dplyr)
library(stringr)
#data$FLAVOR.SCENT<-str_replace(data$FLAVOR.SCENT,"CITRUS DEW","LEMON LIME")
#data$FLAVOR.SCENT<-str_replace(data$FLAVOR.SCENT,"DEW","LEMON LIME")
data$FLAVOR.SCENT<-str_replace(data$FLAVOR.SCENT,"CITRUS DEW","DEW")
#data<-subset(data,data$FLAVOR.SCENT=="LEMON LIME")
#data<-subset(data,data$FLAVOR.SCENT != "CITRUS DEW")
#data<-subset(data,data$FLAVOR.SCENT != "DEW")
str(data)

data.pl<-subset(data,data$L5 =="PRIVATE LABEL" )
data.cd<-subset(data,data$L5 =="CANADA DRY" )
str(data.pl)
data.pl.g<-subset(data.pl,data.pl$FLAVOR.SCENT =="GINGER ALE")
data.pl.l<-subset(data.pl,data.pl$FLAVOR.SCENT =="LEMON LIME")

data.pl.g.d<-subset(data.pl.g,data.pl.g$CALORIE.LEVEL =="DIET")
data.pl.g.r<-subset(data.pl.g,data.pl.g$CALORIE.LEVEL =="REGULAR")
data.pl.l.d<-subset(data.pl.l,data.pl.l$CALORIE.LEVEL =="DIET")
data.pl.l.r<-subset(data.pl.l,data.pl.l$CALORIE.LEVEL =="REGULAR")


data.cd.d<-subset(data.cd,data.cd$CALORIE.LEVEL =="DIET")
data.cd.r<-subset(data.cd,data.cd$CALORIE.LEVEL =="REGULAR")

data.pl.g.r$L5<-str_replace(data.pl.g.r$L5,"PRIVATE LABEL","PL GINGER ALE")
data.pl.g.d$L5<-str_replace(data.pl.g.d$L5,"PRIVATE LABEL","PL DIET GINGER ALE")
data.pl.l.r$L5<-str_replace(data.pl.l.r$L5,"PRIVATE LABEL","PL LEMON LIME")
data.pl.l.d$L5<-str_replace(data.pl.l.d$L5,"PRIVATE LABEL","PL DIET LEMON LIME")

data.cd.d$L5<-str_replace(data.cd.d$L5,"CANADA DRY","DIET CANADA DRY")

data<-subset(data,data$L5 !="PRIVATE LABEL")
data<-subset(data,data$L5 !="CANADA DRY")
#data<-subset(data,data$FLAVOR.SCENT !="GINGER ALE")
data<- rbind(data,data.pl.g.r,data.pl.g.d,data.pl.l.r,data.pl.l.d,data.cd.d,data.cd.r)


tmp1<- aggregate(cbind(total_rev_carbbev) ~
                   MARKET+CHAIN+store_type+iri_key+year+week+L4+L5+VOL_EQ+PACKAGE+FLAVOR.SCENT+CALORIE.LEVEL,
                 data = data, mean, na.rm = TRUE)
marketsize<- aggregate(cbind(total_rev_carbbev) ~
                   MARKET+CHAIN+store_type+iri_key+year+week,
                 data = tmp1, mean, na.rm = TRUE)

head(marketsize,10)

data$display_minor<- ifelse(data$d==1,1,0)
data$display_major<- ifelse(data$d==2,1,0)
data$feature_small<- ifelse(data$f=="C",1,0)
data$feature_medium<- ifelse(data$f=="B",1,0)
data$feature_large<- ifelse(data$f=="A",1,0)
data$coupon<- ifelse(data$f=="A+",1,0)
data$displayall<-data$display_minor+data$display_major
data$featureall<-data$feature_small+data$feature_medium+data$feature_large

data.agg<-data

data.agg$price<-data.agg$price*data.agg$units
#data.agg$price_deflated<-data.agg$price_deflated*data.agg$units

data.agg$display_minor<-data.agg$display_minor*data.agg$units
data.agg$display_major<-data.agg$display_major*data.agg$units
data.agg$feature_small<-data.agg$feature_small*data.agg$units
data.agg$feature_medium<-data.agg$feature_medium*data.agg$units
data.agg$feature_large<-data.agg$feature_large*data.agg$units
data.agg$coupon<-data.agg$coupon*data.agg$units
data.agg$displayall<-data.agg$displayall*data.agg$units
data.agg$featureall<-data.agg$featureall*data.agg$units

units.sum<-aggregate(cbind(units, dollars) ~ 
                       
                       MARKET+CHAIN+store_type+iri_key+year+week+
                       L4+L5+VOL_EQ+PACKAGE+FLAVOR.SCENT+CALORIE.LEVEL, 
                     
                     
                     data = data.agg, sum, na.rm = TRUE)

head(units.sum,10)
tmp<- aggregate(cbind(price,display_minor,display_major,feature_small,feature_medium,feature_large,
                      coupon,displayall,featureall) ~
                  MARKET+CHAIN+store_type+iri_key+year+week+L4+L5+VOL_EQ+PACKAGE+FLAVOR.SCENT+CALORIE.LEVEL,
                data = data.agg, sum, na.rm = TRUE)

data.agg <- merge(units.sum,tmp,by=c("MARKET","CHAIN","store_type","iri_key","year","week","L4","L5","VOL_EQ","PACKAGE","FLAVOR.SCENT","CALORIE.LEVEL")) 
data.agg <- merge(data.agg,marketsize,by=c("MARKET","CHAIN","store_type","iri_key","year","week")) 
head(data.agg)
data.agg$price<-data.agg$price/data.agg$units

data.agg$display_minor<-data.agg$display_minor/data.agg$units
data.agg$display_major<-data.agg$display_major/data.agg$units
data.agg$feature_small<-data.agg$feature_small/data.agg$units
data.agg$feature_medium<-data.agg$feature_medium/data.agg$units
data.agg$feature_large<-data.agg$feature_large/data.agg$units
data.agg$coupon<-data.agg$coupon/data.agg$units
data.agg$displayall<-data.agg$displayall/data.agg$units
data.agg$featureall<-data.agg$featureall/data.agg$units



data.agg$yearfactor<-as.factor(data.agg$year)
data.agg$thanksgiving<- ifelse(data.agg$week==1160,1 ,
                                    ifelse(data.agg$week==1213,1, 
                                           ifelse(data.agg$week==1265,1, 
                                                  ifelse(data.agg$week==1317,1, 
                                                         ifelse(data.agg$week==1369,1, 
                                                                ifelse(data.agg$week==1421,1, 
                                                                       ifelse(data.agg$week==1473,1, 
                                                                              ifelse(data.agg$week==1526,1, 
                                                                                     ifelse(data.agg$week==1578,1,
                                                                                            ifelse(data.agg$week==1630,1, 
                                                                                                   ifelse(data.agg$week==1682,1,
                                                                                                          0)))))))))))



data.agg$christmas   <- ifelse(data.agg$week==1165,1,
                                    ifelse(data.agg$week==1217,1, 
                                           ifelse(data.agg$week==1269,1, 
                                                  ifelse(data.agg$week==1321,1, 
                                                         ifelse(data.agg$week==1373,1, 
                                                                ifelse(data.agg$week==1425,1, 
                                                                       ifelse(data.agg$week==1478,1, 
                                                                              ifelse(data.agg$week==1530,1, 
                                                                                     ifelse(data.agg$week==1582,1,
                                                                                            ifelse(data.agg$week==1634,1, 
                                                                                                   ifelse(data.agg$week==1686,1,
                                                                                                          0)))))))))))




data.agg$newyearseve<-  ifelse(data.agg$week==1166,1 ,
                                    ifelse(data.agg$week==1218,1, 
                                           ifelse(data.agg$week==1270,1, 
                                                  ifelse(data.agg$week==1322,1, 
                                                         ifelse(data.agg$week==1374,1, 
                                                                ifelse(data.agg$week==1426,1, 
                                                                       ifelse(data.agg$week==1479,1, 
                                                                              ifelse(data.agg$week==1531,1, 
                                                                                     ifelse(data.agg$week==1583,1,
                                                                                            ifelse(data.agg$week==1635,1, 
                                                                                                   ifelse(data.agg$week==1687,1,
                                                                                                          0)))))))))))




data.agg1 <- subset(data.agg, data.agg$year<20066)
data.agg <- subset(data.agg1, data.agg1$year>2009)

data.agg$yearfactor<-as.factor(data.agg$year)
data.agg$iri_keyfactor<-as.factor(data.agg$iri_key)
data.agg$y2011<-ifelse(data.agg$year==2011,1,0)
data.agg$iri_key264242<-ifelse(data.agg$iri_key==264242,1,0)
data.agg$iri_key284586<-ifelse(data.agg$iri_key==284586,1,0)
data.agg$iri_key292593<-ifelse(data.agg$iri_key==292593,1,0)
data.agg$iri_key657979<-ifelse(data.agg$iri_key==657979,1,0)
data.agg$iri_key668871<-ifelse(data.agg$iri_key==668871,1,0)
data.agg$iri_key933690<-ifelse(data.agg$iri_key==933690,1,0)

head(data.agg)
# Calculation of OG 
library(plyr)
sumunits<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+iri_key+year+week, data = data.agg, sum, na.rm = TRUE)
head(sumunits)
sumunits<-rename(sumunits, c("units"="sumunits", "dollars"="sumdollars"))
data.agg <- merge(data.agg,sumunits,by=c("MARKET","CHAIN","store_type","iri_key","year","week")) 

data.agg$share1<-data.agg$units/data.agg$sumunits*(data.agg$sumdollars/(data.agg$total_rev_carbbev/1))

withinshare<-aggregate(cbind(share1) ~ MARKET+CHAIN+store_type+iri_key+year+week, data = data.agg, sum, na.rm = TRUE)
withinshare<-rename(withinshare, c("share1"="shareog1"))
data.agg <- merge(data.agg,withinshare,by=c("MARKET","CHAIN","store_type","iri_key","year","week")) 
data.agg$shareog1<-(1-data.agg$shareog1)
head(data.agg)

t1<-lm(log(share1)-log(shareog1)~-1+L5+yearfactor+iri_keyfactor+(price)+display_minor+display_major+feature_small+feature_medium+feature_large+thanksgiving:FLAVOR.SCENT+christmas+newyearseve,data=data.agg)
summary(t1)
aggregate(cbind(units, price) ~ L4+L5+VOL_EQ+PACKAGE+FLAVOR.SCENT+CALORIE.LEVEL, data = data.agg, mean, na.rm = TRUE)


# Nested Logit: Diet vs Sugar first
sumunits.diet<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+iri_key+year+week+CALORIE.LEVEL, data = data.agg, sum, na.rm = TRUE)
sumunits.diet<-rename(sumunits.diet, c("units"="sumunits.diet", "dollars"="sumdollars.diet"))
data.agg <- merge(data.agg,sumunits.diet,by=c("MARKET","CHAIN","store_type","iri_key","year","week","CALORIE.LEVEL")) 
data.agg$share.diet<-data.agg$units/data.agg$sumunits.diet
summary(data.agg)
head(data.agg,10)

nt<-lm(log(share1)-log(shareog1)~-1+log(share.diet)+L5+yearfactor+iri_keyfactor+(price)+displayall+featureall,data=data.agg)
summary(nt)



# Nested Logit: Flavor first
sumunits.flavor<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+iri_key+year+week+FLAVOR.SCENT, data = data.agg, sum, na.rm = TRUE)
sumunits.flavor<-rename(sumunits.flavor, c("units"="sumunits.flavor", "dollars"="sumdollars.flavor"))
data.agg <- merge(data.agg,sumunits.flavor,by=c("MARKET","CHAIN","store_type","iri_key","year","week","FLAVOR.SCENT")) 
data.agg$share.flavor<-data.agg$units/data.agg$sumunits.flavor
head(data.agg,10)

nt<-lm(log(share1)-log(shareog1)~-1+log(share.flavor)+L5+yearfactor+iri_keyfactor+(price)+displayall+featureall,data=data.agg)
summary(nt)


head(data)

# 3-level nested logit
data<-data.agg
#data<-subset(data,data$FLAVOR.SCENT == "GINGER ALE")
data$group<-data$CALORIE.LEVEL
data$subgroup<-data$FLAVOR.SCENT
sumunits.subgroup<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+iri_key+year+week+group+subgroup, data = data, sum, na.rm = TRUE)
sumunits.subgroup<-rename(sumunits.subgroup, c("units"="sumunits.subgroup", "dollars"="sumdollars.subgroup"))
data <- merge(data,sumunits.subgroup,by=c("MARKET","CHAIN","store_type","iri_key","year","week","group","subgroup")) 

sumunits.group<-aggregate(cbind(units, dollars) ~ MARKET+CHAIN+store_type+iri_key+year+week+group, data = data, sum, na.rm = TRUE)
sumunits.group<-rename(sumunits.group, c("units"="sumunits.group", "dollars"="sumdollars.group"))
data <- merge(data,sumunits.group,by=c("MARKET","CHAIN","store_type","iri_key","year","week","group")) 
head(data,10)

data$share.hg<-data$sumunits.subgroup/data$sumunits.group
data$share.jhg<-data$units/data$sumunits.subgroup

nt<-lm(log(share1)-log(shareog1)~-1+log(share.hg)+log(share.jhg)+L5+y2011+iri_key264242+iri_key284586+iri_key292593+iri_key657979+iri_key668871+(price)+displayall+featureall+thanksgiving+christmas+newyearseve,data=data)
summary(nt)





sigma.g<-nt$coefficients['log(share.hg)']
sigma.g
sigma.hg<-nt$coefficients['log(share.jhg)']
sigma.hg
alpha<-nt$coefficients['price']
alpha
share.jhg<-data$share.jhg
share.hg<-data$share.hg
share.j<-data$share1
price<-data$price

eta.jj<-(1/(1-sigma.hg)-(1/(1-sigma.hg)-1/(1-sigma.g))*share.jhg-sigma.g/(1-sigma.g)*share.jhg*share.hg-share.j)*alpha*price
eta.jk<-((1/(1-sigma.hg)-1/(1-sigma.g))*share.jhg+sigma.g/(1-sigma.g)*share.jhg*share.hg+share.j)*-alpha*price
eta.jkk<-(sigma.g/(1-sigma.g)*share.jhg*share.hg+share.j)*-alpha*price
eta.jkkk<-(share.j)*-alpha*price
mean(eta.jj)
data$etajj<-eta.jj
data$etajk<-eta.jk
data$etajkk<-eta.jkk
data$etajkkk<-eta.jkkk
aggregate(cbind(eta.jj,eta.jk,eta.jkk,eta.jkkk) ~ MARKET+CHAIN+store_type+L5+group+subgroup, data = data, mean, na.rm = TRUE)


summary(t1)
head(data.agg)
data$B1<-ifelse(data$L5=="MOUNTAIN DEW",1,0)
data$B2<-ifelse(data$L5=="DIET MOUNTAIN DEW",1,0)
data$B3<-ifelse(data$L5=="CANADA DRY",1,0)
data$B4<-ifelse(data$L5=="DIET CANADA DRY",1,0)
data$B5<-ifelse(data$L5=="SCHWEPPES",1,0)
data$B6<-ifelse(data$L5=="DIET SCHWEPPES",1,0)
data$B7<-ifelse(data$L5=="SPRITE",1,0)
data$B8<-ifelse(data$L5=="SPRITE ZERO",1,0)
data$B9<-ifelse(data$L5=="SIERRA MIST",1,0)
data$B10<-ifelse(data$L5=="DIET SIERRA MIST",1,0)
data$B11<-ifelse(data$L5=="PL GINGER ALE",1,0)
data$B12<-ifelse(data$L5=="PL DIET GINGER ALE",1,0)
data$B13<-ifelse(data$L5=="PL LEMON LIME",1,0)
data$B14<-ifelse(data$L5=="PL DIET LEMON LIME",1,0)


data$B1.price<-data$price*.2*data$B1
data$price.sim<-data$price-data$B1.price
est<-nt$coefficients
summary(nt)
est<-rename(est, c("L5MOUNTAIN DEW"="b1", "L5DIET MOUNTAIN DEW"="b2","L5CANADA DRY"="b3","L5DIET CANADA DRY"="b4",
                   "L5SCHWEPPES"="b5","L5DIET SCHWEPPES"="b6","L5SPRITE"="b7","L5SPRITE ZERO"="b8",
                   "L5SIERRA MIST"="b9","L5DIET SIERRA MIST"="b10",
                   "L5PL GINGER ALE"="b11","L5PL DIET GINGER ALE"="b12",
                   "L5PL LEMON LIME"="b13","L5PL DIET LEMON LIME"="b14",
                   "iri_key264242"="i1","iri_key284586"="i2","iri_key292593"="i3","iri_key657979"="i4","iri_key668871"="i5",
                   "y2011"="beta0","price"="alpha","displayall"="beta1","featureall"="beta2"))
summary(nt)           
d<-data
head(d)
# pred shares at observed prices
d$delta<-d$B1*est['b1']+d$B2*est['b2']+d$B3*est['b3']+d$B4*est['b4']+d$B5*est['b5']+d$B6*est['b6']+d$B7*est['b7']+
         d$B8*est['b8']+d$B9*est['b9']+d$B10*est['b10']+d$B11*est['b11']+d$B12*est['b12']+d$B13*est['b13']+
         d$B14*est['b14']+
         d$y2011*est['beta0']+d$displayall*est['beta1']+d$featureall*est['beta2']+d$price*est['alpha']+
         d$iri_key264242*est['i1']+d$iri_key284586*est['i2']+d$iri_key292593*est['i3']+d$iri_key657979*est['i4']+d$iri_key668871*est['i5']
d$expdelta<-exp(d$delta)

Dh<-exp(d$delta/(1-sigma.hg))
Dh<-aggregate(cbind(Dh) ~ MARKET+CHAIN+store_type+iri_key+year+week+group+subgroup, data = d, sum, na.rm = TRUE)
d<- merge(d,Dh,by=c("MARKET","CHAIN","store_type","iri_key","year","week","group","subgroup")) 
d$pred.share.jhg<-exp(d$delta/(1-sigma.hg))/d$Dh
d$tmpn<-d$Dh^((1-sigma.hg)/(1-sigma.g))
tmpd<-aggregate(cbind(tmpn) ~ MARKET+CHAIN+store_type+iri_key+year+week+group, data = d, sum, na.rm = TRUE)
tmpd<-rename(tmpd, c("tmpn"="tmpd"))
d<- merge(d,tmpd,by=c("MARKET","CHAIN","store_type","iri_key","year","week","group")) 
d$pred.share.hg<-d$tmpn/d$tmpd

d$tmpn<-d$tmpd^(1-sigma.g)
tmpd<-aggregate(cbind(tmpn) ~ MARKET+CHAIN+store_type+iri_key+year+week, data = d, sum, na.rm = TRUE)
tmpd<-rename(tmpd, c("tmpn"="tmp.d"))
d<- merge(d,tmpd,by=c("MARKET","CHAIN","store_type","iri_key","year","week")) 
d$pred.share.g<-d$tmpn/d$tmp.d
d$pred.share<-d$pred.share.jhg*d$pred.share.hg*d$pred.share.g


head(d,10)

