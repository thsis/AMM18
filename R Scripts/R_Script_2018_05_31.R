setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")

# Aggregate Cola Data
data.cola<-read.csv("cola_amm_boston.csv")
#data.cola <- subset(data.cola, data.cola$PACKAGE=="BOTTLE")
#data.cola <- subset(data.cola, data.cola$store_type=="GROC")
data.cola <- subset(data.cola, data.cola$CHAIN==33)                    )
str(data.cola)

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

units.sum<-aggregate(cbind(units, dollars) ~ 
                       
                       MARKET+CHAIN+store_type+year+week+L4+L5+VOL_EQ+PACKAGE, 
                     
                     
                     data = data.cola.agg, sum, na.rm = TRUE)

head(units.sum,10)
tmp<- aggregate(cbind(price,price_deflated,display_minor,display_major,feature_small,feature_medium,feature_large,
                      coupon,displayall,featureall) ~ 
                  
                  MARKET+CHAIN+store_type+year+week+L4+L5+VOL_EQ+PACKAGE,
                data = data.cola.agg, sum, na.rm = TRUE)

head(tmp,10)
data.cola.agg <- merge(units.sum,tmp,by=c("MARKET","CHAIN","store_type","year","week","L4","L5","VOL_EQ","PACKAGE")) 

head(data.cola.agg,100)
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

tmp<- aggregate(cbind(units,price,price_deflated,display_minor,display_major,feature_small,feature_medium,feature_large,
                      coupon,displayall,featureall) ~ 
                  
                  MARKET+CHAIN+store_type+L4+L5+VOL_EQ+PACKAGE,
                data = data.cola.agg, mean, na.rm = TRUE)

tmp
cola <-data.cola.agg
str(cola)

cola$thanksgiving<- ifelse(cola$week==1160,1 ,
                           ifelse(cola$week==1213,1, 
                                  ifelse(cola$week==1265,1, 
                                         ifelse(cola$week==1317,1, 
                                                ifelse(cola$week==1369,1, 
                                                       ifelse(cola$week==1421,1, 
                                                              ifelse(cola$week==1473,1, 
                                                                     ifelse(cola$week==1526,1, 
                                                                            ifelse(cola$week==1578,1,
                                                                                   ifelse(cola$week==1630,1, 
                                                                                          ifelse(cola$week==1682,1,
                                                                                                 0)))))))))))



str(cola)
summary(cola)
library(psych)
describe(cola)
sum(cola$thanksgiving)
cola$christmas   <- ifelse(cola$week==1165,1,
                           ifelse(cola$week==1217,1, 
                                  ifelse(cola$week==1269,1, 
                                         ifelse(cola$week==1321,1, 
                                                ifelse(cola$week==1373,1, 
                                                       ifelse(cola$week==1425,1, 
                                                              ifelse(cola$week==1478,1, 
                                                                     ifelse(cola$week==1530,1, 
                                                                            ifelse(cola$week==1582,1,
                                                                                   ifelse(cola$week==1634,1, 
                                                                                          ifelse(cola$week==1686,1,
                                                                                                 0)))))))))))
                                  
                                  
                                  
                                  
cola$newyearseve<-  ifelse(cola$week==1166,1 ,
                    ifelse(cola$week==1218,1, 
                    ifelse(cola$week==1270,1, 
                    ifelse(cola$week==1322,1, 
                    ifelse(cola$week==1374,1, 
                    ifelse(cola$week==1426,1, 
                    ifelse(cola$week==1479,1, 
                    ifelse(cola$week==1531,1, 
                    ifelse(cola$week==1583,1,
                    ifelse(cola$week==1635,1, 
                    ifelse(cola$week==1687,1,
                                                                                                                                   0)))))))))))
str(cola)
sapply(cola[,21:24 ],sum)
describe(cola)


cola$display<- cola$display_minor+cola$display_major
cola$feature<- cola$feature_small+cola$feature_medium+cola$feature_large
cola$yearasfactor <-as.factor(cola$year)
cola$y2001<- ifelse(cola$year==2001,1,0)
cola$y2002<- ifelse(cola$year==2002,1,0)
cola$y2003<- ifelse(cola$year==2003,1,0)
cola$y2004<- ifelse(cola$year==2004,1,0)
cola$y2005<- ifelse(cola$year==2005,1,0)
cola$y2006<- ifelse(cola$year==2006,1,0)
cola$y2007<- ifelse(cola$year==2007,1,0)
cola$y2008<- ifelse(cola$year==2008,1,0)
cola$y2009<- ifelse(cola$year==2009,1,0)
cola$y2010<- ifelse(cola$year==2010,1,0)
cola$y2011<- ifelse(cola$year==2011,1,0)
str(cola)

model.full<-lm(log(units)~log(price)+display+feature,data=cola)
summary(model.full)
# Bottle
cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
str(cola.sub)
model<-lm(log(units)~log(price)+display+feature,data=cola.sub)
summary(model)
model<-lm(log(units)~-1+L5+L5:log(price)+L5:display+L5:feature,data=cola.sub)
summary(model)
model<-lm(log(units)~-1+L5*yearasfactor+L5:log(price)+L5:display+L5:feature,data=cola.sub)
summary(model)


cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='COKE CLASSIC')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))
plot(x=cola.sub$week,y=residuals(model),xlab="Week", ylab="Residuals",panel.last = abline(h=0, lty=2))
cbind(cola.sub$week==1352,cola.sub$week==1353)
cola.sub[(1352-1116):(1353-1110),]

cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='DIET COKE')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))


cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='DIET PEPSI')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))

cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='PEPSI')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))



cola.sub<-subset(cola,cola$VOL_EQ==0.75)
cola.sub<-subset(cola.sub,cola.sub$L5=='COKE CLASSIC')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))

cola.sub<-subset(cola,cola$VOL_EQ==0.75)
cola.sub<-subset(cola.sub,cola.sub$L5=='DIET COKE')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))


cola.sub<-subset(cola,cola$VOL_EQ==0.75)
cola.sub<-subset(cola.sub,cola.sub$L5=='DIET PEPSI')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
library(car)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))

cola.sub<-subset(cola,cola$VOL_EQ==0.75)
cola.sub<-subset(cola.sub,cola.sub$L5=='PEPSI')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))



cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='COKE CLASSIC')
model<-lm(log(units)~log(price)+display+feature,data=cola.sub)
summary(model)
library(nortest)
ad.test(residuals(model)) 
shapiro.test(residuals(model)) 
lillie.test(residuals(model)) 



# systematic analysis at the brand and package size level


library(car)
cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='COKE CLASSIC')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))
plot(x=cola.sub$week,y=residuals(model),xlab="Week", ylab="Residuals",panel.last = abline(h=0, lty=2))
cola.sub[(1352-1116):(1353-1110),]

cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='DIET COKE')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))


cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='DIET PEPSI')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))

cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='PEPSI')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))



cola.sub<-subset(cola,cola$VOL_EQ==0.75)
cola.sub<-subset(cola.sub,cola.sub$L5=='COKE CLASSIC')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))

cola.sub<-subset(cola,cola$VOL_EQ==0.75)
cola.sub<-subset(cola.sub,cola.sub$L5=='DIET COKE')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))


cola.sub<-subset(cola,cola$VOL_EQ==0.75)
cola.sub<-subset(cola.sub,cola.sub$L5=='DIET PEPSI')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))

cola.sub<-subset(cola,cola$VOL_EQ==0.75)
cola.sub<-subset(cola.sub,cola.sub$L5=='PEPSI')
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))



# Test Display feature interasction
cola.sub<-subset(cola,cola$VOL_EQ==0.3521)
cola.sub<-subset(cola.sub,cola.sub$L5=='COKE CLASSIC')
#cola.sub<-subset(cola.sub,cola.sub$year>2006)

cola.sub$sb <- ifelse(cola.sub$year>2006,1,0)
cola.sub$lagfeature<-lag(cola.sub$feature)



model<-lm(log(units)~log(price)*sb+display*sb+feature*sb+lagfeature*sb+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
dwt(model)
cor(cbind(cola.sub$display,cola.sub$feature))
plot(x=cola.sub$week,y=residuals(model),xlab="Week", ylab="Residuals",panel.last = abline(h=0, lty=2))

str(cola)



model<-lm(units~price+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)


tmp<- aggregate(cbind(units,price,displayall,featureall) ~ 
                  year+L4+L5+VOL_EQ+PACKAGE,
                data = data.cola.agg, mean, na.rm = TRUE)

tmp





#Display residual plot over time
plot(x=cola.sub$week,y=residuals(model),
     xlab="Week", ylab="Residuals",
     panel.last = abline(h=0, lty=2))
t<-cbind(cola.sub$week, residuals(model))
t<-as.data.frame(t)
str(t)
summary(t)
t[order(t$V2),]

model<-lm(log(units)~log(price)+display+feature:yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub)
summary(model)
ad.test(residuals(model)) 
shapiro.test(residuals(model)) 
lillie.test(residuals(model)) 


tmp<- subset(cola.sub, cola.sub$year<2008)
nrow(tmp)
model<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=tmp)
summary(model)


#Display residual plot over time
plot(x=tmp$week,y=residuals(model),
     xlab="Week", ylab="Residuals",
     panel.last = abline(h=0, lty=2))
t<-cbind(cola.sub$week, residuals(model))
t<-as.data.frame(t)
str(t)
summary(t)
t[order(t$V2),]
subset(cola.sub,cola.sub$week==1447)
library(car)
dwt(model)

# Weighted least squares

summary(lm(abs(residuals(model)) ~ log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub))
wts <- 1/fitted(lm(abs(residuals(model)) ~ yearasfactor:log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=cola.sub))^2
wts <- 1/fitted(lm(abs(residuals(model)) ~ fitted(model)))^4
str(wts)
wts<-as.data.frame(wts)
wts
plot(x=cola.sub$week,y=wts$wts,
     xlab="Week", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

model.w<-lm(log(units)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,weights=wts,data=cola.sub)
summary(model)
summary(model.w)
#Display residual plot over time
plot(x=cola.sub$week,y=residuals(model.w),
     xlab="Week", ylab="Residuals",
     panel.last = abline(h=0, lty=2))
t<-cbind(cola.sub$week, residuals(model.w))
t<-as.data.frame(t)
str(t)
summary(t)
t[rev(order(t$V2)),]

cola.sub$logprice<-log(cola.sub$price)
cola.sub$logunits<-log(cola.sub$units)

xx<-cola.sub[c('logprice','display','feature','thanksgiving','christmas','newyearseve',
               'y2001','y2002','y2003','y2004','y2005','y2006','y2007','y2008','y2009','y2010','y2011')]
yy<-cola.sub[c('logunits')]


res.ts <- ts(residuals(model))
res <- (residuals(model))
lagdata1 <- ts.intersect(res.ts, lag1res)

rho <- coef(lm(res.ts ~ lag1res -1, data=lagdata1)) 
rho
y.ts <- ts(cola.sub$logunits)
x.ts <- ts(xx)

lag1y <- lag(y.ts, -1)
lag1x <- lag(x.ts, -1)
y.co <- y.ts-rho*lag1y
x.co <- x.ts-rho*lag1x
model.2 <- lm(y.co ~ x.co)


dwt(model.2)
summary(model)
summary(model.2)


                                                  
                                                             

install.packages('dplyr')
library(dplyr)
const<-1
xx<-cbind(const,xx)
xlag<-apply(xx,2,lag)
ylag<-apply(yy,2,lag)
x<-xx-rho*xlag
y<-yy-rho*ylag
cola.sub.autocor<-cbind(y,x)
model<-lm(logunits~-1+const+logprice+display+feature+thanksgiving+christmas+y2001+y2002+y2003+y2004+y2005+y2006+y2007+y2008+y2009+y2010+newyearseve,data=cola.sub.autocor)
summary(model)
head(y)
head(x)
str(t)
length(xx)
str(xx)
    )

summary(model.2)
