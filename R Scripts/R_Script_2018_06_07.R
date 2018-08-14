setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")

# read all carbbev
data.carbbev<-read.csv("carbbev_205066.csv")
data.carbbev<-subset(data.carbbev,data.carbbev$VOL_EQ==0.3521)
str(data.carbbev)
head(data.carbbev)
nrow(data.carbbev)
data.carbbev$yearfactor<-as.factor(data.carbbev$year)
data.carbbev$iri_keyfactor<-as.factor(data.carbbev$iri_key)
data.carbbev$display_minor<- ifelse(data.carbbev$d==1,1,0)
data.carbbev$display_major<- ifelse(data.carbbev$d==2,1,0)
data.carbbev$feature_small<- ifelse(data.carbbev$f=="C",1,0)
data.carbbev$feature_medium<- ifelse(data.carbbev$f=="B",1,0)
data.carbbev$feature_large<- ifelse(data.carbbev$f=="A",1,0)
data.carbbev$coupon<- ifelse(data.carbbev$f=="A+",1,0)
data.carbbev$displayall<-data.carbbev$display_minor+data.carbbev$display_major
data.carbbev$featureall<-data.carbbev$feature_small+data.carbbev$feature_medium+data.carbbev$feature_large
data.carbbev$volume<-data.carbbev$units*data.carbbev$VOL_EQ

data.carbbev$price<-data.carbbev$price*data.carbbev$volume
data.carbbev$display_minor<-data.carbbev$display_minor*data.carbbev$volume
data.carbbev$display_major<-data.carbbev$display_major*data.carbbev$volume
data.carbbev$feature_small<-data.carbbev$feature_small*data.carbbev$volume
data.carbbev$feature_medium<-data.carbbev$feature_medium*data.carbbev$volume
data.carbbev$feature_large<-data.carbbev$feature_large*data.carbbev$volume
data.carbbev$coupon<-data.carbbev$coupon*data.carbbev$volume
data.carbbev$displayall<-data.carbbev$displayall*data.carbbev$volume
data.carbbev$featureall<-data.carbbev$featureall*data.carbbev$volume

volume.sum<-aggregate(cbind(volume) ~  MARKET+CHAIN+store_type+year+week+L4+L5+PACKAGE, 
                       data = data.carbbev, sum, na.rm = TRUE)

head(volume.sum,10)
tmp<- aggregate(cbind(price,display_minor,display_major,feature_small,feature_medium,feature_large,
                      coupon,displayall,featureall) ~ 
                MARKET+CHAIN+store_type+year+week+L4+L5+PACKAGE,
                data = data.carbbev, sum, na.rm = TRUE)

head(tmp,10)
data.carbbev <- merge(volume.sum,tmp,by=c("MARKET","CHAIN","store_type","year","week","L4","L5","PACKAGE")) 

head(data.carbbev,100)
data.carbbev$price<-data.carbbev$price/data.carbbev$volume
data.carbbev$display_minor<-data.carbbev$display_minor/data.carbbev$volume
data.carbbev$display_major<-data.carbbev$display_major/data.carbbev$volume
data.carbbev$feature_small<-data.carbbev$feature_small/data.carbbev$volume
data.carbbev$feature_medium<-data.carbbev$feature_medium/data.carbbev$volume
data.carbbev$feature_large<-data.carbbev$feature_large/data.carbbev$volume
data.carbbev$coupon<-data.carbbev$coupon/data.carbbev$volume
data.carbbev$displayall<-data.carbbev$displayall/data.carbbev$volume
data.carbbev$featureall<-data.carbbev$featureall/data.carbbev$volume

head(data.carbbev,20)

tmp<- aggregate(cbind(volume,price,display_minor,display_major,feature_small,feature_medium,feature_large,
                      coupon,displayall,featureall) ~ 
                  
                  MARKET+CHAIN+store_type+L4+L5+PACKAGE,
                data = data.carbbev, mean, na.rm = TRUE)

tmp
tmp<- aggregate(cbind(volume) ~ 
                  
                  L4+L5+PACKAGE,
                data = data.carbbev, sum, na.rm = TRUE)

tmp
cola <-data.carbbev
str(cola)

data.carbbev$thanksgiving<- ifelse(cola$week==1160,1 ,
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



data.carbbev$christmas   <- ifelse(cola$week==1165,1,
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




data.carbbev$newyearseve<-  ifelse(cola$week==1166,1 ,
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


data.carbbev$display<- data.carbbev$display_minor+data.carbbev$display_major
data.carbbev$feature<- data.carbbev$feature_small+data.carbbev$feature_medium+data.carbbev$feature_large
data.carbbev$yearasfactor <-as.factor(data.carbbev$year)
data.carbbev$y2001<- ifelse(data.carbbev$year==2001,1,0)
data.carbbev$y2002<- ifelse(data.carbbev$year==2002,1,0)
data.carbbev$y2003<- ifelse(data.carbbev$year==2003,1,0)
data.carbbev$y2004<- ifelse(data.carbbev$year==2004,1,0)
data.carbbev$y2005<- ifelse(data.carbbev$year==2005,1,0)
data.carbbev$y2006<- ifelse(data.carbbev$year==2006,1,0)
data.carbbev$y2007<- ifelse(data.carbbev$year==2007,1,0)
data.carbbev$y2008<- ifelse(data.carbbev$year==2008,1,0)
data.carbbev$y2009<- ifelse(data.carbbev$year==2009,1,0)
data.carbbev$y2010<- ifelse(data.carbbev$year==2010,1,0)
data.carbbev$y2011<- ifelse(data.carbbev$year==2011,1,0)
str(data.carbbev)


model.full<-lm(log(volume)~log(price)+display+feature,data=data.carbbev)
summary(model.full)
# Bottle
data.carbbev.sub<-subset(data.carbbev,data.carbbev$PACKAGE=='BOTTLE')
data.carbbev.sub<-subset(data.carbbev.sub,data.carbbev.sub$L5=='COKE CLASSIC')
model<-lm(log(volume)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=data.carbbev.sub)
summary(model)
install.packages('car')
library(car)
dwt(model)
cor(cbind(data.carbbev.sub$display,data.carbbev.sub$feature))
plot(x=data.carbbev.sub$week,y=residuals(model),xlab="Week", ylab="Residuals",panel.last = abline(h=0, lty=2))



data.carbbev.sub<-subset(data.carbbev,data.carbbev$PACKAGE=='BOTTLE')
data.carbbev.sub<-subset(data.carbbev.sub,data.carbbev.sub$L5=='MOUNTAIN DEW')
model<-lm(log(volume)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=data.carbbev.sub)
summary(model)
install.packages('car')
library(car)
dwt(model)
cor(cbind(data.carbbev.sub$display,data.carbbev.sub$feature))
plot(x=data.carbbev.sub$week,y=residuals(model),xlab="Week", ylab="Residuals",panel.last = abline(h=0, lty=2))


data.carbbev.sub<-subset(data.carbbev,data.carbbev$PACKAGE=='BOTTLE')
data.carbbev.sub<-subset(data.carbbev.sub,data.carbbev.sub$L5=='COKE CLASSIC')
model<-lm(log(volume)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=data.carbbev.sub)
summary(model)
dwt(model)
cor(cbind(data.carbbev.sub$display,data.carbbev.sub$feature))


data.carbbev.sub<-subset(data.carbbev,data.carbbev$PACKAGE=='BOTTLE')
data.carbbev.sub<-subset(data.carbbev.sub,data.carbbev.sub$L5=='PEPSI')
model<-lm(log(volume)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=data.carbbev.sub)
summary(model)
install.packages('car')
library(car)
dwt(model)
cor(cbind(data.carbbev.sub$display,data.carbbev.sub$feature))
plot(x=data.carbbev.sub$week,y=residuals(model),xlab="Week", ylab="Residuals",panel.last = abline(h=0, lty=2))


data.carbbev.sub<-subset(data.carbbev,data.carbbev$PACKAGE=='BOTTLE')
data.carbbev.sub<-subset(data.carbbev.sub,data.carbbev.sub$L5=='DIET PEPSI')
model<-lm(log(volume)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=data.carbbev.sub)
summary(model)
dwt(model)
cor(cbind(data.carbbev.sub$display,data.carbbev.sub$feature))




# Can
data.carbbev.sub<-subset(data.carbbev,data.carbbev$PACKAGE=='CAN')
data.carbbev.sub<-subset(data.carbbev.sub,data.carbbev.sub$L5=='COKE CLASSIC')
model<-lm(log(volume)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=data.carbbev.sub)
summary(model)
dwt(model)
cor(cbind(data.carbbev.sub$display,data.carbbev.sub$feature))
cor(cbind(data.carbbev.sub$volume,data.carbbev.sub$price))
plot(x=data.carbbev.sub$week,y=residuals(model),xlab="Week", ylab="Residuals",panel.last = abline(h=0, lty=2))


data.carbbev.sub<-subset(data.carbbev,data.carbbev$PACKAGE=='CAN')
data.carbbev.sub<-subset(data.carbbev.sub,data.carbbev.sub$L5=='DIET COKE')
model<-lm(log(volume)~log(price)+display+feature+yearasfactor+thanksgiving+christmas+newyearseve,data=data.carbbev.sub)
summary(model)
dwt(model)
cor(cbind(data.carbbev.sub$display,data.carbbev.sub$feature))
head(data.carbbev.sub,20)
