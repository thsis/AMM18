setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")
coke<-read.csv("SalesDataCokeClassic.csv")
head(coke)
str(coke)
coke$display<- coke$display_minor+coke$display_major
coke$feature<- coke$feature_small+coke$feature_medium+coke$feature_large
summary(coke)



model.full<-lm(log(units)~log(price)+display+feature,data=coke)


coke$uniform<-runif(nrow(coke),0,10)
coke$random<-rnorm(nrow(coke),4,10)


str(coke)
coke$yearasfactor<-as.factor(coke$year)
coke$weekasfactor<-as.factor(coke$week)

#Load the nortest package to access normality tests:
#  - Anderson-Darling
#  - Lilliefors (Kolmogorov-Smirnov)




install.packages("nortest")
library(nortest)
ad.test(residuals(model.full)) 
shapiro.test(residuals(model.full)) 
lillie.test(residuals(model.full)) 


t<-cbind(coke$week, residuals(model.full))
t<-as.data.frame(t)
str(t)
summary(t)
t[order(t$V2),]

coke$thanksgiving<- ifelse(coke$week==1526,1 ,ifelse(coke$week==1578,1 ,
                    ifelse(coke$week==1630,1, ifelse(coke$week==1682,1,0))))

model.test<-(lm(log(units) ~log(price)+display+feature+thanksgiving,data=coke))
summary(model.test)

#Display residual plot over time
plot(x=coke$week,y=residuals(model.test),
     xlab="Week", ylab="Residuals",
     panel.last = abline(h=0, lty=2))
t<-cbind(coke$week, residuals(model.test))
t<-as.data.frame(t)
str(t)
summary(t)
t[order(t$V2),]
coke[100:103,]


#Create group factor variable splitting the sample into two groups.
#Load the car package to access tests for constant error variance:

#  Modified Levene (Brown-Forsythe)
#Cook-Weisberg score (Breusch-Pagan)

install.packages("car")
library(car)
model <- lm(log(units) ~ log(price)+display+feature+thanksgiving, data=coke)
summary(model)
coke$pricegroup <- factor(coke$price<=mean(coke$price))
str(coke)
leveneTest(residuals(model), group=coke$pricegroup)
ncvTest(model)

coke$feature01 <- factor(coke$feature>0.5)
leveneTest(residuals(model), group=coke$feature01)


str(coke)
plot(x=coke$price, y=coke$units,  
     col=ifelse(coke$feature>0.9, "red", "blue"),
     panel.last = c(lines(sort(coke$price[coke$feature <0.9]),
                          fitted(model.full)[coke$feature<0.9][order(coke$price[coke$feature<0.9])],
                          col="blue"),
                    lines(sort(coke$price[coke$feature]),
                          fitted(model.full)[coke$feature>0.9][order(coke$price[coke$feature>0.9])],
                          col="red")))

# 
coke$logunits<-log(coke$units)
coke$logprice<-log(coke$price)
coke$yearfactor <-as.factor(coke$year)
logmodel.full <- lm(logunits~logprice+display+feature+thanksgiving,data=coke)
summary(logmodel.full)


summary(lm(log(units) ~ log(price)+display+feature, data=coke))

plot(x=coke$logprice, y=coke$logunits,
     panel.last = lines(sort(coke$logunits), fitted(logmodel.full)[order(coke$logprice)]))

plot(x=fitted(logmodel.full), y=residuals(logmodel.full),
     panel.last = abline(h=0, lty=2))
head(coke)

qqnorm(residuals(logmodel.full), main="", datax=TRUE)
qqline(residuals(logmodel.full), datax=TRUE)

ad.test(residuals(logmodel.full)) 
shapiro.test(residuals(logmodel.full)) 
lillie.test(residuals(logmodel.full)) 


exp(predict(logmodel.full, interval="prediction",
            newdata=data.frame(logprice=c(log(1),log(1.5),log(2)),display=c(1,0.5,0),feature=c(1,1,0),thanksgiving=c(0,0,0))))

exp(coefficients(logmodel.full)[c(3,4,5)]) 
exp(confint(logmodel.full)[c(3,4,5),]) # 95% CI


vif(model)
vif(logmodel.full)

coke$price2<-coke$price^2
m<-lm(units~price+price2,data=coke)
m<-lm(units~price+random+uniform,data=coke)
summary(m)
vif(m)

dwt(logmodel.full)
install.packages("rgl")
library(rgl)
scatter3d(units ~ price+feature,data=coke)


coke$thanksgiving<- ifelse(coke$week==1526,1 ,ifelse(coke$week==1578,1 ,
                                                     ifelse(coke$week==1630,1, ifelse(coke$week==1682,1,0))))

coke$christmas<- ifelse(coke$week==1530,1 ,ifelse(coke$week==1582,1 ,
                                                  ifelse(coke$week==1634,1, ifelse(coke$week==1686,1,0))))

coke$newyearseve<- ifelse(coke$week==1531,1 ,ifelse(coke$week==1583,1 ,
                                                    ifelse(coke$week==1635,1, ifelse(coke$week==1687,1,0))))

# Weighted least squares
coke$logprice<-log(coke$price)
coke$logunits<-log(coke$units)
model.red<-lm(log(units)~log(price),data=coke)
model.full<-lm(log(units)~log(price)+display+feature,data=coke)
summary(model.full)

plot(coke$logprice, residuals(model.red))
plot(coke$logprice, abs(residuals(model.red)))

summary(lm(abs(residuals(model.full)) ~ log(price)+display+feature,data=coke))
wts <- 1/fitted(lm(abs(residuals(model.full)) ~ log(price)+display+feature,data=coke))^2

model.full.w<-lm(log(units)~log(price)+display+feature,weights=wts,data=coke)
summary(model.full)
summary(model.full.w)


# weighted least squares with thanksgiving

model.full<-lm(log(units)~log(price)+display+feature+thanksgiving,data=coke)
summary(model.full)


wts <- 1/fitted(lm(abs(residuals(model.full)) ~ log(price)+display+feature+thanksgiving,data=coke))^2

model.full.w<-lm(log(units)~log(price)+display+feature+thanksgiving,weights=wts,data=coke)
summary(model.full)
summary(model.full.w)

# weighted least squares with thanksgiving, christmas, newyearseve

model.full<-lm(log(units)~log(price)+display+feature+thanksgiving+christmas+newyearseve,data=coke)
summary(model.full)


wts <- 1/fitted(lm(abs(residuals(model.full)) ~ log(price)+display+feature+thanksgiving+christmas+newyearseve,data=coke))^2

model.full.w<-lm(log(units)~log(price)+display+feature+thanksgiving+christmas+newyearseve,weights=wts,data=coke)
summary(model.full)
summary(model.full.w)



plot(x=coke$logprice, y=coke$logunits, ylim=c(0,20),
     panel.last = c(lines(sort(coke$logprice), fitted(model.red)[order(coke$logprice)], col="blue"),
                    lines(sort(coke$logprice), fitted(model.red.w)[order(coke$logprice)], col="red")))
legend("topleft", col=c("blue","red"), lty=1,
       inset=0.02, legend=c("OLS", "WLS"))

plot(coke$logprice, rstandard(model.red.w))






# Weighted least squares
coke$logprice<-log(coke$price)
coke$logunits<-log(coke$units)

coke$feature<-ifelse(coke$feature>0.8,1,0)
model.red<-lm(units~price,data=coke)
model.full<-lm(units~price+display+feature,data=coke)
summary(model.red)
summary(model.full)

plot(coke$price, residuals(model.red))
plot(coke$price, abs(residuals(model.red)))


wts <- 1/fitted(lm(abs(residuals(model.red)) ~ price,data=coke))^2

model.red.w<-lm(units~price,weights=wts,data=coke)
summary(model.red)
summary(model.red.w)



plot(x=coke$price, y=coke$units, ylim=c(0,20000),
     panel.last = c(lines(sort(coke$price), fitted(model.red)[order(coke$price)], col="blue"),
                    lines(sort(coke$price), fitted(model.red.w)[order(coke$price)], col="red")))
legend("topleft", col=c("blue","red"), lty=1,
       inset=0.02, legend=c("OLS", "WLS"))

plot(coke$price, rstandard(model.red.w))




vars <- tapply(residuals(model.full), (coke$feature),var)
vars
wts <- coke$feature/vars[2] + (1-coke$feature)/vars[1]


model.full.w<-lm(units~price+display+feature,weights=wts,data=coke)
summary(model.full)
summary(model.full.w)

plot(fitted(model.full), rstandard(model.full), col=coke$feature+1)
plot(fitted(model.full.w), rstandard(model.full.w), col=coke$feature+1)


coke$logunits<-log(coke$units)
coke$logprice<-log(coke$price)
model.red<-lm(log(units)~log(price),data=coke)

dwt(model.red)

# Cochrane-Orcutt Procedure
res.ts <- ts(residuals(model.red))
lag1res <- lag(res.ts, -1)
lagdata1 <- ts.intersect(res.ts, lag1res)
acp <- coef(lm(res.ts ~ lag1res -1, data=lagdata1)) 
acp
y.ts <- ts(coke$logunits)
x.ts <- ts(coke$logprice)
lag1y <- lag(y.ts, -1)
lag1x <- lag(x.ts, -1)
y.co <- y.ts-acp*lag1y
x.co <- x.ts-acp*lag1x
model.2 <- lm(y.co ~ x.co)
summary(model.2)

dwt(model.2)
summary(model.red)
summary(model.2)


b0 <- coef(model.2)[1]/(1-acp) 
sqrt(vcov(model.2)[1,1])/(1-acp) 
b1 <- coef(model.2)[2] 

fit.208 <- b0+b1*coke$logprice[208] 
res.208 <- coke$logunits[208]-fit.208 
fit.209 <- b0+b1*1
forecast.209 <- fit.209+acp*res.208 
exp(forecast.209)




