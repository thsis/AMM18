setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")
coke<-read.csv("SalesDataCokeClassic.csv")
head(coke)
str(coke)
coke$display<- coke$display_minor+coke$display_major
coke$feature<- coke$feature_small+coke$feature_medium+coke$feature_large
summary(coke)


coke$logunits<-log(coke$units)
coke$logprice<-log(coke$price)
model.red<-lm(log(units)~log(price),data=coke)
summary(model.red)


dwt(model.red)

# Cochrane-Orcutt Procedure
res.ts <- ts(residuals(model.red))
lag1res <- lag(res.ts, -1)
lagdata1 <- ts.intersect(res.ts, lag1res)
rho <- coef(lm(res.ts ~ lag1res -1, data=lagdata1)) 
rho
y.ts <- ts(coke$logunits)
x.ts <- ts(coke$logprice)
lag1y <- lag(y.ts, -1)
lag1x <- lag(x.ts, -1)
y.co <- y.ts-rho*lag1y
x.co <- x.ts-rho*lag1x
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


coke$logunits<-log(coke$units)
coke$logprice<-log(coke$price)
tmp<-coke[c('logunits','logprice','display','feature','thanksgiving')]

  

model.full<-lm(logunits~logprice+display+feature+thanksgiving, data=coke)
dwt(model.full)
res.ts <- ts(residuals(model.full))
lag1res <- lag(res.ts, -1)
lagdata1 <- ts.intersect(res.ts, lag1res)
rho <- coef(lm(res.ts ~ lag1res -1, data=lagdata1)) 
rho

#install.packages('dplyr')
library(dplyr)
const<-1
tmp<-cbind(const,tmp)
tmplag<-apply(tmp,2,lag)
coke.autocor<-tmp-rho*tmplag
str(coke.autocor)
model<-lm(logunits~-1+const+logprice+display+feature+thanksgiving,data=coke.autocor)
summary(model)
summary(model.full)

res.ts <- ts(residuals(model.full))
lag1res <- lag(res.ts, -1)
lagdata1 <- ts.intersect(res.ts, lag1res)
rho <- coef(lm(res.ts ~ lag1res -1, data=lagdata1)) 
rho
y.ts <- ts(coke$logunits)
x.ts <- ts(coke[c('logprice','display','feature','thanksgiving')])
lag1y <- lag(y.ts, -1)
lag1x <- lag(x.ts, -1)
y.co <- y.ts-rho*lag1y
x.co <- x.ts-rho*lag1x
model.2 <- lm(y.co ~ x.co)
summary(model.2)


