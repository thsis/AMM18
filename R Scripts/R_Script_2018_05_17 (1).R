setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")
coke<-read.csv("SalesDataCokeClassic.csv")
head(coke)
str(coke)
coke$display<- coke$display_minor+coke$display_major
coke$feature<- coke$feature_small+coke$feature_medium+coke$feature_large
summary(coke)

# Display a scatterplot matrix of the data
pairs(formula =~units+price+display+feature, data=coke)
pairs(formula =~log(units)+log(price)+display+feature, data=coke)


# Fit a multiple linear regression of units on price, display and feature

model <- lm(units ~ price+display+feature , data=coke)
str(model)
model$fitted.values
summary(model)
model <- lm(log(units) ~ log(price)+display+feature, data=coke)
model <- lm(log(units) ~ log(price), data=coke)

# Use the ANOVA function to display anova table with sequential (typ I) sums of squares
anova(model)
summary(model)


# Do regression via matrix multiplication
str(coke)
Y <- coke[,9]
X <- cbind(1,coke[,10], coke[,17],coke [, 18])
b <- solve(t(X)%*%X)%*%t(X)%*%Y
b

yhat <- X%*%b
resid<-Y-yhat
tmp=cbind(Y,yhat,resid)
head(tmp,10)
mean(resid)


# Create a scatterplot of the data with a fitted simple linear regression
# of units on price and a horizontal line at the mean of price
model.red<-lm(units~price,data=coke)
model.full<-lm(units~price+display+feature+price*feature,data=coke)
summary(model.red)
summary(model.full)

plot(coke$price,coke$units,xlab="Price",ylab="Unit Sales Coke Classic",
     panel.last=c(lines(sort(coke$price),fitted(model.red)[order(coke$price)]),
                  abline(h=mean(coke$units),lty=2)))

plot(coke$price,coke$units,xlab="Price",ylab="Unit Sales Coke Classic",
     panel.last=c(lines(sort(coke$price),fitted(model.full)[order(coke$price)]),
                  abline(h=mean(coke$units),lty=2)))

plot ( units ~ price , data =coke ,
       xlab =" Price", ylab ="Unit Sales Coke Classic")
abline(model.red,col="blue")


# Calculate th SSE for the full and the reducd model
SSE.red<-sum(residuals(model.red)^2)
SSE.full<-sum(residuals(model.full)^2)

# Calculate the F-st to compare the reduced vs. the full modell
Ftest<- (SSE.red-SSE.full)/(model.red$df.residual-model.full$df.residual)*model.full$df.residual/SSE.full
Ftest
anova(model.red, model.full)


# F-Test
# full
MSR<-sum((model.full$fitted.values-mean(coke$units))^2)/(nrow(coke)-model.full$df.residual-1)
MSE<-sum((coke$units-model.full$fitted.values)^2)/model.full$df.residual
MSR
MSE
MSR/MSE
summary(model.full)
pf(MSR/MSE,1,nrow(coke)-2,lower.tail=F)
# reduced
MSR<-sum((model.red$fitted.values-mean(coke$units))^2)/(nrow(coke)-model.red$df.residual-1)
MSE<-sum((coke$units-model.red$fitted.values)^2)/model.red$df.residual
MSR
MSE
MSR/MSE
summary(model.red)
pf(MSR/MSE,1,nrow(coke)-2,lower.tail=F)
nrow(coke)


SSE.red<-sum((coke$units-mean(coke$units))^2)
Ftest<- (SSE.red-SSE.full)/(nrow(coke)-model.full$df.residual)*nrow(coke)/SSE.full
Ftest
summary(model.full)
str(model.full)
nrow(coke)
model.full$df.residual



summary(model.full)
summary(model.red)
head(coke)

confint(model.full)
confint(model.red)

# Prediction with 95% confidence interval
predict(model.full, data=coke,interval="confidence", se.fit=T,
        newdata=data.frame(price=0.7,display=1,feature=1))

predict(model.full, data=coke,interval="confidence", se.fit=T,
        newdata=data.frame(price=c(1,1.5,2),display=c(1,0.5,0),feature=c(1,1,0)))

        
#Display residual plot with fitted (predicted) values on the horizontal axis.
plot(x=fitted(model.full), y=residuals(model.full),
     xlab="Fitted values", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

plot(x=fitted(model.red), y=residuals(model.red),
     xlab="Fitted values", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

#Display residual plot with price on the horizontal 
plot(x=coke$price, y=residuals(model.full),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

plot(x=coke$price, y=residuals(model.red),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))

#Display residual plot with display on the horizontal axis.
plot(x=coke$display, y=residuals(model.full),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))


#Display residual plot with feature on the horizontal axis.
plot(x=coke$feature, y=residuals(model.full),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))


#Display residual plot with price*feature on the horizontal axis.
plot(x=(coke$price*coke$feature), y=residuals(model.full),
     ylab="Residuals",
     panel.last = abline(h=0, lty=2))


#Display histogram of the residuals.
hist(residuals(model.full), main="",breaks=50)
hist(residuals(model.red), main="",breaks=50)

#Display normal probability plot of the residuals and add a diagonal line to the plot. The argument "datax" determines which way round to plot the axes (false by default, which plots the data on the vertical axis, or true, which plots the data on the horizontal axis).
qqnorm(residuals(model.full), main="", datax=TRUE)
qqline(residuals(model.full), datax=TRUE)

qqnorm(residuals(model.red), main="", datax=TRUE)
qqline(residuals(model.red), datax=TRUE)




# Influential and outlier observations
d<-read.csv("data_leverage.csv")
dd<-read.csv("t.csv",dec = ",",sep=";")


d<-d[1:20,1:3]
d
d1<-rbind(d,c(21,4,40))
d2<-rbind(d,c(21,14,68))
d3<-rbind(d,c(21,13,15))
d1
d1
plot(d1$x,d1$y)
plot(y~x,data=d)
plot(y~x,data=d1,col=ifelse(d1$Row==21,"red","black"))
plot(y~x,data=d2,col=ifelse(d1$Row==21,"red","black"))
plot(y~x,data=d3,col=ifelse(d1$Row==21,"red","black"))

model.0<-lm(y~x,data=d)
model.1<-lm(y~x,data=d1)
model.2<-lm(y~x,data=d2)
model.3<-lm(y~x,data=d3)
summary(model.0)
summary(model.1)
summary(model.2)
summary(model.3)


model.test<-lm(y~x,data=d3)
summary(model.test)
# leverage 
lev <- hatvalues(model.test)
round(lev, 6)
str(lev)
plot(lev)

which(lev>3*2/21)

lev_thres <- 3*2/20
lev_thres

plot(sort(lev))

3*2/21
# Influential data points
dffit <-dffits(model.test)
round(dffit,6)
dffit_thres <-2*sqrt((2+1)/(21-2-1))
dffit_thres
which(abs(dffit)>dffit_thres)

cook <- cooks.distance(model.test)
round(cook, 6)



str(d1)
dd
summary(model.red)
# leverage 
lev <- hatvalues(model.full)
round(lev, 6)
str(lev)
plot(lev)

which(lev>3*5/208)

lev_thres <- 3*2/208
t<-coke[c(50, 207),]
t
plot(sort(lev))
str(coke)
coke_wo_lev <-coke[lev<=lev_thres,]
str(coke_wo_lev)

3*2/208
# Influential data points
dffit <-dffits(model.red)
round(dffit,6)
plot(dffit)
cook <- cooks.distance(model.red)
round(cook, 6)


#Load the nortest package to access normality tests:
#  - Anderson-Darling
#  - Lilliefors (Kolmogorov-Smirnov)

install.packages("nortest")
library(nortest)
ad.test(residuals(model.full)) 
ad.test(residuals(model.red)) 
shapiro.test(residuals(model.full)) 
shapiro.test(residuals(model.red)) 
lillie.test(residuals(model.full)) 
lillie.test(residuals(model.red)) 


#Create lotgroup factor variable splitting the sample into two groups.
#Load the car package to access tests for constant error variance:
  
#  Modified Levene (Brown-Forsythe)
#Cook-Weisberg score (Breusch-Pagan)

library(car)
model <- lm(log(units) ~ log(price)+display+feature, data=coke)
coke$pricegroup <- factor(coke$price<=mean(coke$price))

leveneTest(residuals(model), group=coke$pricegroup)
ncvTest(model)



coke$feature01 <- factor(coke$feature>0.5)
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
logmodel.full <- lm(logunits~logprice+display+feature,data=coke)
summary(logmodel.full)

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
            newdata=data.frame(logprice=c(log(1),log(1.5),log(2)),display=c(1,0.5,0),feature=c(1,1,0))))

exp(coefficients(logmodel.full)[c(3,4)]) 
exp(confint(logmodel.full)[c(3,4),]) # 95% CI



vif(logmodel.full)

coke$price2<-coke$price^2
m<-lm(units~price+price2,data=coke)
summary(m)
vif(m)

t<-read.csv("test.csv")
t
t$i<-1
t
x<-t[,c(3, 1)]
x
y<-t[,2]
y
solve(t(x)%*%x)%*%t(x)%*%y
y<-as.matrix(y)
