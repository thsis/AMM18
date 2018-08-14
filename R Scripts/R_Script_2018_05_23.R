setwd("C:/Users/klapperd/Dropbox/Humboldt/Lehre/AMM/AMM_SS2018/Data")
coke<-read.csv("SalesDataCokeClassic.csv")
head(coke)
str(coke)
coke$display<- coke$display_minor+coke$display_major
coke$feature<- coke$feature_small+coke$feature_medium+coke$feature_large
summary(coke)

# Display a scatterplot matrix of the data
pairs(formula =~(units)+(price)+display+feature, data=coke)
pairs(formula =~log(units)+log(price)+display+feature, data=coke)



model.red<-lm(log(units)~log(price),data=coke)
model.full<-lm(log(units)~log(price)+display+feature,data=coke)
summary(model.red)
summary(model.full)
anova(model.red,model.full)


coke$uniform<-runif(nrow(coke),0,10)
coke$random<-rnorm(nrow(coke),4,10)

model.red1<-lm(log(units)~log(price)+random+uniform,data=coke)
model.full1<-lm(log(units)~log(price)+display+feature+random+uniform,data=coke)
summary(model.full1)
summary(model.full)
summary(model.red)
summary(model.red1)


str(coke)
coke$yearasfactor<-as.factor(coke$year)
coke$weekasfactor<-as.factor(coke$week)

summary(lm(log(units)~log(price)+display+feature,data=coke))

summary(lm(log(units)~yearasfactor+log(price)+display+feature,data=coke))

tmp<- aggregate(cbind(units,price,display,feature) ~  year+ L5+VOL_EQ+PACKAGE,
                data = coke, mean, na.rm = TRUE)
tmp



summary(lm(log(units)~-1+yearasfactor+log(price)+display+feature,data=coke))
summary(lm(log(units)~yearasfactor:log(price)+display+feature,data=coke))

model.test<-(lm(log(units)~-1+yearasfactor+yearasfactor:log(price)+display+feature,data=coke))
summary(model.test)
str(coke)

summary(lm(log(units)~-1+weekasfactor+log(price)+display+feature,data=coke))
head(log(coke$units),10)

str(model.full)

#Display residual plot over time
plot(x=coke$week,y=residuals(model.full),
     xlab="Week", ylab="Residuals",
     panel.last = abline(h=0, lty=2))

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

ad.test(residuals(model.test)) 
shapiro.test(residuals(model.test)) 


