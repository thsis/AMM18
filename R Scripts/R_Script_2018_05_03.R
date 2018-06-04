# Load CRM data


cust.df <- read.csv("http://goo.gl/PmPkaG")

str( cust.df)
head(cust.df,10)
cust.df$cust.id <- factor(cust.df$cust.id)
str(cust.df$cust.id)

plot(x= cust.df$age , y= cust.df$ credit.score )


cor.test ( cust.df$age , cust.df$ credit.score )
lm( cust.df$ credit.score ~ cust.df$age)


plot(cust.df$age,cust.df$credit.score ,
     col ="blue",
     xlim =c (15 ,55) , ylim =c(500 , 900) ,
     main =" Active Customers as of June 2014 ",
     xlab =" Customer Age ( years )", ylab =" Credit Score ")
abline(lm( cust.df$ credit.score ~ cust.df$age))


# Dealing with Zero inflated data

plot( cust.df$store.spend , cust.df$online.spend ,
      xlab =" Prior 12 months in - store sales ($)",
      ylab =" Prior 12 months online sales ($)")
cor.test ( cust.df$store.spend, cust.df$ online.spend )


plot( cust.df$store.spend +1,
      cust.df$online.spend +1, log="xy")

# Plot a panel
par(mfrow=c(2,2))
with( cust.df,plot ( distance.to.store , store.spend ))
with( cust.df,plot ( distance.to.store , online.spend ))
with( cust.df,plot ( distance.to.store , store.spend +1, log ="xy"))
with( cust.df,plot ( distance.to.store , online.spend +1, log ="xy"))


pairs( formula = ~ age + credit.score + distance.to.store
       + online.spend + store.trans + store.spend , data = cust.df)

install.packages("car")
library(car)
scatterplotMatrix( formula = ~ age + credit.score +
                     distance.to.store  ,
                   data = cust.df, diagonal ="histogram")

head(cust.df[, c(2, 3, 5:12) ],10)
library(psych)
describe(cust.df[, c(11, 12) ])
cor( cust.df[, c(2, 3, 5:12) ])
cor( cust.df[, c(2, 3, 5:12) ], use ="pairwise.complete.obs")
str(cust.df)
install.packages("corrplot")
library( corrplot ) # install if needed
par(mfrow=c(1,1))
corrplot(corr = cor ( cust.df[ , c(2,3, 5:12) ],
                      use ="complete.obs"), method ="ellipse")


tmp <-subset(cust.df, cust.df$sat.service>=0)
tmp1 <-subset(cust.df, !is.na(cust.df$sat.service))
str(tmp1)
describe(cust.df)

v1<-is.na(cust.df$sat.service)
v2<-is.na(cust.df$sat.selection)
table(v1,v2)

cor( cust.df[, c(2, 3, 5:12) ])
cor( cust.df[, c(2, 3, 5:12) ], use ="complete.obs")
cor( cust.df[, c(2, 3, 5:12) ], use ="pairwise.complete.obs")
cor( tmp[, c(2, 3, 5:12) ])

cor( cust.df[, c(2, 3) ])

lm(credit.score~age,data=cust.df)

summary(lm(credit.score~age+online.spend,data=cust.df))
install.packages('lm.beta')
library(lm.beta)
summary(lm.beta(lm(credit.score~age+online.spend,data=cust.df)))


# Dfferences between Groups

seg.df <- read.csv("http://goo.gl/qw303p")
str(seg.df)
summary(seg.df)
describe(seg.df)
mean(seg.df$income[seg.df$Segment == "Moving up"])

mean(seg.df$income[seg.df$Segment == "Moving up"&
                     seg.df$subscribe =="subNo"])


by( seg.df$income , seg.df$Segment , mean )

by(seg.df$income,list(seg.df$Segment, seg.df$subscribe ), mean )

aggregate( income ~ Segment + ownHome , data =seg.df , mean )
aggregate( income ~ Segment + ownHome , data =seg.df , sum )



with(seg.df, tapply(income, list(Segment,ownHome), mean) )


table(seg.df$Segment, seg.df$ownHome )
with(seg.df ,table(Segment, ownHome ))
with(seg.df ,prop.table(table(Segment, ownHome )))
with(seg.df ,prop.table(table(Segment, ownHome) , margin=2))

# Better histograms
library(lattice)
histogram(~subscribe | Segment , data=seg.df)


histogram (~ subscribe | Segment , data =seg.df ,
           type ="count", layout =c(3 ,3) ,
           col =c("burlywood", "darkolivegreen"))

histogram (~ subscribe | Segment + ownHome , data =seg.df)


seg.mean<-aggregate(income ~ Segment,data=seg.df,mean )
seg.mean
barchart(income~ Segment , data = seg.mean , col =" grey ")

seg.agg<-aggregate(income ~ Segment + ownHome,data=seg.df,mean )
seg.agg
str(seg.df)
barchart(income~ Segment , data = seg.agg, groups=ownHome,
         auto.key=TRUE )

bwplot(Segment ~ income , data =seg.df, horizontal =TRUE ,
       xlab = "Income")

boxplot (seg.df$income ~ seg.df$Segment , horizontal =TRUE ,
         ylab =" yyy", xlab ="xxx", las =1,
         main =" TITLE")


bwplot( income ~ ownHome , data =seg.df)
t.test ( income ~ ownHome , data =seg.df)


str(seg.df)
t<-aggregate( income ~ Segment + ownHome , data =seg.df , mean )
t<-aggregate(x = seg.df[, c("income", "age")], by = list(seg.df$Segment, seg.df$ownHome),
             FUN = mean)

t1<-ifelse (t$Group.2 =="ownNo", 0, 1)
str(t)
t<-cbind(t,t1)
t

seg.dt = data.table(seg.df)
t.dt <- seg.dt[, .(income = mean(income), age = mean(age)), by = .(Segment, ownHome)]
t.dt


tmp.tab <- table(rep(c(1:4) , times = c(25 ,25 ,25 ,20) ))
tmp.tab
str(tmp.tab)





seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)
str(seg.df)
chisq.test(table(seg.df$Segment ))
t1<-table(seg.df$subscribe,seg.df$ownHome )
t2<-table(seg.df$subscribe,seg.df$Segment)
chisq.test(t1,correct = FALSE)
t1
chisq.test(t2,correct = FALSE)
t2

binom.test(120 , 200, p =0.5)

t.test ( income ~ ownHome , data =seg.df)


seg.aov.own <- aov( income ~ ownHome , data = seg.df)
anova(seg.aov.own)

seg.aov.own <- aov( income ~ Segment , data = seg.df)
anova(seg.aov.own)

anova(aov( income ~ ownHome*Segment , data = seg.df))

anova(aov( income ~ ownHome+Segment+ownHome:Segment , data = seg.df))
anova(aov( income ~ ownHome*Segment*subscribe , data = seg.df))

anova(aov( income ~ ownHome+Segment+subscribe+ownHome:Segment , data = seg.df))

str(seg.df)

summary(lm(income ~ Segment, data=seg.df))

summary(lm(income ~ Segment-1, data=seg.df))




regout<-summary(lm(income ~ Segment-1, data=seg.df))
anova(aov( income ~ Segment , data = seg.df))
regout

str(regout)
out<-data.frame(regout$coefficients[,c('Estimate','Std. Error')])
out
out$lower<-out[,1]-2*out[,2]
out$upper<-out[,1]+2*out[,2]
out
str(out)

sat.df <- read.csv("http://goo.gl/HKnl74")
summary(sat.df)
str(sat.df)
cor( sat.df[ , c(2, 4:8) ])

library ( corrplot )
corrplot.mixed(cor( sat.df[ , c(2, 4:8) ]) , upper ="ellipse")

library(lattice)
library(grid)
library( gpairs )
gpairs(sat.df)


sat.df$logdist <- log(sat.df$distance )
str(sat.df)

m1<-(lm( overall ~ rides , data =sat.df))
plot ( overall ~ rides , data =sat.df ,
       xlab =" Satisfaction with Rides ", ylab ="Overall
       Satisfaction ")
abline(m1,col="blue")

# Session November 09, 2017


m1<-(lm( overall ~ rides , data =sat.df))
str(m1)
par( mfrow =c(2 ,2))
plot(m1)

m2<-lm(overall~rides+games+wait+clean, data = sat.df)
summary(m2)
summary(m1)
summary(m1)$r.squared


anova (m1 , m2)
library(ggplot2)
library(coefplot )
coefplot(m2 , intercept =FALSE , outerCI =1.96 ,
         lwdOuter =1.5 ,
         ylab =" Rating of Feature ",
         xlab =" Association with Overall
         Satisfaction ")
par( mfrow =c(1 ,1))
plot(sat.df$overall , fitted (m1), col ="red",
     xlim =c (0 ,100) , ylim =c (0 ,100) ,
     xlab =" Actual Overall Satisfaction ", ylab =" Fitted
     Overall Satisfaction ")
points(sat.df$overall , fitted (m2), col="blue")
legend("topleft", legend =c(" model 1", " model 2"),
       col =c("red", "blue"), pch =1)
coef(m2)["(Intercept)"]+coef(m2)["rides"]*100 +coef(m2)["games"]*100 +coef(m2)["wait"]*100+coef(m2)["clean"]*100
coef(m2)["(Intercept)"]+coef(m2)["rides"]*1 +coef(m2)["games"]*1 +coef(m2)["wait"]*1+coef(m2)["clean"]*1

exp(coef(m3)["(Intercept)"]+coef(m3)["rides"]*1 +coef(m3)["games"]*1 +coef(m3)["wait"]*1+coef(m3)["clean"]*1)

m3<-lm(log(overall)~rides+games+wait+clean, data = sat.df)
summary(m3)
coef(m2)%*%c(1, 1 , 1 , 1 , 1)

fitted(m2)[1:10]
predict(m2 , sat.df [1:10 , ])
str(sat.df)

index<-sample(1:500,400)
index
insample<-sat.df[index,]
outsample<-sat.df[-index,]
str(insample)
str(outsample)

m2<-lm(overall~rides+games+wait+clean, data = sat.df)
m2insample<-lm(overall~rides+games+wait+clean, data = insample)
summary(m2)
summary(m2insample)
predict(m2insample , outsample)
outsample$predict<-predict(m2insample , outsample)
str(outsample)
plot(outsample$overall , outsample$predict, col ="red",
     xlim =c (0 ,100) , ylim =c (0 ,100) ,
     xlab =" Actual Overall Satisfaction ", ylab =" Fitted
     Overall Satisfaction ")
cor(outsample$overall , outsample$predict)

sat.std <- sat.df[ , -3]
sat.std[ , 3:7] <- scale (sat.std[ , 3:7])
sat.std$ logdist <- log (sat.df$ distance )
str(sat.std)
m3 <- lm(overall ~rides+games+wait+clean+weekend +
           logdist+num.child , data = sat.std)
summary (m3)
sat.std$num.child.factor<-factor(sat.std$num.child )
m3factor <- lm(overall ~rides+games+wait+clean+weekend +
                 logdist+num.child.factor , data = sat.std)
summary(m3)
anova(m3,m3factor)

sat.std$has.child <- factor( sat.std$num.child > 0)
m3haschild <- lm(overall ~rides+games+wait+clean+weekend +
                   logdist+has.child , data = sat.std)
summary(m3haschild)
anova(m3factor,m3haschild)

summary(lm(overall ~rides+games+clean+weekend +
             logdist+num.child.factor+wait:num.child.factor , data = sat.std))
