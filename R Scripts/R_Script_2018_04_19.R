xx<-c(2,4,6,8)
x
xNum <- c(1,3.14159,5,7)
xNum
xLog <-c(TRUE,FALSE,TRUE,TRUE)
xLog
xChar <-c("foo","bar","boo","far")
xChar
xMix <-c(1,TRUE,3,"Hello, world")
xMix
str(xMix)
x2 <-c(x,x)
x2
c(x2,"Hello")
c(x2,100)
xMix
as.numeric(xMix[1])
xMix[1]+1.5
as.numeric(xMix[1])+1.5
xNum
summary(xNum)
summary(xChar)
xChar
x2
x2+1
x2+pi
x2_pi<-x2+pi
x2_pi
x2_pi<-x2_pi-pi
x2_pi
x
x2
(x+cos(0.5))*x2
length(x)
length(x2)
str(x2)
str(xChar)
xSeq <-1:10
xSeq
xSeq <-4:11
xSeq
1:5*2
1:(5*2)
xNum
xNum[2:4]
tmp<-xNum[2:4]
tmp
xNum[c(1,3)]
myStart <-2
xNum[myStart:sqrt(myStart+7)]
xNum
xNum[myStart:sqrt(myStart+14)]
1:(5*2)
xSeq
xSeq[-5:-7]
xNum
xNum[c(FALSE,TRUE,TRUE,TRUE)]
xNum[c(FALSE,TRUE,TRUE)]
xNum[c(FALSE)]
xNum
xNum >3
my.test.scores <-c(91,93,NA,NA)
my.test.scores
mean(my.test.scores)
max(my.test.scores)
str(my.test.scores)
mean(my.test.scores,na.rm=TRUE)
max(my.test.scores,na.rm=TRUE)
min(my.test.scores,na.rm=TRUE)
na.omit(my.test.scores)
mean(na.omit(my.test.scores))
is.na(my.test.scores)
mean(my.test.scores[!is.na(my.test.scores)])
?mean
x <- c(0:10, 50)
x
xm <- mean(x)
xm
c(xm, mean(x, trim = 0.10))


x.df<-(data.frame(xNum,xLog,xChar))
x.df
x.df[2,1]
x.df[2:4,1]
x.df[1,3]
x.df <-(data.frame(xNum,xLog,xChar,stringsAsFactors = FALSE))
x.df
x.df[1,3]



store.num <-  factor(c(3, 14, 21, 32, 54) ) # store id
store.num
store.rev <- c(543 , 654 , 345 , 678 , 234) # store revenue ,$K
store.visits <- c(45 , 78, 32, 56, 34) # visits , 1000 s
store.manager <- c(" Annie ", " Bert ", " Carla ", " Dave ", " Ella ")
store.df <- data.frame ( store.num , store.rev ,
                            store.visits , store.manager ,
                            stringsAsFactors = FALSE )
store.daniel <- data.frame ( store.num , store.rev ,
                         store.visits , store.manager)
store.df
str(store.df)
summary(store.df)

store.df$store.manager
store.df[, c(3, 4)]

setwd("C:/Users/ludwi/OneDrive/Dokumente/AMM_SS2018/Data")
write.csv ( store.df , file = "store-df.csv",row.names = FALSE )

tmp.df<-read.csv("store-df.csv")
tmp.df

tmp.df<-read.csv("cola_amm_ws1617.csv")
str(tmp.df)
tmp.df

se <- function (x) {sd(x) / sqrt ( length (x))}
se
se(store.df[,3])
se(store.df$store.visits)

f1 <- function (x) {sd(x) }
f1(store.df$store.visits)
f2<- function (x) { length (x)}
f2(store.df$store.visits)

se <- function (x) {
  tmp.sd <- sd(x)
  tmp.N <- length (x) 
  tmp.se <- tmp.sd / sqrt (tmp.N) 
  return (tmp.N) 
  }
se( store.df$store.visits )


se 



save (store.df , file ="store-df-backup.RData")
rm( store.df)
mean(store.df$store.rev)
load("store-df-backup.RData")
mean(store.df$store.rev)
store.df <-5
store.df



install.packages("car")
library(car)
Salaries
write.csv (Salaries, file = "salaries.csv",row.names = FALSE )
str(Salaries)
Salaries$logs<-(log(Salaries$salary))
summary(lm(logs ~ -1+ rank + yrs.since.phd + discipline + sex, data=Salaries))


# Describing data
store.df <- read.csv ("http://goo.gl/QPDdMl")
summary ( store.df)
table (store.df$p1price )
prop.table(table(store.df$p1price))
tmp <- table (store.df$p1price )
tmp
str(tmp)
table(store.df$p1price , store.df$p1prom )
table(store.df$p1prom, store.df$p2prom )
median(store.df$p1sales p2sales)
hist ( store.df$p1sales )
hist ( store.df$p1sales ,
       main =" Product 1 Weekly Sales Frequencies , All
       Stores ",
       xlab =" Product 1 Sales ( Units )",
       ylab =" Count ")
hist ( store.df$p1sales ,
       main =" Product 1 Weekly Sales Frequencies , All
       Stores ",
       xlab =" Product 1 Sales ( Units )",
       ylab =" Count ",
breaks=20,freq=FALSE,
col="lightblue")
lines ( density ( store.df$p1sales , bw =10) , # bw = smoothing
        type ="l", col =" darkred ", lwd =2)
