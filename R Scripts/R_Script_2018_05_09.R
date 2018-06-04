setwd('C:/Users/ludwi/OneDrive/Dokumente/AMM_SS2018/Data')
data.cola<-read.csv("cola_amm_boston.csv")

str(cola)
proddesc<-read.csv("prod11_carbbev.csv")
str(proddesc)
head(proddesc,3)
head(cola)
describe(cola)
summary(cola)


tmp<-read.csv("Datad2011.csv")
head(tmp,20)
head(cola,10)
proddesc$PACKAGE
library(psych)
describe(cola)
describeBy(cola$dollars,cola$L5)


data.cola <- subset(data.cola, data.cola$PACKAGE=="BOTTLE")
data.cola <- subset(data.cola, data.cola$store_type=="G")
data.cola <- subset(data.cola, data.cola$CHAIN==33)
head(data.cola,10)
describeBy(data.cola$dollars,data.cola$L5)
describeBy(data.cola$units,data.cola$L5)
describeBy(data.cola$price,data.cola$L5)
describeBy(data.cola$display,data.cola$L5)
str(data.cola)

data.cola$logunits<-log(data.cola$units)
data.cola$logprice<-log(data.cola$price)

data.cola$display_minor<- ifelse(data.cola$display==1,1,0)
data.cola$display_major<- ifelse(data.cola$display==2,1,0)
data.cola$feature_small<- ifelse(data.cola$feature=="C",1,0)
data.cola$feature_medium<- ifelse(data.cola$feature=="B",1,0)
data.cola$feature_large<- ifelse(data.cola$feature=="A",1,0)

data.cola$iri_keyf<-as.factor(data.cola$iri_key)


summary(lm(logunits~L5+logprice*L5+display_minor+display_major+feature_small+feature_medium+feature_large, data=data.cola))
summary(lm(logunits~iri_keyf, data=data.cola))
table(data.cola$iri_key)
