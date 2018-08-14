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
median(store.df$p1sales)
c(p1=median(store.df$p1sales), p2= median(store.df$p2sales))
hist ( store.df$p1sales )
hist(store.df$p1sales, breaks=25)
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



c(p1=median(store.df$p1sales), p2= median(store.df$p2sales))

lapply(X = store.df[, c(4, 5)], FUN = median)
apply(store.df[, c("p1sales", "p2sales")],2,FUN=median)
str(store.df)
summary(store.df$storeNum)

#

boxplot ( store.df$p2sales , xlab =" Weekly sales ", ylab ="P2",
          main =" Weekly sales of P2 , All stores ",
          horizontal = TRUE )

boxplot ( store.df$p2sales , xlab =" Weekly sales ", ylab ="P2",
          main =" Weekly sales of P2 , All stores ",
          horizontal = FALSE )

boxplot ( store.df$p2sales ~ store.df$storeNum, horizontal =TRUE ,
          ylab =" Store ", xlab =" Weekly unit sales ", las =1,
          main =" Weekly Sales of P2 by Store ")

boxplot ( store.df$p2sales ~ store.df$p2prom, horizontal =TRUE ,
          ylab =" Store ", xlab =" Weekly unit sales ", las =1,
          main =" Weekly Sales of P2 by Promo ")


boxplot ( p2sales ~ p2prom , data = store.df , horizontal =TRUE ,
          yaxt ="n", ylab ="P2 promoted in store ?",
          xlab =" Weekly sales ",main =" Sales of P2 vs promotion ")
axis( side =2, at=c(1 ,2) , labels =c("No"," Yes "), 
      las =1)
summary(store.df$p2prom)
store.df$p2promfactor <-ifelse(store.df$p2prom==1, 'Yes', 'No')

boxplot ( p2sales ~ p2promfactor , data = store.df , horizontal =TRUE ,
          ylab ="P2 promoted in store ?",
          xlab =" Weekly sales ",main =" Sales of P2 vs promotion ")




by( store.df$p1sales , store.df$storeNum , mean )


desc <- function (x) { list ( mean = mean (x), sd = sd(x))}
by( store.df$p1sales , store.df$storeNum , desc )

install.packages("psych")
library( psych )
describeBy ( store.df$p1sales , store.df$storeNum , trim=0.5)


library( data.table ) 
store.dt <- as.data.table ( store.df)
store.dt
store.dt[, list ( mean = mean ( p1sales ),
                  sd = sd( p1sales )), keyby = storeNum ]
store.dt[, desc ( p1sales ), keyby = storeNum ]
store.dt[, describe( p1sales ), keyby = storeNum ]
storeMean <- aggregate ( store.df$p1sales ,
                         by= list(store = store.df$storeNum ), mean )
storeMean


storeSum <- aggregate ( store.df$ p1sales ,
                        by= list ( store = store.df$
                                     storeNum ), sum )
storeSum


table ( store.df$p1price , store.df$p1prom )

p1.table2 <- table(store.df$p1price,store.df$p1prom )
p1.table2
p1.table2[, 2] / (p1.table2[, 1] + p1.table2[, 2])

summary(store.df)
describe(store.df)


# Aggregate sales by country
p1sales.sum <- aggregate ( store.df$p1sales ,
                           by= list ( country = store.df$country ), sum )
p1sales.sum
install.packages("rworldmap")
install.packages("RColorBrewer")
library( rworldmap )
library( RColorBrewer )
p1sales.map <- joinCountryData2Map(p1sales.sum ,
                                   joinCode ="ISO2",
                                   nameJoinColumn ="country")
mapCountryData ( p1sales.map , nameColumnToPlot ="x",
                 mapTitle ="Total P1 sales by Country ",
                 colourPalette = brewer.pal(7, "Greens"),
                 catMethod ="fixedWidth", addLegend = FALSE )





summary(store.df)

plot(x= store.df$p1price , y= store.df$p1sales)
cor.test(x= store.df$p1price , y= store.df$p1sales)


tmp<-lm(p1sales~p1price+p1prom, data=store.df)
summary(tmp)


store.df$ela.lin<--52*store.df$p1price/store.df$p1sales
summary(store.df)

store.df$logp1sales<-log(store.df$p1sales)
store.df$logp1price<-log(store.df$p1price)
summary(store.df)


summary(lm(logp1sales~logp1price, data=store.df))

store.df$logp1prom<-log(store.df$p1prom)
store.df$oneoverp1price<- -1/store.df$p1price
summary(lm(logp1sales~store.df$oneoverp1price, data=store.df))


summary(lm(logp1sales~logp1price+p1prom+p2prom, data=store.df))


summary(store.df)
