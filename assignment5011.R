#Sequential---------------------------------------------------------------------------------------------------

#input the all the csv from the folder
#set working directory
setwd("C:/Users/lim woei lee/Desktop/5011/Grocery Data Set")

#read all the file into 1 dataset
library(tidyverse)
df<-
  list.files(path = "/Users/lim woei lee/Desktop/5011/Grocery Data Set",pattern = "*.csv")%>%
  map_df(~read_csv(.))
df

#create function in variable f
f <- function(i) {
  list.files(path = "/Users/lim woei lee/Desktop/5011/Grocery Data Set",pattern = "*.csv")%>%
    map_df(~read_csv(.))
}

#sequential time taken
sequential<-system.time(lapply(1:2,f))
sequential



#PARALLEL----------------------------------------------------------------------------------------------------

library(parallel)

numCores<- detectCores()

c1<-makeCluster(numCores)

c1

clusterEvalQ(c1,{
  library(ggplot2)
  library(stringr)
  library(tidyverse)
  library(parallel)
})

parellel<-system.time(parLapply(c1, 1:2, f))


parellel
sequential

library(microbenchmark)
aaa<-microbenchmark(parellel,sequential)
aaa

ggplot(aaa,aes(x=expr,y=time))+geom_bar(stat = "identity")
autoplot(aaa)


#define objective-------------------------------------------------------------------------------------- 

#storing data in vairable
july<-read.csv("Jul_borough_grocery.csv")
aug<-read.csv("Aug_borough_grocery.csv")
sep<-read.csv("Sep_borough_grocery.csv")

table1<-data.frame(july$area_id,
                   july$energy_fat,july$num_transactions,
                   aug$energy_fat,aug$num_transactions,
                   sep$energy_fat,sep$num_transactions)

summary(table1)

#descriptive analysis-----------------------------------------------------------------------------------

mean(table1$july.energy_fat)
mean(table1$july.num_transactions)
mean(table1$aug.energy_fat)
mean(table1$aug.num_transactions)
mean(table1$sep.energy_fat)
mean(table1$sep.num_transactions)

median(table1$july.energy_fat)
median(table1$july.num_transactions)
median(table1$aug.energy_fat)
median(table1$aug.num_transactions)
median(table1$sep.energy_fat)
median(table1$sep.num_transactions)

sd(table1$july.energy_fat)
sd(table1$july.num_transactions)
sd(table1$aug.energy_fat)
sd(table1$aug.num_transactions)
sd(table1$sep.energy_fat)
sd(table1$sep.num_transactions)

hist(table1$july.energy_fat,main = "histogram of July energy_fat")
hist(table1$aug.energy_fat, main = "histogram of August energy_fat")
hist(table1$sep.energy_fat,main = "histogram of September energy_fat")
hist(table1$july.num_transaction,main = "histogram of July num_transaction")
hist(table1$aug.num_transactions,main = "histogram of August num_transaction")
hist(table1$sep.num_transactions ,main = "histogram of September num_transaction")

test<-data.frame(mean = c(mean(table1$july.energy_fat),
                          mean(table1$aug.energy_fat),
                          mean(table1$sep.energy_fat)),
                 sd = c(sd(table1$july.energy_fat),
                        sd(table1$aug.energy_fat),
                        sd(table1$sep.energy_fat)),
                 catogory=c("july.energy_fat","aug.energy_fat","sep.energy_fat"))
ggplot(test,aes(x=catogory,y = mean))+
  geom_bar(position = position_dodge(),stat = "identity" )

#correlation test-------------------------------------------------------------------------------------

cor.test(table1$july.energy_fat ,table1$july.num_transactions)
cor.test(table1$aug.energy_fat,table1$aug.num_transactions)
cor.test(table1$sep.energy_fat,table1$sep.num_transactions)


#july
ggscatter(table1, x = "july.energy_fat", y = "july.num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "energy_fat", ylab = "num_trannsaction")

#aug
ggscatter(table1, x = "aug.energy_fat", y = "aug.num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "energy_fat", ylab = "num_trannsaction")

#sep
ggscatter(table1, x = "sep.energy_fat", y = "sep.num_transactions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "energy_fat", ylab = "num_trannsaction")


install.packages(ggstatsplot)
library(ggstatsplot)
ggscatterstats(
  data = dat,
  x = wt,
  y = mpg,
  bf.message = FALSE
)

#hypothesis testing--------------------------------------------------------------------------------------------------------
setwd("C:/Users/lim woei lee/Desktop/5011/Data")

install.packages("caTools")
library(caTools)
library(tidyverse)
df1<-
  list.files(path = "/Users/lim woei lee/Desktop/5011/Data",pattern = "*.csv")%>%
  map_df(~read_csv(.))
df1

hypo<- lm(july.energy_fat~july.num_transactions,table1)
print(summary(hypo))

hypo1<- lm(aug.energy_fat~aug.num_transactions,table1)
print(summary(hypo1))

hypo2<- lm(sep.energy_fat~sep.num_transactions,table1)
print(summary(hypo))


#regression-------------------------------------------------------------------------------------------------------------
a<-c(table1$july.energy_fat,
     table1$aug.energy_fat,
     table1$sep.energy_fat)
b<-c(table1$july.num_transactions,
     table1$aug.num_transactions,
     table1$sep.num_transactions)

regression <-lm(b~a)
print(regression)

plot(a, b, main = "energy_fat vs num_transaction Regression model", 
     xlab="energy_fat", ylab="num_transaction")
abline(lm(b~a))

6