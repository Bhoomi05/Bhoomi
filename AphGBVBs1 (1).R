rm(list=ls())
setwd("C:/Users/hp/Desktop/DATA SCIENTISTS")
getwd()
#Import Train and Test Data Set
data=read.csv("Rental_Bike.csv")
#Combine both Train and Test Data set to understand the distribution of independent variable together
#Variable type of each variables
str(data)
#Find missing values in data set if any.
table(is.na(data))
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(data$season)
hist(data$weather)
hist(data$hum)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)
data$season=as.factor(data$season)
data$weather=as.factor(data$weathersit)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)
data$day=substr(data$dteday,16,17)
data$day
data$day=as.factor(data$day)
data$day
train=data[as.integer(substr(data$dteday,9,10))<20,]
train
test=data[as.integer(substr(data$dteday,9,10))>19,]
test
boxplot(data$cnt~data$day,xlab="day", ylab="count of users")
boxplot(log(train$cnt)~train$day,xlab="day",ylab="log(cnt)")
data$year=substr(data$dteday,1,4)
data$year

data$year=as.factor(data$year)
data$year
train=data[as.integer(substr(data$year,1,4))<20,]
train
test=data[as.integer(substr(data$year,1,4))>19,]
test
boxplot(data$cnt~data$day,xlab="day", ylab="count of users")
data$rain=(data$weather==3 | data$weather==4)
data$rain
sub=data.frame(train$registered,train$casual,train$cnt,train$temp,train$hum,train$atemp,train$windspeed)
sub
cor(sub)
# hypothesis that bike demand will increase preceeding year
data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)
data$daytype=""
data$daytype
data$daytype[data$holiday==0 & data$workingday==0]="weekend"
data$daytype[data$holiday==1]="holiday"
data$daytype[data$holiday==0 & data$workingday==1]="working day"
data$daytype
data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday" ]=1
train$hour=as.factor(train$hour)
train$holiday=as.factor(train$holiday)
train$weekend=as.factor(train$weekend)
train$workingday=as.factor(train$workingday)
logreg=log(data$registered)
logreg
logcas=log(data$casual)
logcas
#predicting log of registered users
set.seed(416)
fit1= randomForest(logreg ~ dteday+ workingday+day+holiday+hum+atemp+windspeed+season+weathersit+weekend+year, data=train,importance=TRUE, ntree=250)
pred1=predict(fit1,test)
test$logreg=pred1
#predicting log of casual users
set.seed(415)
fit2= randomForest(logcas ~ dteday+ day + hum+atemp +windspeed+season+weather+holiday+workingday+weekend+year, data=train,importance=TRUE, ntree=250)
pred2=predict(fit2,test)
test$logcas=pred2
test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$cnt=test$casual+test$registered
s= data.frame(dteday=test$dteday,count=test$cnt)
write.csv(s,file="submit.csv",row.names=FALSE)

