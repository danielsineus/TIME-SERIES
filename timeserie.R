library(xts)
library(forecast)
library(dplyr)
library(zoo)
library(timeSeries)
library(lubridate)
library(ggplot2)

# read the CVS data
data<-read.csv("Salesrecord.csv", header = TRUE, stringsAsFactors = F)
str(data)
#Select the date and revenue columns 
data1<-data[,c(6,12)]
str(data1)
#Convert the character-format date into date format
data1$Order.Date<-as.Date(data1$Order.Date, "%m/%d/%Y")
data1$Order.Date<-sort(data1$Order.Date)

str(data1)
head(data1)
tail(data1)

#Convert the data by aggregating by days
# get the revenue expressed in 10000 unit of dollar
data2<-data1%>%
  group_by(Order.Date)%>%
  summarise(revenu=sum(Total.Revenue)/10000)%>%
  na.omit()%>%
  as.data.frame()
colnames(data2)<-c("date", "revenue")
str(data2)
head(data2)
tail(data2)


#Save a dataframe in R as csv
write.csv(data2,"datse.csv")
dat<-read.csv("datse.csv")
dat$date<-as.character(dat$date)
class(dat)
str(dat)

tat<-xts(dat, order.by = as.Date(dat$date,"%Y-%m-%d"))
class(tat)
str(tat)
tat<-tat[,c(0,3)]

# convert to a month period
# When used this method it take only the revenue of last day of the month and fix this revenue as revenue of the months
tat_monhly<-to.period(tat, period = "months", OHLC = FALSE)
head(tat_monhly, 30)
class(tat_monhly)
periodicity(tat_monhly)

#get the cumulative sum of each day till we get to the last day of the month and start over for the next month
sat_monthly<-split(tat, f="months")
sat_mont<-lapply(sat_monthly, cumsum)
sat_rbind<-do.call(rbind, sat_mont)
head(sat_rbind,30)
tail(sat_rbind)

#get the cumulative sum of each day till we get to the last day of the month and start over for the next month
edhec.mon<-split(tat[,1], f="months")
edhec.mon<-lapply(edhec.mon, cumsum)
edhec.monthd<-do.call(rbind, edhec.mon)
head(edhec.monthd,60)

head(edhec)

# Find the mean of each month not by spliting by using endpoint. only the last day that will be mentioned
tat_year<-tat["2010/2017"]
ep<-endpoints(tat_year, "months")
peded<-period.apply(tat_year, INDEX = ep, FUN=mean)

#calculate sum for each period by defining the endpoint
edagc<-period.sum(tat, INDEX = ep)
head(edagc)

#covert the xts data to ts data
peded_ts<-ts(edagc, start=c(2010,1), frequency=12)
class(peded_ts)
head(peded_ts, 100)
plot(peded_ts)

autoplot(peded_ts)

plot(peded_ts, xlab="Time", ylab="Revenue", bty="l")


plot(peded_ts, main="Revenue", xlab="Date", ylab="Revenue")
lines(peded_ts, col="red")

# Decomposing seasonal Data
#Trend component
#Seasonal component
#Irregular component
A<-decompose(peded_ts)
plot(A)


#Simple Forecasting Method
# set the training data from 2014 to 2017
set_train<-window(peded_ts, start=c(2015,1), end=c(2017,7))
plot(set_train)
#plot some forecasts
autoplot(set_train)+
  autolayer(meanf(set_train,h=22),
            series = "mean", PI=FALSE)+
  autolayer(naive(set_train, h=22),
            series = "Naive", PI=FALSE)+
  autolayer(snaive(set_train, h=22),
            series = "Seasonal naive",PI=FALSE)+
  ggtitle("Forecast for Quartely train")+
  xlab("year")+ylab("Revenue")+
  guides(colour=guide_legend(title = "forecast"))

library(forecast)

#Linear Regression for TIME SERIES
sales.lm<-tslm(peded_ts~trend+I(trend^2))
par(mfrow=c(2,1))
plot(peded_ts, xlab="time", ylab="Sales", bty="l")
lines(sales.lm$fitted, lwd=2)
ridership.ts.zoom<-window(peded_ts, start=c(2016,1), end=c(2017,7))
plot(ridership.ts.zoom, xlab="time", ylab="revenue")

#Joining partition
nvalid<-36
nvalid
length(peded_ts)
ndata<-length(peded_ts) - nvalid
ndata
55/12
train.ts<-window(peded_ts, start=c(2014,1), end=c(2014, ndata))
valid.ts<-window(peded_ts, startt=c(2014, ndata+1), end=c(2014, ndata+nvalid))
data2.lm<-tslm(train.ts~trend + I(trend^2))
data2.lm.pred<-forecast(data2.lm, h=nvalid, level=0)
View(data2.lm.pred)
plot(data2.lm.pred, ylab="Sales", xlab="time", bty="l", flty=2)
axis(1, at=seq(2015, 2019, 1), labels = format(seq(2015, 2019,1)))
lines(data2.lm$fitted, lwd=2)
lines(valid.ts)
#Measuring Predictive Accuracy 
accuracy(data2.lm.pred$mean, valid.ts)
# interpretation
names(data2.lm.pred)
data2.lm.pred$residuals
valid.ts - data2.lm.pred$mean

#Smoothing Method
# the moving average (MA) for forecasting
# Centered Moving Average 
library(zoo)
ma.trailing<-rollmean(peded_ts, k=12, align = "right")
ma.centered<-ma(peded_ts, order = 12)
plot(peded_ts, bty="l", xaxt="n")
axis(1, at=seq(2015, 2019,1), labels = format(seq(2015,2019,1)))
lines(ma.centered, lwd=2)
lines(ma.trailing, lwd=2, lty=2)
