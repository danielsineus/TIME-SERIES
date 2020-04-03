library(xts)
library(forecast)
library(dplyr)
library(zoo)
library(timeSeries)
library(lubridate)
library(ggplot2)
library(tidyverse)
data<-read.csv("Salesrecord.csv", header = TRUE, stringsAsFactors = F)
str(data)
data1<-data[,c(6,12)]
str(data1)
data1$Order.Date<-as.Date(data1$Order.Date, "%m/%d/%Y")
data1$Order.Date<-sort(data1$Order.Date)

str(data1)
head(data1)
tail(data1)
c<-data1%>%
  mutate(year=as.Date(yeardata1$Order.Date))

data2<-data1%>%
  group_by(Order.Date)%>%
  summarise(revenu=sum(Total.Revenue)/10000)%>%
  na.omit()%>%
  as.data.frame()
colnames(data2)<-c("date", "revenue")
str(data2)
head(data2)
tail(data2)


dat1<-dat%>%
  group_by(year)%>%
  summarise(revenue=sum(revenue))%>%
  na.omit()%>%
  as.data.frame()



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
View(tat)

# convert to a month period
tat_monhly<-to.period(tat, period = "months", OHLC = FALSE)
head(tat_monhly)
class(tat_monhly)
periodicity(tat_monhly)

sat_monthly<-split(tat, f="months")
sat_mont<-lapply(sat_monthly, cumsum)
sat_rbind<-do.call(rbind, sat_mont)
head(sat_rbind)
tail(sat_rbind)
rast<-as.ts(sat_rbind)
plot(rast)

tat_year<-tat["2010/2017"]
ep<-endpoints(tat_year, "months")
peded<-period.apply(tat_year, INDEX = ep, FUN = )
head(peded)
peded_ts<-as.ts(peded, frequency=12)
head(peded_ts)
plot(peded_ts)

head(sat_monthly)
help("period.apply")
data2_ts<-ts(data2$revenue)
head(data2_ts)
class(data2_ts)

data_xts<-as.xts(data2_ts)
class(data_xts)
head(data_xts)
head(data2_ts,22)
class(data2_ts)
start(data2_ts)
end(data2_ts)
tail(data2_ts)

data_xts<-as.xts(data2_ts)

#Simple Forecasting Method
# set the training data from 2014 to 2017
set_train<-window(data2_ts, start=c(2015,1), end=c(2017,7))
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


#Linear Regression for TIME SERIES
sales.lm<-tslm(data2_ts~trend+I(trend^2))
par(mfrow=c(2,1))
plot(data2_ts, xlab="time", ylab="Sales", bty="l")
lines(sales.lm$fitted, lwd=2)


#Joining partition
nvalid<-32
nvalid
length(data2_ts)
ndata<-length(data2_ts) - nvalid
ndata
train.ts<-window(data2_ts, start=c(2010,1), end=c(2010, ndata))
valid.ts<-window(data2_ts, startt=c(2010, ndata+1), end=c(2010, ndata+nvalid))
data2.lm<-tslm(train.ts~trend + I(trend^2))
data2.lm.pred<-forecast(data2.lm, h=nvalid, level=0)
View(data2.lm.pred)
plot(data2.lm.pred, ylab="Sales", xlab="time", bty="l", flty=2)
axis(1, at=seq(2010, 2017, 1), labels = format(seq(2010, 2017,1)))
lines(data2.lm$fitted, lwd=2)
lines(valid.ts)
#Measuring Predictive Accuracy 
accuracy(data2.lm.pred$mean, valid.ts)
# interpretation
names(data2.lm.pred)
data2.lm.pred$residuals
valid.ts - data2.lm.pred$mean

