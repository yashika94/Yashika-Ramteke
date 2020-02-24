library(fpp)
library(fpp2)
library(ggplot2)
library(quantmod)

#Setting the start date and end date
from_date <- as.Date("2015-01-04")
from_date
to_date <- as.Date("2019-01-11")
to_date

#Using lapply function
lapply(from_date, class)
lapply(to_date, class)

#Web crawling from Yahoo finance
getSymbols("BTC-USD", src = "yahoo", from = from_date, to = to_date)
View(`BTC-USD`)
head(`BTC-USD`)
summary(`BTC-USD`)

#Converting to timeseries data
data_ts <- ts(`BTC-USD`,start=c(2015,1),end=c(2019,01), frequency = 12)
data_ts

#Selecting only BTC-USD.Open column for further analysis
data=data_ts[,1]
data
View(data)

#Calculating training data
training_data = window(data,start=c(2015,1), end=c(2018,1))
training_data

#Calculating testing data
testing_data = window(data,start=c(2018,1), end=c(2019,1))
testing_data

#Autocorelation Function
#Here, lwd indicates the width of the lines.
Acf(data, lwd=3,main="Bitcoin price")

#DataPlot
autoplot(data) + ggtitle("Bitcoin price") + xlab("Year") + ylab("Currency in USD")
#Seasonal plot
ggseasonplot(data, year.labels=TRUE, year.labels.left=TRUE) + ggtitle("Seasonal plot for Bitcoin price") + ylab("Currency in USD")
#Seasonal subseries plot
ggsubseriesplot(data) + ggtitle("Seasonal subseries plot for Bitcoin price") + ylab("Currency in USD")



#Ploting Mean, Naive, and Seasonal Naive
mean_fit <- meanf(training_data,h=12)
naive_fit <- naive(training_data,h=12)
snaive_fit <- snaive(training_data,h=12)
autoplot(training_data) +
  autolayer(meanf(training_data, h=12),
            series="Mean", PI=FALSE) +
  autolayer(naive(training_data, h=12),
            series="Naive", PI=FALSE) +
  autolayer(snaive(training_data, h=12),
            series="Seasonal naive", PI=FALSE) +
  ggtitle("Forecasts of Bitcoin price using Mean, Naive and Snaive") +
  xlab("Year") + ylab("Currency in USD") +
  guides(colour=guide_legend(title="Forecast"))


#Performing linear regression
lreg <- tslm(training_data ~ trend)
tslm_fit=forecast(lreg, h=12)
summary(tslm_fit)
plot(tslm_fit, ylab="Bitcoin price",
     xlab="t")


#Performing STL decomposition
stl_decomp <- stl(training_data, t.window=12, s.window="periodic")
plot(stl_decomp)

  #Demonstrating seasonally adjusted data
plot(training_data, col="grey",
     main="Bitcoin price",
     xlab="", ylab="New orders index")
lines(seasadj(stl_decomp),col="red",ylab="Seasonally adjusted")

#Performing Moving Average
par(mfrow=c(2,2))

plot(training_data, main="Bitcoin price",
     ylab="$ million", xlab="Year")
lines(ma(training_data,3),col="green")
legend("topleft",lty=1,col="green",cex=0.6,legend=c("3-MA"))
plot(training_data, main="Bitcoin price",
     ylab="$ million", xlab="Year")
lines(ma(training_data,5),orange="orange")
legend("topleft",lty=1,col="blue",cex=0.6,legend=c("5-MA"))
plot(training_data, main="Bitcoin price",
     ylab="$ million", xlab="Year")
lines(ma(training_data,7),col="red")
legend("topleft",lty=1,col="red",cex=0.6,legend=c("7-MA"))
plot(training_data, main="Bitcoin price",
     ylab="$ million", xlab="Year")
lines(ma(training_data,9),col="blue")
legend("topleft",lty=1,col="blue",cex=0.6,legend=c("9-MA"))


#Simple Exponential Smoothing technique
ses_fit <- ses(training_data, h = 12)
ses_fit <- forecast(ses_fit)
summary(ses_fit)
plot(ses_fit)

fit1 <-ses(training_data, alpha=0.1, initial="simple", h=3)
fit2 <-ses(training_data, alpha=0.2, initial="simple", h=3)
fit3 <-ses(training_data, h=3)
plot(fit1,main="Bitcoin price", ylab="Currency in USD", xlab="Year", fcol="white", type="o")
lines(fitted(fit1), col="red", type="o")
lines(fitted(fit2), col="green", type="o")
lines(fitted(fit3), col="blue", type="o")
lines(fit1$mean, col="red", type="o")
lines(fit2$mean, col="green", type="o")
lines(fit3$mean, col="blue", type="o")
legend("topleft",lty=1, col=c(1,"red","green","blue"),
       c("data", expression(lambda == 0.2), expression(lambda == 0.6),
         expression(lambda == 0.89)),pch=1)

#Holt's Linear trend
#This method has two smoothing techniques.
#SES is not reliable here.
hlin_fit <- holt(training_data, h=3)
hlin_fit <- forecast(hlin_fit)
summary(hlin_fit)
plot(hlin_fit, main = "Holt's Linear Trend")
lines(training_data)

#Performing Auto Arima
#Using auto.arima() to predict values automatially
acc.arima <- auto.arima(training_data)
arima_fit <- forecast(acc.arima, h=12)
summary(arima_fit)
plot(arima_fit)

#Holt's Winter Additive and Multiplicative technique
#These two variations differ in nature of the seasnal component
add_1 <- hw(training_data,seasonal="additive")
add_1 <- forecast(add_1)
mul_2 <- hw(training_data,seasonal="multiplicative")
mul_2 <- forecast(mul_2)
autoplot(training_data) +
  autolayer(add_1, series="HW additive forecasts", PI=FALSE) +
  autolayer(mul_2, series="HW multiplicative forecasts", PI=FALSE) +
  ggtitle("Bitcoin price") +
  xlab("Year") +
  ylab("Currency in USD") +
  guides(colour=guide_legend(title="Forecast"))



#Comparison between the models developed
acc.mean=accuracy(mean_fit,testing_data)
acc.naive=accuracy(naive_fit,testing_data)
acc.snaive=accuracy(snaive_fit,testing_data)
acc.linear=accuracy(tslm_fit,testing_data)
acc.ses=accuracy(ses_fit, testing_data)
acc.holt=accuracy(hlin_fit, testing_data)
acc.multi=accuracy(add_1, testing_data)
acc.add=accuracy(mul_2, testing_data)
acc.arima=accuracy(arima_fit, testing_data)


acc.table<-rbind(acc.mean, acc.naive, acc.snaive, acc.linear, acc.ses, acc.holt, acc.add, acc.multi, acc.arima)
acc.table
row.names(acc.table)<-c('Mean training','Mean test', 'Naive training', 'Naive test', 'Seasonal. Naive training', 'Seasonal. Naive test' ,'Linear training', 'Linear test','ses training', 'ses test',"Holt's Linear training", "Holt's Linear test", 'Add training', 'Add test','Multi training', 'Multi test','ARIMA training', 'ARIMA test')


#Tabular format
#Overall comarison between our different accuracy models to determine the best out of all
acc.table<-as.data.frame(acc.table)
acc.table

