library(readr)
#NFLX <- read_csv("C:/Users/wfu/Downloads/NFLX.csv", col_types = cols(`%Change` = col_number(), 
#                                          `Adj Close` = col_number(), Close = col_number(), 
#                                           Date = col_date(format = "%m/%d/%Y"), 
#                                           High = col_number(), Low = col_number(), 
#                                           Open = col_number()))

NFLX <- read_csv("C:/Users/wfu/Downloads/NFLX.csv", col_types = cols( 
  `Adj Close` = col_number(), Close = col_number(), 
  Date = col_date(format = "%Y-%m-%d"), 
  High = col_number(), Low = col_number(), 
  Open = col_number()))
View(NFLX)
attach(NFLX)

#plot the raw data
plot(Date, Close, type="p", pch=20, col="blue", cex=0.5,  main="NFLX Price")

#smooth the raw data
k<-c(1/24,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/24)
y<-filter(Close, filter=k, sides=2)
lines(Date, y, lty=1, col="red", lwd=2)

#smooth it again
y2<-filter(y, filter=k, sides=2)
lines(Date, y2, lty=2, col="green", lwd=1)
#detach(NFLX)

#there is trend, 1st order differencing
d <- diff(y, 1)
#d<-ts(d)
plot(d, type='b', pch=1, col="red", cex=0.5, main="Non-seasonal 1st order differencing")

#there is heteroscedasticity after 1st differencing, try log
logc <-log(Close)
plot(Date, logc, type="p", pch=20, col="blue", cex=0.5,  main="Log of NFLX Price", xaxt="n") 
#xaxt="n" means do not plot x axis
axis.Date(1, at=seq(as.Date("2013/1/1"), as.Date("2019/1/1"),"months"), format ="%D", las=1)
#identify(Date, logc, labels = Close)

#differencing logc
dlgc <- diff(logc, 1)
plot(dlgc, type='b', pch=1, col="red", cex=0.5, main="1st-order differencing of Log NFLX")



#install.packages(astsa)
library(astsa)
acf2(dlgc, 48, main = "1st-order differencing of Log NFLX")

#examine seasonal effect
#s <- diff(y, 12)
#plot(s, type='b', pch=1, col="blue", cex=0.5, main="Seasonal differencting, S=12")
#acf2(s, 48, main = "1st-order seasonal differencing, Season =12")

#sarima(y, 6, 1, 0, 3, 1, 0, S=12)
#sarima(y, 6, 1, 0, 3, 0, 0, S=12) #no seasonal differencing, failed
#sarima(y, 6, 1, 0, 4, 0, 0, S=12)
#sarima(y, 1, 1, 1, 0, 0, 1, S=12)
sarima(logc, 1, 1, 1)
sarima(logc, 5, 1, 0)
sarima(logc, 0, 1, 5)
sarima(logc, 5, 1, 5)

#it appears only lag 5 is significant in ACF and PACF. Fit ARIMA with non-consecutive lags
a <-arima(logc, order = c(5,1,5), fixed = c(0,0,0,0, NA,0,0,0,0,NA ))
t<- numeric()
for (i in 11:40) {
  b<-Box.test(a$residuals, lag = i, type = c("Box-Pierce", "Ljung-Box"), fitdf = 10)
  print(c(i, b$p.value))
  t<- rbind(t, c(i, b$p.value))
}#Ljung-Box
plot(t[,1], t[,2], col="red", ylim=c(0, 1), main="Ljung-Box")
abline(h=0.05, col="blue", lty=2)

sarima.for(logc, 12, 5, 1, 5)
