##Time Series Demo ##
##Wei Fu 2022-12-8##

#white noise
set.seed(4)
x<-ts(rnorm(200))
ts.plot(x, col="blue", main="White Noise")
abline(h=0, col="orange")

#random walk
set.seed(5)
y[1]<-rnorm(1)
for (t in 2:200) y[t]<- y[t-1]+rnorm(1)
yt <- ts(y)
ts.plot(y, col="red", lwd=2, main="Random Walk")
abline(h=0, col="green4")

#set.seed(4)
y<- w <-rnorm(1000)
for (t in 2:1000) y[t]<- y[t-1]+w[t]
yt <- ts(y)
ts.plot(y, col="red", lwd=2, main="Random Walk")
abline(h=0, col="green4")

#AR(1) Sample ACF and PACF
del <- 0.5
phi <- -0.6
w<- rnorm(1000)
ar1 <- del + w[1]
for (t in 2:1000) ar1[t]<- del + phi*ar1[t-1]+w[t]
ats <- ts(ar1)
acf(ats, main="AR(1) Sample ACF")
pacf(ats, main="AR(1) Sample PACF")

#compute an AR(1) with ACF =-0.6
#ar1b <-acf2AR(c(1, -0.6))

#AR(1) theoretical ACF and PACF
#thear <- ARMAacf(ar=c(-0.3), lag.max=30, pacf = TRUE)
#plot(thear, type="h")

#MA(1) ACP and PACF
mu <- 1
theta1 <- -0.8
e<- rnorm(1000)
ma1 <- mu + e[1]
for (t in 2:1000) ma1[t]<- mu + e[t] + theta1*e[t-1]
mts <- ts(ma1)
acf(mts, main="MA(1) Sample ACF")
pacf(mts, main="MA(1) Sample PACF")

# library(readxl)
# Self_Service_Item_Level_Pivot12072022 <- read_excel("Training/TimeSeries/Self-Service_Item Level_Pivot12072022.xlsx", 
#                                                     sheet = "Sheet1", col_types = c("text", 
#                                                                                     "numeric"))
# View(Self_Service_Item_Level_Pivot12072022)
Library(zoo)
#as.yearmon("2020-01") is a funtion in zoo
prem<- read.zoo(Self_Service_Item_Level_Pivot12072022, FUN=as.yearmon)

#install.packages("sarima")
#library(sarima)

#plot prem, looks like a quadratic trend
ts.plot(prem)
par(mfrow=c(1,1))
plot(prem, main="Average Unit Prem 2020 - 2022")

#try 2nd order differencing
pdiff <- diff(prem, 2)
plot(pdiff, col="blue", main="Average Unit Premium with 2nd Differening")

#looks like there may still be some trend and maybe seasonality
#ignore it for now and fit a non-seaonal ARIMA

acf2(pdiff)
acf(pdiff)
pacf(pdiff)

#acf cuts off after 1, pacf tapers off
#try MA(1)
sarima(prem, 0, 2, 1) # ACF shows seasonality at 6, Ljung-Box looks very bad
#adding seasonality 
sarima(prem, 0, 2, 1, 0, 0, 1, 6) 
#much better, but still a spike at lag 12, Ljung-Box looks better but not great

#change it to just 1 non-seasonal and no seasonal differences
sarima(prem, 0, 1, 1, 0, 0, 1, 6) # ma1 no longer significant

#remove ma1 but keep sma1
sarima(prem, 0, 1, 0, 0, 1, 1, 6)

#seasonal difference first
pd12<-diff(prem, 12)
par(mfrow=c(1,1))
plot(pd12, main="Average Unit Premium with 12th Difference") 
acf2(pd12)
#still trend, 1st difference again
pd1and12<-diff(pd12, 1)
par(mfrow=c(1,1))
plot(pd1and12, main="Average Unit Premium with 1st & 12th Difference")
acf2(pd1and12, main="Avg. Unit Prem with 1st and 1th Difference")
acf(pd1and12, main="Avg. Unit Prem with 1st and 1th Difference")
pacf(pd1and12,main="Avg. Unit Prem with 1st and 1th Difference")

#just check
pd6<- diff(prem, 6)
par(mfrow=c(1,1))
plot(pd6)

sarima(prem, 0, 1, 0, 0, 1, 1, 6)