# Uncomment to install the necessary packages 
#install.packages(c("quantmod", "MASS", "moments",ggplot2", "zoo", "fGarch", "broom"))

# Load packages into library
library(quantmod,moments,MASS,tseries)
library(forecast,ggplot2,zoo,fGarch,broom)

#Source the forecasting functions
source("ForecastFunctions.R")

#import daily price data from 2000-05-15 to 2017-09-09 
getSymbols(Symbols = c("IVV"), parent.frame(),from = "2000-05-15", to = "2017-09-09",src = "yahoo") 

#Use only adjusted closing prices
ivv.prices <- IVV$IVV.Adjusted

#calculate the log returns -- will give you NA value in dataframe
ivv.returns <- diff(log(ivv.prices))

#take out the NA from differencing
ivv.returns <- ivv.returns[-1]

#calculate the squared returns
ivv.returns.sq <- ivv.returns^2

#Augmented Dickey Fuller test for stationarity
#The null hypothesis is that the returns series 
# is not stationary (or has a unit root)
# There are more sophisticated tests that will be used in due time
adf.test(ivv.returns,alternative = c("stationary"))

#split data into training and testing data and plot
ivv.returns.train <- ivv.returns[1:(.90*length(ivv.returns)),]
plot(ivv.returns.train)

ivv.returns.test <- ivv.returns[(.90*length(ivv.returns)):length(ivv.returns),]
plot(ivv.returns.test)

#plotting
par(mfrow=c(1,2))
acf(ivv.returns.train)
pacf(ivv.returns.train)

par(mfrow=c(1,2))
plot(ivv.prices)
plot(ivv.returns)

par(mfrow=c(1,1))



# Can uncomment the three lines under the fit for:
# 1. fcst is a dataframe of the forecasts on a rolling window
# 2. pred subsets the fcst dataframe into only the forecasted values
# 3. fe are the forecast errors

fit <- arima(ivv.returns.train, order = c(2,0,0))
# fcst <- data.frame(rollapply(returns[,4], width = length(returns.test), ArmaFcst1, by = 1))
# pred <- fcst[(length(fcst$IVV.Adjusted)-length(returns.test)+1):length(fcst$IVV.Adjusted),]
# fe <- (pred - returns.test)

fit1 <- arima(ivv.returns.train,order = c(1,0,1))
# fcst1 <- data.frame(rollapply(returns[,4], width = length(returns.test), ArmaFcst2, by = 1))
# pred1 <- fcst1[(length(fcst$IVV.Adjusted)-length(returns.test)):length(fcst$IVV.Adjusted),]
# fe1 <- (pred1 - returns.test)

fit2 <- arima(ivv.returns.train,order = c(2,0,1))
# fcst2 <- data.frame(rollapply(returns[,4], width = length(returns.test), ArmaFcst3, by = 1))
# pred2 <- fcst2[(length(fcst$IVV.Adjusted)-length(returns.test)+1):length(fcst$IVV.Adjusted),]
# fe2 <- (pred2 - returns.test)

fit3 <- arima(ivv.returns.train,order = c(1,0,2))
# fcst3 <- data.frame(rollapply(returns[,4], width = length(returns.test), ArmaFcst4, by = 1))
# pred3 <- fcst3[(length(fcst$IVV.Adjusted)-length(returns.test)+1):length(fcst$IVV.Adjusted),]
# fe3 <- (pred3 - returns.test)

fit4 <- arima(ivv.returns.train,order = c(0,0,2))
# fcst4 <- data.frame(rollapply(ivv.returns, width = length(returns.test), ArmaFcst4, by = 1))
# pred4 <- fcst3[(length(fcst$IVV.Adjusted)-length(returns.test)+1):length(fcst$IVV.Adjusted),]
# fe4 <- (pred4 - returns.test)



prediction1 <- data.frame(rollapply(ivv.returns, width = length(ivv.returns.test)-1,GarchFcst1,by = 1))
sd.prediction1 <- data.frame(rollapply(ivv.returns, width = length(ivv.returns.test),GarchStdevFcst1,by = 1))
resid.prediction1 <- data.frame(rollapply(ivv.returns, width = length(ivv.returns.test),GarchFcstErr1,by = 1))
pred1.ts <- as.xts(prediction1)
ivv.ar2garch.fcst <- prediction1[(length(ivv.ar2.fcst$IVV.Adjusted) - length(ivv.returns.test) +1):length(ivv.ar2.fcst$IVV.Adjusted),]
sd.pred1.ts <- as.xts(sd.prediction1)
ar.garch.fcst <- pred1.ts[length(ivv.returns.train)+1:length(ivv.returns),]
argarch.fe <- c(ivv.ar2garch.fcst - ivv.returns.test)


# prediction2 <- data.frame(rollapply(ivv.returns, width = length(ivv.returns.test),GarchFcst2,by = 1))

# prediction3 <- data.frame(rollapply(ivv.returns, width = length(ivv.returns.test),GarchFcst3,by = 1))

# prediction4 <- data.frame(rollapply(ivv.returns, width = length(ivv.returns.test),GarchFcst4,by = 1))

prediction5 <- data.frame(rollapply(ivv.returns, width = length(ivv.returns.test),GarchFcst5,by = 1))
sd.prediction5 <- data.frame(rollapply(ivv.returns, width = length(ivv.returns.test),GarchStdevFcst5,by = 1))
pred5.ts <- as.xts(prediction5)
sd.pred5.ts <- as.xts(sd.prediction5)



