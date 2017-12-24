library(tseries)
library(fGarch)


ArmaFcst1 <- function(x){
  fit <- arima(x, order = c(2,0,0))
  fcst <- forecast(fit, h = 1)$mean[1]
  return(fcst)
}
ArmaFcst2 <- function(x){
  fit <- arima(x, order = c(1,0,1))
  fcst <- forecast(fit, h = 1)$mean[1]
  return(fcst)
}
ArmaFcst3 <- function(x){
  fit <- arima(x, order = c(2,0,1))
  fcst <- forecast(fit, h = 1)$mean[1]
  return(fcst)
}
ArmaFcst4 <- function(x){
  fit <- arima(x, order = c(1,0,2))
  fcst <- forecast(fit, h = 1)$mean[1]
  return(fcst)
}
ArmaFcst5 <- function(x){
  fit <- arima(x, order = c(0,0,2))
  fcst <- forecast(fit, h = 1)$mean[1]
  return(fcst)
}

GarchFcst1 <- function(x){
  fit <- garchFit(formula=~arma(2,0)+garch(1,1),data=x,trace=F,include.mean = FALSE,include.shape = FALSE)
  fcst <- predict(fit, n.ahead = 1)$meanForecast
  #print("1")
  return(fcst)
}
GarchFcstErr1 <- function(x){
  fit <- garchFit(formula=~arma(2,0)+garch(1,1),data=x,trace=F,include.mean = FALSE,include.shape = FALSE)
  fcst <- predict(fit, n.ahead = 1)$residuals
  #print("1")
  return(fcst)
}
GarchStdevFcst1 <- function(x){
  fit <- garchFit(formula=~arma(2,0)+garch(1,1),data=x,trace=F)
  fcst <- predict(fit, n.ahead = 1)$standardDeviation
  # print("5")
  return(fcst)
}
GarchFcst2 <- function(x){
  fit <- garchFit(formula=~arma(1,1)+garch(1,1),data=x,trace=F,include.mean = FALSE,include.shape = FALSE)
  fcst <- predict(fit, n.ahead = 1)$meanForecast
  # print("2")
  return(fcst)
}
GarchStdevFcst2 <- function(x){
  fit <- garchFit(formula=~arma(1,1)+garch(1,1),data=x,trace=F)
  fcst <- predict(fit, n.ahead = 1,mse = c("cond"))$sdDeviation
  # print("5")
  return(fcst)
}
GarchFcst3 <- function(x){
  fit <- garchFit(formula=~arma(2,1)+garch(1,1),data=x,trace=F,include.mean = FALSE,include.shape = FALSE)
  fcst <- predict(fit, n.ahead = 1)$meanForecast
  # print("3")
  return(fcst)
}
GarchFcst4 <- function(x){
  fit <- garchFit(formula=~arma(1,2)+garch(1,1),data=x,trace=F,include.mean = FALSE,include.shape = FALSE)
  fcst <- predict(fit, n.ahead = 1)$meanForecast
  # print("4")
  return(fcst)
}
GarchFcst5 <- function(x){
  fit <- garchFit(formula=~arma(0,2)+garch(1,1),data=x,trace=F,include.mean = FALSE,include.shape = FALSE)
  fcst <- predict(fit, n.ahead = 1)$meanForecast
  # print("5")
  return(fcst)
}
GarchStdevFcst5 <- function(x){
  fit <- garchFit(formula=~arma(0,2)+garch(1,1),data=x,trace=F,include.mean = FALSE,include.shape = FALSE)
  fcst <- predict(fit, n.ahead = 1)$standardDeviation
  # print("5")
  return(fcst)
}
GarchFcst6 <- function(x){
  fit <- garchFit(formula=garch(1,1),data=x)
  fcst <- predict(fit, n.ahead = 1)$meanForecast
  # print("5")
  return(fcst)
}

