##Bibliotecas - Próximas execuções - Chamada das bibliotecas
library("readxl")
library("readr")

library("dplyr")

library("timeSeries")
library("fBasics")
library("forecast")
library("tseries")

library("ggplot2")
library("ggfortify")

library("lubridate")
library("chron")

library("fma")

##Carregar os arquivos

BKfile <- readxl::read_excel("C:/Users/User/Desktop/BK.xlsx")
BKfile
BKfile = BKfile[!is.na(BKfile$Data),]

Sales = ts(data = BKfile$Sales, start = c(2015, 1), end = c(2019,12), 
           frequency = 12)
Sales

plot(Sales)

plot(decompose(Sales))

##Teste automático de raiz unitária

forecast::ndiffs(Sales, test = "adf")

forecast::ndiffs(Sales, test = "kpss")

forecast::ndiffs(Sales, test = "pp")

##ACF
acf(Sales)

##Partial ACF
pacf(Sales)

##Critério de Van-Dale
acf(diff(Sales))
pacf(diff(Sales))


##Testes de Raiz Unitária
tseries::adf.test(Sales, alternative="stationary", k=0)
tseries::adf.test(Sales, alternative="explosive", k=0)

tseries::adf.test(diff(Sales), alternative="stationary", k=0)
tseries::adf.test(diff(Sales), alternative="explosive", k=0)

## Auto Arima
fit = auto.arima(Sales, stationary = T)
summary(fit)

tseries::jarque.bera.test(fit$residuals)
hist(fit$residuals)
fBasics::qqnormPlot(fit$residuals)
forecast::checkresiduals(fit)
forecast(fit, 12)
plot(forecast(fit, 12))
plot(fit)
autoplot(fit)

## Segunda execução
fit = arima(Sales, order = c(2, 2, 2), c(2, 1, 1))
summary(fit)
tseries::jarque.bera.test(fit$residuals)
hist(fit$residuals)
fBasics::qqnormPlot(fit$residuals)
forecast::checkresiduals(fit)
forecast(fit, 12)
plot(forecast(fit, 12))
plot(fit)
autoplot(fit)

## terceira execução
fit = arima(Sales, order = c(2, 1, 2), c(2, 1, 1))
summary(fit)
tseries::jarque.bera.test(fit$residuals)
hist(fit$residuals)
fBasics::qqnormPlot(fit$residuals)
forecast::checkresiduals(fit)
forecast(fit, 12)
plot(forecast(fit, 12))
plot(fit)
autoplot(fit)

## testes execução
##fit = arima(Sales, order = c(1, 0, 3), c(0, 0, 1)) auto arima
fit = arima(Sales, order = c(3, 1, 3), c(3, 1, 3))
summary(fit)
tseries::jarque.bera.test(fit$residuals)
hist(fit$residuals)
fBasics::qqnormPlot(fit$residuals)
forecast::checkresiduals(fit)
forecast(fit, 12)
plot(forecast(fit, 12))
plot(fit)
autoplot(fit)
