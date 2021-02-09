##Bibliotecas - Próximas execuções - Chamada das bibliotecas
## Arquivo v15
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

BKfile <- readxl::read_excel("C:/Ana Paula/IDV3/R/Arquivos/HistoricoBK.xlsx")
##BKfile <- readxl::read_excel("C:/Users/CPU/Desktop/HistoricoBK.xlsx")

BKfile

BKfile = BKfile[!is.na(BKfile$Data),]

#BKfile['Traffic']
#BKfile = BKfile[!(BKfile$Ano %in% c(2015, 2016)),]

Traffic = ts(data = BKfile$Traffic, start = c(2015, 1), end = c(2019,12), frequency = 12)

Traffic

plot(Traffic)

plot(decompose(Traffic))

##Teste automático de raiz unitária

forecast::ndiffs(Traffic, test = "adf")

forecast::ndiffs(Traffic, test = "kpss")

forecast::ndiffs(Traffic, test = "pp")

##ACF
acf(Traffic)

##Partial ACF
pacf(Traffic)

##Critério de Van-Dale
acf(diff(Traffic))
pacf(diff(Traffic))

acf(diff(Traffic, 2))
pacf(diff(Traffic, 2))

##Testes de Raiz Unitária
tseries::adf.test(Traffic, alternative="stationary", k=0)
tseries::adf.test(Traffic, alternative="explosive", k=0)

tseries::adf.test(diff(Traffic), alternative="stationary", k=0)
tseries::adf.test(diff(Traffic), alternative="explosive", k=0)

tseries::adf.test(diff(Traffic, 2), alternative="stationary", k=0)
tseries::adf.test(diff(Traffic, 2), alternative="explosive", k=0)


## Auto Arima
fit = auto.arima(Traffic, stationary = T)
summary(fit)

## Primeira execução - Auto Arima
#fit = arima(Traffic, order = c(1, 0, 0), c(1, 0, 0))
#summary(fit)

tseries::jarque.bera.test(fit$residuals)

hist(fit$residuals)

fBasics::qqnormPlot(fit$residuals)

forecast::checkresiduals(fit)

forecast(fit, 12)

plot(forecast(fit, 12))

plot(fit)

autoplot(fit)

## Segunda execução
##fit = arima(Traffic, order = c(0, 1, 0), seasonal = c(12, 0, 0))
fit = arima(Traffic, order = c(0, 1, 0), c(1, 0, 1))
summary(fit)

tseries::jarque.bera.test(fit$residuals)

hist(fit$residuals)

fBasics::qqnormPlot(fit$residuals)

forecast::checkresiduals(fit)

forecast(fit, 12)

plot(forecast(fit, 12))

plot(fit)

autoplot(fit)

## Terceira execução
fit = arima(Traffic, order = c(1, 1, 0), c(1, 0, 1))
summary(fit)

tseries::jarque.bera.test(fit$residuals)

hist(fit$residuals)

fBasics::qqnormPlot(fit$residuals)

forecast::checkresiduals(fit)

forecast(fit, 12)

plot(forecast(fit, 12))

plot(fit)

autoplot(fit)

## Quarta execução
fit = arima(Traffic, order = c(1, 1, 1), c(0, 1, 1))
summary(fit)

tseries::jarque.bera.test(fit$residuals)

hist(fit$residuals)

fBasics::qqnormPlot(fit$residuals)

forecast::checkresiduals(fit)

forecast(fit, 12)

plot(forecast(fit, 12))

plot(fit)

autoplot(fit)

## Quinta execução
fit = arima(Traffic, order = c(1, 1, 1), c(0, 0, 1))
summary(fit)

tseries::jarque.bera.test(fit$residuals)

hist(fit$residuals)

fBasics::qqnormPlot(fit$residuals)

forecast::checkresiduals(fit)

forecast(fit, 12)

plot(forecast(fit, 12))

plot(fit)

autoplot(fit)

## Sexta execução
fit = arima(Traffic, order = c(0, 1, 0), c(2, 0, 1))
summary(fit)

tseries::jarque.bera.test(fit$residuals)

hist(fit$residuals)

fBasics::qqnormPlot(fit$residuals)

forecast::checkresiduals(fit)

forecast(fit, 12)

plot(forecast(fit, 12))

plot(fit)

autoplot(fit)

## Setima execução
fit = arima(Traffic, order = c(2, 1, 0), c(1, 0, 1))
summary(fit)

tseries::jarque.bera.test(fit$residuals)

hist(fit$residuals)

fBasics::qqnormPlot(fit$residuals)

forecast::checkresiduals(fit)

forecast(fit, 12)

plot(forecast(fit, 12))

plot(fit)

autoplot(fit)
