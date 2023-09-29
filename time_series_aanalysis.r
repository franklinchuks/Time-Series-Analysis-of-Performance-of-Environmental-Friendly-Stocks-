rm(list=ls())
options(scipen=999)

install.packages("vrtest")
install.packages("forecast")
install.packages("dynlm")
install.packages("FinTS")
install.packages("rugarch")
install.packages("quantmod")
install.packages("pastecs")
install.packages("tidyquant")
install.packages("astsa")
install.packages("ggplot2")

library(tseries)
library(vrtest)
library(forecast)
library(dynlm)
library(FinTS)
library(rugarch)
library(tidyquant)
library(dplyr)
library(pastecs)
library(ggplot2)
library(astsa)
library(tidyverse)


df1 <- read.csv("TSLA.csv")
d <- getSymbols("TSLA", from = "2011-01-01", to = "2022-12-31")
View(d)
rsi_tsla <- RSI(Cl(TSLA))
tsla <- ts(data.frame(Date = time(df1$Adj.Close)[-1], Price = as.vector(df1$Adj.Close[-1]), Returns = as.vector(diff(log(df1$Adj.Close))), RSI = as.vector(rsi_tsla)[-1]))
tsla_d <- df1$Date
tsla_p <- df1$Adj.Close
tsla_r <- diff(log(df1$Adj.Close))

df2 <- read.csv("TM.csv")
getSymbols("TM", from = "2011-01-01", to = "2022-12-31")
rsi_tm <- RSI(Cl(TM))
tm <- ts(data.frame(Date = time(df2$Adj.Close)[-1], Price = as.vector(df2$Adj.Close[-1]), Returns = as.vector(diff(log(df2$Adj.Close))), RSI = as.vector(rsi_tm)[-1]))
tm_d <- df2$Date
tm_p <- df2$Adj.Close
tm_r <- diff(log(df2$Adj.Close))

stat.desc(tsla)
stat.desc(tm)

qplot(sample = tsla_r, main="Tesla QQPLOT")
qplot(sample = tm_r, main="Toyota QQPLOT")

hist(tsla_r)
hist(tm_r)

qplot(tsla_r, main="Tesla Histogram")
qplot(tm_r, main="Toyota Histogram")

plot.ts(tsla_r, main="Tesla Time Series Plot")
plot.ts(tm_r, main="Toyota Time Series Plot")

boxplot(tsla_r, main="Tesla BoxPlot")
boxplot(tm_r, main="Toyota BoxPlot")

autoplot(tsla[, 2:4], facets = TRUE) + xlab("Year") + ylab("") + ggtitle("Changes in Tesla")
autoplot(tm[, 2:4], facets = TRUE) + xlab("Year") + ylab("") + ggtitle("Changes in Toyota")

adf.test(tsla_r)
adf.test(tm_r)

shapiro.test(tsla_r)
shapiro.test(tm_r)

acf(tsla_r, main="Tesla ACF of Returns")
acf(tm_r, main="Toyota ACF of Returns")

pacf(tsla_r, main="Tesla PACF of Returns")
pacf(tm_r, main="Toyota PACF of Returns")

ArchTest(tsla_r)
ArchTest(tm_r)

garch(tsla_r, grad="numerical", trace=FALSE)
tsla_garch = ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder = c(1,1)))
tsla_garch_fit = ugarchfit(tsla_garch, data = tsla_r)
ugarchforecast(tsla_garch_fit, n.head=10)

garch(tm_r, grad="numerical", trace=FALSE)
tm_garch = ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder = c(1,1)))
tm_garch_fit = ugarchfit(tm_garch, data = tm_r)
ugarchforecast(tm_garch_fit, n.head=10)

Box.test(tm_r, lag = 20, type = "Ljung-Box")

tsla_arima = arima(tsla_r, order = c(1,0,1))
tsla_arima
tsla_et = residuals(tsla_arima)
acf(tsla_et, main="Tesla ACF ARMA Test")
plot.ts(tsla_et, main="Tesla TS Plot of ARMA Test")
gghistogram(tsla_et)

tm_arima = arima(tm_r, order = c(1,0,1))
tm_arima
tm_et = residuals(tm_arima)
acf(tm_et, main="Toyota ACF ARMA Test")
plot.ts(tm_et, main="Toyota TS Plot of ARMA Test")
gghistogram(tm_et, main="Toyota Histogram Plot of ARMA Test")

tsdiag(tm_arima, main = "Toyota Motor Residuals")
dev.new()
tsdiag(tsla_arima, main = "Tesla Motor Residuals")

checkresiduals(tm_arima, main = "Toyota Motor Residuals")
dev.new()
checkresiduals(tsla_arima, main = "Tesla Motor Residuals")

autoplot(tm_arima, main = "Toyota Motor Autoplot")
dev.new()
autoplot(tsla_arima, main = "Tesla Motor Autoplot")

ggtsdisplay(tm_arima$residuals, main = "Toyota Motor ARMA Residuals")
dev.new()
ggtsdisplay(tsla_arima$residuals, main = "Tesla Motor ARMA Residuals")

forecast::auto.arima(tsla[, "Returns"], xreg=tsla[, "RSI"])
forecast::auto.arima(tm[, "Returns"], xreg=tm[, "RSI"])

tsla_armax <- arima(tsla[, "Returns"], order = c(1,0,1), xreg = tsla[, "RSI"])
tm_armax <- arima(tm[, "Returns"], order = c(1,0,1), xreg = tm[, "RSI"])
tsla_armax
tm_armax

autoplot(tsla_armax, main = "Tesla Motor ARMAX Autoplot")
dev.new()
autoplot(tm_armax, main = "Toyota Motor ARMAX Autoplot")

ggtsdisplay(tsla_armax$residuals, main = "Toyota Motor ARMAX Residuals")
dev.new()
ggtsdisplay(tm_armax$residuals, main = "Tesla Motor ARMAX Residuals")

checkresiduals(tm_arima, main = "Toyota Motor ARMAX Residuals")
dev.new()
checkresiduals(tsla_arima, main = "Tesla Motor ARMAX Residuals")


