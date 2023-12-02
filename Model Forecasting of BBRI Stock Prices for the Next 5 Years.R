install.packages("tseries")
library(TSA)
library(forecast)
library(ggplot2)
library(tseries)
library(readxl)
library(lmtest)
attach(`stock_price.(3)`)
df = Close

df_ts = ts(df)
df_ts

plot.ts(df_ts)

df_tsd1=diff(df_ts)
plot.ts(df_tsd1)

acf(df_tsd1, lag.max=20, plot=FALSE)
acf(df_tsd1, lag.max=20)

pacf(df_tsd1, lag.max=20, plot=FALSE)
pacf(df_tsd1, lag.max=20)

df_arima = arima(df_ts, order =c(0,1,1))
df_arima

checkresiduals(df_arima)
adf.test(df_arima$residuals)
jarque.bera.test(df_arima$residuals)

df_arimaforecast <- forecast(df_arima, h=5)
df_arimaforecast

plot(df_arimaforecast)
plot.ts(df_arimaforecast$residuals)

den <- density(df_arimaforecast$residual)
plot(den, frame = FALSE, col = "blue",main = "Density plot")

actual= ts(actual_price$Close)
pred = df_arimaforecast$mean

mean(actual-pred)^2

par(mfrow = c(1, 2))
plot.ts(actual_price$Close, main ="Grafik Actual Price", ylim =c(5400,5550))
plot.ts(df_arimaforecast$mean, main ='Grafik Predicted Price',ylim =c(5400,5550))
