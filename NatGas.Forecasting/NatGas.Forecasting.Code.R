library(fpp2)
library(lubridate)

Y <- read.csv("NaturalGas.csv", header = TRUE)
tail(Y)

yearA <- year(min(Y$DATE))
monthA <- month(min(Y$DATE))
yearZ <- year(max(Y$DATE))
monthZ <- month(max(Y$DATE))

Y.ts <- ts(Y[,2], start = c(yearA, monthA), 
           end = c(yearZ, monthZ), 
           frequency = 12)

tail(Y.ts) 

autoplot(Y.ts)+
  ggtitle("Natural Gas Consumption")+
  ylab("in bcf")
  
acf(Y.ts)
pacf(Y.ts)

DY <- diff(Y.ts)
tail(DY)

autoplot(DY)+
  ggtitle("Natural Gas Consumption Differenced")+
  ylab("in bcf")
  
ggseasonplot(DY)+
  ggtitle("Seasonal Plot")
  
ggsubseriesplot(DY)

fit_snaive <- snaive(DY)
summary(fit_snaive)

fcast_snaive <- forecast(fit_snaive, h = 24)
plot(fcast_snaive, include=60, showgap = FALSE)

fit_ets <- ets(Y.ts)
summary(fit_ets)

fcast_ets <- forecast(fit_ets, h = 24)
plot(fcast_ets, include=60, showgap = FALSE)

summary(fcast_ets)

fit_arima <- auto.arima(Y.ts, stepwise = TRUE, approximation = FALSE,
                        trace = TRUE)
                        
summary(fit_arima)

fcast_arima <- forecast(fit_arima, h = 24)
plot(fcast_arima, include=40, showgap = FALSE)

summary(fcast_arima)

checkresiduals(fit_arima)
checkresiduals(fit_ets)
checkresiduals(fit_snaive)



