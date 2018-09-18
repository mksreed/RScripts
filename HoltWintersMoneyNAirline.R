###Volume of Money####
getwd()
#setwd("C:\Users\msreed\Documents\personal\Cousera\TimeSeries")
money.data = read.csv("volume-of-money-abs-definition-m.csv")
money.data.ts = ts(money.data[,2],start=c(1960,2) , frequency=12)
par(mfrow=c(3,1))
plot(money.data.ts, main="Time Plot of Volume of Money")
acf(money.data.ts, main="ACF of Volume of Money")
acf(money.data.ts, type="partial", main="PACF of Volume of Money")
#set up our transformed data and smoothing parameters
data = money.data[,2]
N = length(data)
alpha = 0.7
beta = 0.5
##prepare empty arrays so we can store values
forecast = NULL
level = NULL
trend = NULL
#initialize level and trend in a very simple way
level[1] = data [1]
trend[1] = data [2]- data [1]
#initialize forecast to get started
forecast[1] = data [1]
forecast[2] = data [2]
#loop to build forecasts
for( n in 2:N ) {
	level[n] = alpha* data [n] + (1-alpha)*(level[n-1]+trend[n-1])
	trend[n] = beta*(level[n] - level[n-1]) + (1-beta)*trend[n-1]
	forecast[n+1] = level[n] + trend[n]
}
#display your calculated forecast values
forecast[3:N]
plot(forecast[3:N])
#verify that we have recovered HoltWinters() output
m = HoltWinters(data, alpha = 0.7, beta = 0.5, gamma = FALSE)
m$fitted[,1]
plot(m, main="Holt Winters Fitting of Money Volume with Bogus Parameters")
m=HoltWinters(data, gamma = FALSE)
plot(m, main="Holt Winters Fitting of Money Volume with Optimal Parameters")
#########################################################
m=HoltWinters(x = log10(AirPassengers), beta = FALSE, gamma = FALSE)
#plot(log10(AirPassengers))
plot(m, main="Holt Winters Fitting of Money Volume with Optimal Parameters")
m$SSE