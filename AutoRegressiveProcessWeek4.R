install.packages("isdals")
library(isdals)
data(bodyfat)
# ?bodyfat
attach(bodyfat)
str(bodyfat)
pairs(bodyfat)
Fat.hat=predict(lm(Fat~Thigh))
Triceps.hat=predict(lm(Triceps~Thigh))
cor((Fat- Fat.hat),(Triceps- Triceps.hat))
###########################################
rm( list=ls( all = TRUE ) )
par(mfrow=c(3,1))
#----------
phi.1 = .6; phi.2 = .2; data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2)))
plot(data.ts, main=paste("Autoregressive Process with phi1=",phi.1," phi2=",phi.2) )
acf(data.ts, main="Autocorrelation Function")
acf(data.ts, type="partial", main="Partial Autocorrelation Function")
#-----------
phi.1 = .9; phi.2 = -.6; phi.3 = .3;
data.ts = arima.sim(n = 500, list(ar = c(phi.1, phi.2, phi.3)))
plot(data.ts, main= paste("Autoregressive Process with phi1=",phi.1," phi2=",phi.2," phi3=",phi.3) )
acf(data.ts, main="Autocorrelation Function")
acf(data.ts, type="partial", main="Partial Autocorrelation Function")
#------------
attach(attitude)
str(attitude)
pairs(attitude)
rcl=cbind(rating,complaints,learning)
str(rcl)
cor(rcl)
rating.hat=predict(lm(rating~learning))
complaints.hat=predict(lm(complaints~learning))
cor(rating-rating.hat,complaints-complaints.hat)
pcor(rcl)

