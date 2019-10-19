install.packages('forecast', dependencies = TRUE)
library(forecast)
#################################################
plot(nottem)
plot(decompose(nottem))
plot.ts(cumsum(rnorm(500)))
acf(lynx,lag.max=30)
pacf(lynx,lag.max=30)
ggseasonplot(nottem) # plot curve for each year separately
#################################################
test=ts(c(rnorm(100,2,1),rnorm(100,50,1)),start=1)
plot(test)
plot(diff(test))
#Unit Root Tests
x=rnorm(1000)
library(tseries)
adf.test(x) # Augmented Dickey Fuller test
adf.
#################################################
set.seed(50)
testts=ts(rnorm(300),start=c(1919,1),frequency=4) 
plot(testts)
# 20 observations into the future for three primitive models
meanmodel=meanf(testts,h=20)
naivemodel=naive(testts,h=20)
driftmodel=rwf(testts,h=20, drift=T) # drift set to True
plot(meanmodel,main="")
lines(naivemodel$mean,col=123,lwd=2)
lines(driftmodel$mean,col=22,lwd=2)
legend("topleft",lty=1,cex=0.5,col=c(4,123,22),legend=c("Mean","Naive","Drift"))
###################################################
Accuracy and model comparison
set.seed(95)
myts<-ts(rnorm(400),start=c(1919,1),frequency=4)
mytstrain<-window(myts,start=1919,end=1999) # took 80% for training and rest 20% for testing
plot(mytstrain)
meanmodel=meanf(mytstrain,h=20)
naivemodel=naive(mytstrain,h=20)
driftmodel=rwf(mytstrain,h=20,drift=T)
mytstest<-window(myts,start=2000) # extracting test set
accuracy(meanmodel,mytstest)      # use accuracy function to test the accuracy on the test window
accuracy(naivemodel,mytstest)
accuracy(driftmodel,mytstest)
meanmodel$residual 
naivemodel$residual[2:200]  # fisrt value is NA
driftmodel$residual[2:200] # since first value is NA
mean(meanmodel$residual) # ideally zero
var(meanmodel$residual) # ideally close to 1
hist(meanmodel$residual) # should look normal ideally
acf(meanmodel$residual)  # autocorrelation, bars should be smaller than threshold
######################ARIMA MODEL####################
plot(lynx)
acf(lynx); pacf(lynx)
auto.arima(lynx)
auto.arima(lynx,trace=T) # gives a comparison of several models(i.e,p,d,q values)
lines(etsmodelmult$fitted,col="red")
##########################Exponential Smoothing
etsmodel=ets(nottem)
plot(nottem,lwd=3)
lines(etsmodel$fitted,col="red")
plot(forecast(etsmodel,h=12))
plot(forecast(etsmodel,h=12,level=95))
etsmodelmult=ets(nottem,model="MZA") # specify if it Mutliplicative, Nonpresent or Additive
plot(nottem,lwd=3)
lines(etsmodelmult$fitted,col="red")
