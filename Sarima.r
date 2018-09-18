#############################################################
x=NULL
z=NULL
n=10000

z=rnorm(n)
x[1:13]=1

for(i in 14:n){
  x[i]<-z[i]+0.7*z[i-1]+0.6*z[i-12]+0.42*z[i-13]
}

par(mfrow=c(2,1))
plot.ts(x[12:1200], main='The first 10 months of simulation SARIMA(0,0,1,0,0)_12', ylab='') 
acf(x, main='SARIMA(0,0,1,0,0,1)_12 Simulation')
##############################################################
library(astsa)

d=1
DD=1

per=4

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, 'LogLikelihood=',model$log, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
model=sarima(log(jj), 0,1,1,1,1,0,4)

#############################################################
install.packages('forecast')
library(astsa)
library(forecast)
milk<-read.csv('monthly-milk-production-pounds-p.csv')
Milk<-na.omit(milk)$Monthly.milk.production..pounds.per.cow..Jan.62...Dec.75
sarima(Milk, 0,1,0,0,1,1,12)

d=NULL
DD=NULL
d=1
DD=1

per=12
for(p in 1:1){
  for(q in 1:1){
    for(i in 1:3){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=Milk, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
model<- arima(x=Milk, order = c(0,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))
forecast(model)
####################################################################
SUV<-read.csv('monthly-sales-for-a-souvenir-sho.csv')
suv<-ts(SUV$Sales)
library(astsa)
library(forecast)

par(mfrow=c(2,2))

plot(suv, main='Monthly sales for a souvenir shop', ylab='', col='blue', lwd=3)
plot(log(suv), main='Log-transorm of sales', ylab='', col='red', lwd=3)
plot(diff(log(suv)), main='Differenced Log-transorm of sales', ylab='', col='brown', lwd=3)
plot(diff(diff(log(suv)),12), main='Log-transorm without trend and seasonaliy', ylab='', col='green', lwd=3)
data<-diff(diff((log(suv)),12))
acf2(data, 50)
d=1
DD=1
per=12
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(suv), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
model<- arima(x=log(suv), order = c(1,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))
forecast(model)
a<-sarima.for(log(suv),12,1,1,0,0,1,1,12)
plot.ts(c(suv,exp(a$pred)), main='Monthly sales + Forecast', ylab='', col='blue', lwd=3)
############################################################################
data(USAccDeaths)
str(USAccDeaths)
?USAccDeaths
plot(USAccDeaths)
d12=diff(USAccDeaths,12)
plot(d12)
dd12=diff(d12)
plot(dd12)
par(mfrow=c(2,1))
acf(dd12)
pacf(dd12)
d=1
DD=1
per=12
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=dd12, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
model<- arima(x=USAccDeaths, order = c(0,1,1), seasonal = list(order=c(0,1,1), period=12))
sarima(USAccDeaths,0,1,1,0,1,1,12)
model=sarima(USAccDeaths,0,1,1,0,1,1,12)
a<-sarima.for(USAccDeaths,12,0,1,1,0,1,1,12)
a

