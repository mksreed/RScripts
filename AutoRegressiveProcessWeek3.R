#####AutoRegressive####Process##Week3
set.seed(2016)
N=1000
phi=1
Z=rnorm(N,0,1)
X=NULL
X[1]=Z[1]
for (t in 2:N){
X[t]=Z[t]+phi*X[t-1]
}
X.ts=ts(X)
par(mfrow=c(2,1))
plot(X.ts,main="AR(1) Time Series  on White Noise, phi=0.4")
X.acf=acf(X.ts,main="AR(1) Time Series  on White Noise, phi=0.4")
##################Using arima ## AR(1)##################
phi1=0.4
X.ts=arima.sim(list(ar=c(phi1)),n=1000)
par(mfrow(2,1))
plot(X.ts,main="AR(1) Time Series  on White Noise, phi1=0.9")
X.acf=acf(X.ts,main="AR(1) Time Series  on White Noise, phi1=0.9")
##################Using arima ## AR(1)##################
phi1=0.5
phi2=0.4
X.ts=arima.sim(list(ar=c(phi1,phi2)),n=1000)
par(mfrow(2,1))
plot(X.ts,xlim=c(200,300),main=paste("AR(1) Time Series  on White Noise, phi=(",phi1," and ",phi2,")"))
X.acf=acf(X.ts,main=paste("AR(1) Time Series  on White Noise, phi=(",phi1," and ",phi2,")"))
#######################################################
