###Simulation of AR(2) Model############
set.seed(2017)
#x(t)=phi1*x(t-1)+phi2*x(t-2)+z_t (z_t ~ N (0,sigm^2)
sigma=4
phi=NULL
phi[1:2]=c(1/3,1/2)
n=10000
ar.process=arima.sim(n,model=list(ar=c(1/3,1/2),sd=4))
ar.process[1:5]
(acf(ar.process))
r=NULL
r[1:2]=acf(ar.process)$acf[2:3]
R=matrix(1,2,2) # 2 x 2 matrix with all members=1
R[1,2]=r[1]
R[2,1]=r[1]
b=matrix(r,nrow=2,ncol=1)
#Rx=b need to solve for x
phi.hat=matrix(c(solve(R,b)[1,1],solve(R,b)[2,1]),2,1)
# Variance estimation
c0=acf(ar.process,type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat
# plot time series along with acf and pacf
par(mfrow=c(3,1))
plot(ar.process,main='Simulated AR(2)')
acf(ar.process,main='ACF')
pacf(ar.process, main='PACF')
###Simulation of AR3 Model###################
set.seed(2017)
#x(t)=phi1*x(t-1)+phi2*x(t-2)+phi3*x(t-3)+z_t (z_t ~ N (0,sigm^2)
sigma=4
phi=NULL
phi[1:3]=c(1/3,1/2,7/100)
n=10000
ar3.process=arima.sim(n,model=list(ar=c(1/3,1/2,7/100),sd=4))
ar.process[1:5]
(acf(ar.process))
r=NULL
r[1:3]=acf(ar.process)$acf[2:4]
R=matrix(1,3,3) # 3 x 3 matrix with all members=1
R[1,2]=r[1]
R[1,3]=r[2]
R[2,1]=r[1]
R[2,3]=r[1]
R[3,1]=r[2]
R[3,2]=r[1]
b=matrix(,nrow=3,ncol=1)
b[1,1]=r[1]
b[2,1]=r[2]
b[3,1]=r[3]
#Rx=b need to solve for x (for phi)
phi.hat=solve(R,b)
# Variance estimation
c0=acf(ar3.process,type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat
# plot time series along with acf and pacf
par(mfrow=c(3,1))
plot(ar3.process,main='Simulated AR(2)')
acf(ar3.process,main='ACF')
pacf(ar3.process, main='PACF')
##############Home Work##################
r=NULL
r[1:3]=c(0.8,0.6,0.2)
c0=5
R=matrix(1,3,3) # 3 x 3 matrix with all members=1
R[1,2]=r[1]
R[1,3]=r[2]
R[2,1]=r[1]
R[2,3]=r[1]
R[3,1]=r[2]
R[3,2]=r[1]
b=matrix(,nrow=3,ncol=1)
b[1,1]=r[1]
b[2,1]=r[2]
b[3,1]=r[3]
phi=solve(R,b)
var=c0*(1-sum(phi*r))
