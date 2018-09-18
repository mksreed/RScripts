########################################################
library(astsa)
my.data=rec
?rec
# Plot rec 
plot(rec, main='Recruitment time series', col='blue', lwd=3)
# subtract mean to get a time series with mean zero
ar.process=my.data-mean(my.data)
# ACF and PACF of the process
par(mfrow=c(2,1))
acf(ar.process, main='Recruitment', col='red', lwd=3)
pacf(ar.process, main='Recruitment', col='green', lwd=3)
# order
p=2
# sample autocorreleation function r
r=NULL
r[1:p]=acf(ar.process, plot=F)$acf[2:(p+1)]
cat('r=',r,'\n')
# matrix R
R=matrix(1,p,p) # matrix of dimension 2 by 2, with entries all 1's.
# define non-diagonal entires of R
for(i in 1:p){
	for(j in 1:p){
		if(i!=j)
			R[i,j]=r[abs(i-j)]
		}
	}
R
# b-column vector on the right
b=NULL
b=matrix(r,p,1)# b- column vector with no entries
b
# solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat
#variance estimation using Yule-Walker Estimator
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
# constant term in the model
phi0.hat=mean(my.data)*(1-sum(phi.hat))
phi0.hat
cat("Constant:", phi0.hat," Coefficients:", phi.hat, " and Variance:", var.hat, '\n')
#########################################
# Time plot for Johnson&Johnson
plot(JohnsonJohnson, main='Johnson & Johnson earnings/share', col='blue', lwd=3)
# log-return of Johnson&Johnson ( without log it is not stationary)
jj.log.return=diff(log(JohnsonJohnson))
jj.log.return.mean.zero=jj.log.return-mean(jj.log.return)
 
# Plots for log-returns
par(mfrow=c(3,1))
plot(jj.log.return.mean.zero, main='Log-return (mean zero) of Johnson&Johnosn earnings per share')
acf(jj.log.return.mean.zero, main='ACF')
pacf(jj.log.return.mean.zero, main='PACF')
# Order
p=4
# sample autocorreleation function r
r=NULL
r[1:p]=acf(jj.log.return.mean.zero, plot=F)$acf[2:(p+1)]
r
# matrix R
R=matrix(1,p,p) # matrix of dimension 4 by 4, with entries all 1's.
# define non-diagonal entires of R
for(i in 1:p){
	for(j in 1:p){
		if(i!=j)
			R[i,j]=r[abs(i-j)]
		}
	}
R
# b-column vector on the right
b=matrix(r,p,1)# b- column vector with no entries
b
phi.hat=solve(R,b)[,1]
phi.hat
# Variance estimation using Yule-Walker Estimator
c0=acf(jj.log.return.mean.zero, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
# Constant term in the model
phi0.hat=mean(jj.log.return)*(1-sum(phi.hat))
phi0.hat
cat("Constant:", phi0.hat," Coefficients:", phi.hat, " and Variance:", var.hat, '\n')
###################################################################################
LakeHuron
#plot(LakeHuron)
?LakeHuron
data=LakeHuron
plot(diff(data))
par(mfrow=c(3,1))
plot(diff(data), main='Lake Huron height')
acf(diff(data), main='ACF')
pacf(diff(data), main='PACF')
p=2
r[1:p]=acf(diff(data),plot=F)$acf[2:3]
R=matrix(1,p,p)
for(i in 1:p){
	for(j in 1:p){
		if(i!=j)
			R[i,j]=r[abs(i-j)]
		}
	}
R
b=matrix(r,p,1)# b- column vector with no entries
b
phi.hat=solve(R,b)[,1]
phi.hat
c0=acf(diff(data), type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
pacf(diff(data))
