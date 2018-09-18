#
# sample of 1000 rolls of a die
#
barplot(table(sample(1:6,1000,replace=TRUE)),col="lightblue")
var(sample(1:6,10000,replace=TRUE))
#
# sampling a die with and without replacement
#
par(mfrow=c(2,1))
hist(sample(1:1000,100,replace=TRUE))
hist(sample(1:1000,100,replace=FALSE))
#
# Central limit theorm with roll of a die
#
n=2  # sample size
nsamples=4  # no of samples
xbar=apply(matrix(sample(1:6,n*nsamples,replace=TRUE),nsamples),1,mean)
barplot(table(xbar),col="lightblue")
var(xbar) # compare with next line
var(sample(1:6,n*nsamples,replace=TRUE)) # should be var(xbar)/n
#
# Conditional Probability 3D view
#
install.packages(rgl)
install.packages(mvtnorm)
library(rgl)
library(mvtnorm)
sigma=matrix(c(8,0.25*8,0.25*8,8),2,2)
xvals=seq(-10,10,length=100)
yvals=seq(-10,10,length=100)
zvals=apply(expand.grid(xvals,yvals),1,function(w) dmvnorm(w,mean=c(0,0),sigma=sigma))
persp3d(x=xvals,y=yvals,z=zvals,col="lightblue")
planes3d(0,1,1,-5,col=grey(0.8))
##########
## Law of Large Numbers
#######
set.seed(100)
nosim=100000
x=cumsum(rnorm(nosim))/1:nosim
plot(1:nosim,x,type="l",xlab="Iteration",ylab="Average",frame=FALSE)
abline(h=0,lty=2)
