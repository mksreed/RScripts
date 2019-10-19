####################################
n=20
x=rnorm(n,0,1)
y=x+runif(n)
plot(x,y,col="blue")
fit=lm(y~x)
abline(fit,col="red",lwd=2)
######### Regression thru origin
fit0=lm(y~x+0)
abline(h=0,lwd=2)
abline(v=0,lwd=2)
abline(fit0,col="green",lwd=2)
######## Centered data
x0=x-mean(x)
y0=y-mean(y)
fit00=lm(y0~x0)
abline(fit00,col="green",lwd=2)
points(x0,y0,col="green")
####### Residuals
xr=resid(fit)
t(x)%*%xr  should be close to zero
yp=predict(fit)
for (i in 1:length(x))
{
#print(i)
lines(c(x[i],x[i]),c(yp[i],y[i]))
}
points(mean(x), mean(y), cex = 2, bg = "lightblue", col = "black", pch = 21)
###################################
dfbetas(fit)
hatvalues(fit)
x1<-c(1,x)
x1%*%t(x1)
solve(x1%*%t(x1))
fit$coef[1]+fit$coef[2]*x
y
######################## Two Variables Matrix formualtion
n=20
x=seq(1,5,length=n)/10+rnorm(n,2,1)
y=x+runif(n)
z=x+y+runif(n)
fit2=lm(z~x+y)
dfbetas(fit2)
hatvalues(fit2)
X=matrix(c(rep(1,n),x,y),ncol=3)
(X%*%fit2$coef-z)/z*100
solve(t(X)%*%X)%*%t(X)%*%z
#################### Correlation/Covariance ###########
data(mtcars);attach(mtcars);n=length(mpg)
#-----------------Correlation
cor(mtcars)
cor(mpg,cyl)-(t(mpg-mean(mpg))%*%(cyl-mean(cyl)))/sd(mpg)/sd(cyl)/(n-1)
# or
sd1=sqrt((t(mpg-mean(mpg))%*%(mpg-mean(mpg)))/(n-1))
sd2=sqrt((t(cyl-mean(cyl))%*%(cyl-mean(cyl)))/(n-1))
cor(mpg,cyl)-(t(mpg-mean(mpg))%*%(cyl-mean(cyl)))/(n-1)/sqrt((t(mpg-mean(mpg))%*%(mpg-mean(mpg)))/(n-1))/sqrt((t(cyl-mean(cyl))%*%(cyl-mean(cyl)))/(n-1))
#----------------Covariance
cov(mtcars)
cov(mpg,cyl)-(t(mpg-mean(mpg))%*%(cyl-mean(cyl)))/(n-1)





