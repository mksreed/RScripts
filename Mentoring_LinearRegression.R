data(mtcars)
str(mtcars)
attach(mtcars)
#########################
fitcyl<-lm(mpg~cyl)
plot(cyl,mpg,xlab = "Cylinders", 
     ylab = "mpg", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fitcyl, lwd = 2)
predict(fitcyl,newdata=data.frame(cyl=c(5,7)))
i=1
#########################
fitwt<-lm(mpg~wt)
plot(wt,mpg,xlab = "Weight", 
     ylab = "mpg", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fitwt,lwd=2)
predict(fitwt,newdata=data.frame(wt=c(5,7)))
e<-resid(fitwt)
yhat<-predict(fitwt)
for (i in 1:length(mpg))
	lines(c(wt[i],wt[i]),c(mpg[i],yhat[i]),col="red", lwd=2)
max(abs(e-(mpg-yhat)))
#---- plot residuals instead of mpg
plot(wt,e,xlab = "Weight", 
     ylab = "residuals", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)

for (i in 1:length(mpg))
	lines(c(wt[i],wt[i]),c(e[i],0),col="red", lwd=2)
abline(h=0,lwd=2)
########################
library(UsingR)
data(diamond)
library(ggplot2)
g=ggplot(diamond,aes(x=carat,y=price),)
g=g+xlab("Mass (carats)")
g=g+ylab("Price (SIN $)")
g=g+geom_point(size=6,color="black",alpha=0.2)
g=g+geom_point(size=5,color="blue",alpha=0.2)
g=g+geom_smooth(method="lm",color="red") # add regression line
######################## Non linear data
x <- runif(100, -3, 3); y <- x + sin(x) + rnorm(100, sd = .2);
plot(x, y); abline(lm(y ~ x))
g=ggplot(data.frame(x=x,y=y),aes(x=x,y=y))
g=g+geom_smooth(method="lm",color="black") # add regression line
g=g+geom_point(size=6,color="red",alpha=0.2)
## plotting residual is more useful
plot(x, resid(lm(y ~ x)));
abline(h = 0)
#######Heteroskedasticity (residual growing)
x <- runif(100, 0, 6); y <- x + rnorm(100, mean = 0, sd = .001 * x);
plot(x, y); abline(lm(y ~ x))
# Again it is more evident in residual plots
plot(x, resid(lm(y ~ x)));
abline(h = 0)
#########
e=c(resid(lm(mpg~1)),resid(lm(mpg~wt)))
fit=factor(c(rep("Itc",length(wt)),rep("Itc, slope",length(wt))))
g2=ggplot(data.frame(e=e,fit=fit),aes(y=e,x=fit,fill=fit))
g2=g2+geom_dotplot(binaxis="y",size=2,stackdir="center")
g2=g2+xlab("Fitting approach")
g2=g2+ylab("Residual price")
##########Regression thru origin.
x=-10:10;y=pi*x/10+exp(1)+rnorm(length(x),mean=0,sd=0.1)
plot(x,y);abline(lm(y~0+x),lwd=2,col="red");abline(lm(y~x),lwd=2,col="blue")
abline(h=0);abline(v=0)
abline(lm((y-exp(1))~x),lwd=2,col="green")

