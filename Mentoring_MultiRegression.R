#-------------------Simple multivar regression
n <- 100
x <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
y <- x + x2 + x3 + rnorm(n, sd = .1)
lm(y~x+x2+x3)
#-------------------Dummy variables
x4<-rnorm(n)
lm(y~x+x2+x3+x4) # will show a small slope since random
x5<-x2+100*x3
lm(y~x+x2+x3+x5) # linear combination , returns NA
#--------------------Factor variable
n=40
x<-seq(1,20,length=n)+rnorm(n,sd=0.1)
x1<-rep(c(0,1,2,3),c(n/4,n/4,n/4,n/4))
y<-x
fit1<-lm(y~x+factor(x1))
plot(x,y,type="n",frame=FALSE)
abline(fit1)
#--------------------
library(datasets)
e <- function(a, b) a - sum( a * b ) / sum( b ^ 2) * b
ey <- e(e(y, x2), e(x3, x2))
ex <- e(e(x, x2), e(x3, x2))
sum(ey * ex) / sum(ex ^ 2)
#------Adjustments
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2) 
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)