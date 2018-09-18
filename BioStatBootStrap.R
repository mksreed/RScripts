#
##jackknife
#
n <- length(gmVol)
theta <- median(gmVol)
jk <- sapply(1 : n,
function(i) median(gmVol[-i])
)
thetaBar <- mean(jk)
biasEst <- (n - 1) * (thetaBar - theta)
seEst <- sqrt((n - 1) * mean((jk - thetaBar)^2))
### Or, using the bootstrap package
library(bootstrap)
out <- jackknife(gmVol, median)
out$jack.se
out$jack.bias
#
##	bootstrap
#
B <- 1000
n <- length(gmVol)
resamples <- matrix(sample(gmVol,n * B,replace = TRUE),B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(.025, .975))
#
#
library(boot)
stat <- function(x, i) {median(x[i])}
boot.out <- boot(data = gmVol,
statistic = stat,
R = 1000)
boot.ci(boot.out)

