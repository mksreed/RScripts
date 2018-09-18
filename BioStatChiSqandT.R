################### ChiSq  CI for the variance
s2 <- 105.977 ^ 2
n <- 513
alpha <- .05
qtiles <- qchisq(c(alpha/2, 1 - alpha/2),
n - 1)
ival <- rev((n - 1) * s2 / qtiles)
####################  Interval for the sd
sqrt(ival)
sigmaVals <- seq(90, 120, length = 1000)
likeVals <- dgamma((n - 1) * s2,
shape = (n - 1)/2,
scale = 2*sigmaVals^2)
likeVals <- likeVals / max(likeVals)
plot(sigmaVals, likeVals, type = "l")
lines(range(sigmaVals[likeVals >= 1 / 8]),c(1 / 8, 1 / 8))
lines(range(sigmaVals[likeVals >= 1 / 16]),c(1 / 16, 1 / 16))
################T Dist
data(sleep)
g1 <- sleep$extra[1 : 10]
g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference)#1.67
s <- sd(difference)#1.13
n <- 10
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)$conf.int
#Starting after the code for the t interval
tStat <- sqrt(n) * mn / s
esVals <- seq(0, 1, length = 1000)
likVals <- dt(tStat, n - 1, ncp = sqrt(n) * esVals)
likVals <- likVals / max(likVals)
plot(esVals, likVals, type = "l")
lines(range(esVals[likVals>1/8]), c(1/8,1/8))
lines(range(esVals[likVals>1/16]), c(1/16,1/16))
##
muVals <- seq(0, 3, length = 1000)
likVals <- sapply(muVals,
function(mu){
(sum((difference-mu)^2) /
sum((difference-mn)^2))^(-n/2)
}
)
plot(muVals, likVals, type = "l")
lines(range(muVals[likVals>1/8]), c(1/8,1/8))
lines(range(muVals[likVals>1/16]), c(1/16,1/16))