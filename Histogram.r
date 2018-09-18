
small.size.dataset=c(91,49,76,112,97,42,70, 100, 8, 112, 95, 90, 78, 62, 56, 94, 65, 58, 109, 70, 109, 91, 71, 76, 68, 62, 134, 57, 83, 66)

hist(small.size.dataset)

hist(small.size.dataset, xlab='My data points')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')
lines(density(small.size.dataset))

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')
lines(density(small.size.dataset), col='red', lwd=5)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)

hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)
lines(density(small.size.dataset), col='red', lwd=5)


