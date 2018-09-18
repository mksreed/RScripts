a=5
b=3
for (i in 10) {
m=10000*i
theta=rbeta(m,a,b)
mean(theta/(1-theta))
}

#5
m=100000
theta=rbeta(m,a,b)
mean(theta/(1-theta))

#6
odds=(theta/(1-theta))
ind=odd>1
mean(ind)


#7
m=1000000
qnorm(0.3,0,1)
theta=rnorm(m,0,1)
quantile(theta,0.3)

#8
sqrt(5.2/5000)


