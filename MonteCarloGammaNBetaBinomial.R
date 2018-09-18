set.seed(32)

m=10000
a=2.0
b=1.0/3.0

theta=rgamma(n=m,shape=a,rate=b)

hist(theta,freq=FALSE)
curve(dgamma(x,shape=a,rate=b),col="blue",add=TRUE)

sum(theta)/m
mean(theta)
a/b

var(theta)
a/b^2

ind = theta < 5.0
mean(ind)

pgamma(q=5,shape=a,rate=b)

quantile(theta,probs=0.9)

se=sd(theta)/sqrt(m)

mean(theta)-2*se
mean(theta)+2*se

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#1 Simulate phi_i from Beta(2,2)
#2 Simulate y_i from Binom(10,phi_i)

m=10000
y=numeric(m)
phi=numeric(m)
for (i in 1:m){
phi[i]=rbeta(1,shape1=2,shape2=2)
y[i]=rbinom(1,10,prob=phi[i])
}

# or we can use a vectorized version

ph=rbeta(m,shape1=2,shape2=2)
y=rbinom(m,size=10,prob=phi)

# we can plot this beta-binomial distribution, the marginal distribution of y
table(y)/m
plot(table(y)/m)

#Marginal Expected value of isn
mean(y)







