linregf=function(x,y)
{
n=length(x)
sxy=sum(x*y)
sxx=sum(x^2)
sx=sum(x)
sy=sum(y)
m=(n*sxy-sx*sy)/(n*sxx-sx^2)
b=(sy-m*sx)/n
print(m)
print(b)
}
##########################################################
# Logical Regression Data
lrn=25
lrf=5
lrm=12
lgpf=lrf/lrn
lgpm=lrm/lrn
logitf=log(lgpf/(1-lgpf))
logitm=log(lgpm/(1-lgpm))
lry=c(rep(1,lrf),rep(0,lrn-lrf),rep(1,lrm),rep(0,lrn-lrm))
lrx=c(rep(0,lrn),rep(1,lrn))
lrds=as.data.frame(cbind(lry,lrx))
names(lrds)=c("binges","gender")
lgx=lrx
lgy=log(lry/(length(lgx)-lry))
lgy=c(rep(logitf,lrf),rep(logitf,lrn-lrf),rep(logitm,lrm),rep(logitm,lrn-lrm))
# Analysis ---------------------------------------------
head(lrds)
table(lrds)
chisq.test(table(lrds))
attach(lrds)
results<-glm(lrds$binges~lrds$gender,family="binomial")
summary(results)
linregf(lgx,lgy)
##########################################################
# Basic Linear regression.
lx=seq(1,100,1)
ly=lx*3+5+0.01
lm(ly~lx)
la=3
lb=5.01
la*sum(lx^2)+lb*sum(lx)-sum(lx*ly)
la*sum(lx)+lb*length(lx)-sum(ly)
linregf(lx,ly)
##########################################################
library("rjags")
mod1_string = " model {
for (i in 1:length(y)) {
 y[i] ~ dbinom(p[i])
 logit(p[i]) = b0 + b[1]*x[i]
}
 b0 ~ dnorm(0.0, 1.0/25.0)
 for (j in 1:1) {
  b[j] ~ dnorm(0.0, 1.0/25.0)
 }
}"
set.seed(100)
data_jags = list(y=lrds$binges,x=lrds$gender)
params = c("b0", "b")
mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)
mod1_sim = coda.samples(model=mod1,variable.names=params,n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))
plot(mod1_sim, ask=TRUE)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
summary(mod1_csim)

