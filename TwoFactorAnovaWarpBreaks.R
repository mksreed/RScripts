data("warpbreaks")
?warpbreaks
head(warpbreaks)
warpbreaks
table(warpbreaks$wool, warpbreaks$tension)
boxplot(breaks ~ wool + tension, data=warpbreaks)
library("rjags")
#
## Loading required package: coda
#
mod1_string = " model {
 for( i in 1:length(y)) {
  y[i] ~ dnorm(mu[tensGrp[i]], prec)
 }
 for (j in 1:3) {
  mu[j] ~ dnorm(0.0, 1.0/1.0e6)
 }
 prec ~ dgamma(5/2.0, 5*2.0/2.0)
 sig = sqrt(1.0 / prec)
} "
set.seed(83)
str(warpbreaks)
data1_jags = list(y=log(warpbreaks$breaks), tensGrp=as.numeric(warpbreaks$tension))
params1 = c("mu", "sig")
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, n.chains=3)
update(mod1, 1e3)
mod1_sim = coda.samples(model=mod1,variable.names=params1,n.iter=5e3)
plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim)
summary(mod1_sim)
dic1 = dic.samples(mod1, n.iter=1e3)
############################################
# Two-way additive model
#
X = model.matrix( ~ wool + tension, data=warpbreaks)
head(X)
tail(X)
mod2_string = " model {
 for( i in 1:length(y)) {
  y[i] ~ dnorm(mu[i], prec)
  mu[i] = int + alpha*isWoolB[i] + beta[1]*isTensionM[i] + beta[2]*isTensionH[i]
 }
 int ~ dnorm(0.0, 1.0/1.0e6)
 alpha ~ dnorm(0.0, 1.0/1.0e6)
 for (j in 1:2) {
  beta[j] ~ dnorm(0.0, 1.0/1.0e6)
 }
 prec ~ dgamma(3/2.0, 3*1.0/2.0)
 sig = sqrt(1.0 / prec)
} "
data2_jags = list(y=log(warpbreaks$breaks), isWoolB=X[,"woolB"], isTensionM=X[,"tensionM"], isTensionH=X[,"tensionH"])
params2 = c("int", "alpha", "beta", "sig")
mod2 = jags.model(textConnection(mod2_string), data=data2_jags, n.chains=3)
update(mod2, 1e3)
mod2_sim = coda.samples(model=mod2,variable.names=params2,n.iter=5e3)
## convergene diagnostics
plot(mod2_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim)
# summarize, collect the DIC for this model,  compare with first one-way model.
summary(mod2_sim)
#######################################################
#
# Two-way cell means model
# In this new model, � will be a matrix with 
# six entries, each corresponding to a treatment group.
#
mod3_string = " model {
 for( i in 1:length(y)) {
  y[i] ~ dnorm(mu[woolGrp[i], tensGrp[i]], prec)
 }
 for (j in 1:max(woolGrp)) {
  for (k in 1:max(tensGrp)) {
   mu[j,k] ~ dnorm(0.0, 1.0/1.0e6)
  }
 }
 prec ~ dgamma(3/2.0, 3*1.0/2.0)
 sig = sqrt(1.0 / prec)
} "
str(warpbreaks)
data3_jags = list(y=log(warpbreaks$breaks), woolGrp=as.numeric(warpbreaks$wool), tensGrp=as.numeric(warpbreaks$tension))
params3 = c("mu", "sig")
mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)
mod3_sim = coda.samples(model=mod3,variable.names=params3,n.iter=5e3)
mod3_csim = as.mcmc(do.call(rbind, mod3_sim))
plot(mod3_sim, ask=TRUE)
## convergence diagnostics
gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
effectiveSize(mod3_sim)
raftery.diag(mod3_sim)
##########################################################
# Compare
#
(dic1 = dic.samples(mod1, n.iter=1e3))
(dic2 = dic.samples(mod2, n.iter=1e3))
(dic3 = dic.samples(mod3, n.iter=1e3))
summary(mod3_sim)
HPDinterval(mod3_csim)
par(mfrow=c(3,2)) # arrange frame for plots
densplot(mod3_csim[,1:6], xlim=c(2.0, 4.5))
prop.table( table( apply(mod3_csim[,1:6], 1, which.min) ) )