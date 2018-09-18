dat = read.table(file="HierarchicalmodelCookiesdata.txt", header=TRUE)
head(dat)
table(dat$location)
#We can also visualize the distribution of chips by location.
hist(dat$chips)
boxplot(chips ~ location, data=dat)
#
# Monte Carlo Simlation for testing some priors.
#
set.seed(112)
n_sim = 500
alpha_pri = rexp(n_sim, rate=1.0/2.0)
beta_pri = rexp(n_sim, rate=5.0)
mu_pri = alpha_pri/beta_pri
sig_pri = sqrt(alpha_pri/beta_pri^2)
summary(mu_pri)
summary(sig_pri)
# 
# After simulating from the priors for and , we can use those samples to simulate further down the hierarchy:
#
lam_pri = rgamma(n=n_sim, shape=alpha_pri, rate=beta_pri)
summary(lam_pri)
# 
# Or for a prior predictive reconstruction of the original data set:
#
(lam_pri = rgamma(n=5, shape=alpha_pri[1:5], rate=beta_pri[1:5]))
(y_pri = rpois(n=150, lambda=rep(lam_pri, each=30)))
summary(lam_pri)
#y_pri gives the original distribution if it was 
#
# JAGS
#
library("rjags")
mod_string = " model {
for (i in 1:length(chips)) {
chips[i] ~ dpois(lam[location[i]])
}
for (j in 1:max(location)) {
lam[j] ~ dgamma(alpha, beta)
}
alpha = mu^2 / sig^2
beta = mu / sig^2
mu ~ dgamma(2.0, 1.0/5.0)
sig ~ dexp(1.0)
} "
set.seed(113)
data_jags = as.list(dat)
params = c("lam", "mu", "sig")
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)
mod_sim = coda.samples(model=mod,variable.names=params,n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))
## convergence diagnostics
plot(mod_sim,ask=TRUE)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
## compute DIC
dic = dic.samples(mod, n.iter=1e3)
## observation level residuals
(pm_params = colMeans(mod_csim))
yhat = rep(pm_params[1:5], each=30)
resid = dat$chips - yhat
plot(resid)
plot(jitter(yhat), resid)
var(resid[yhat<7])
var(resid[yhat>11])
## location level residuals
lam_resid = pm_params[1:5] - pm_params["mu"]
plot(lam_resid)
abline(h=0, lty=2)
summary(mod_sim)
#
# Posterior disctribution of lamda ( from posterior of mu and sig get )
#
(n_sim = nrow(mod_csim))
lam_pred = rgamma(n=n_sim, shape=mod_csim[,"mu"]^2/mod_csim[,"sig"]^2,
rate=mod_csim[,"mu"]/mod_csim[,"sig"]^2)
hist(lam_pred)
mean(lam_pred > 15)
y_pred = rpois(n=n_sim, lambda=lam_pred)
hist(y_pred)
mean(y_pred > 15)
hist(dat$chips)
y_pred1 = rpois(n=n_sim, lambda=mod_csim[,"lam[1]"])
hist(y_pred1)

mean(y_pred1<7)
########################################################
dat1 = read.csv(file="pctgrowth.csv", header=TRUE)
head(dat1)
table(dat1$grp)
means_anova = tapply(dat1$y, INDEX=dat1$grp, FUN=mean)
plot(means_anova)
library("rjags")
mod1_string = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(mu[grp[i]],1/sig2)
}
for (j in 1:max(grp)) {
mu[j] ~ dnorm(muj,1.0/sig2)
}
muj ~ dnorm(0.0,1.0/25)
sig2 ~ dnorm(25.0,1.0/25)
} "
set.seed(113)
data1_jags = as.list(dat1)
params1 = c("mu", "muj", "sig2")
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, n.chains=3)
update(mod1, 1e3)
mod1_sim = coda.samples(model=mod1,variable.names=params1,n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))
## convergence diagnostics
plot(mod1_sim,ask=TRUE)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
summary(mod1_csim)
(pm_params = colMeans(mod1_csim))
yhat = rep(pm_params[1:5], each=30)
plot(means_anova)
points(pm_params[1:5],col="red")