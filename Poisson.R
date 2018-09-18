install.packages("COUNT")
library("COUNT")
data("badhealth")
###########
?badhealth
data(badhealth)
glmbadp <- glm(numvisit ~ badh + age, family=poisson, data=badhealth)
summary(glmbadp)
exp(coef(glmbadp))
library(MASS)
glmbadnb <- glm.nb(numvisit ~ badh + age, data=badhealth)
summary(glmbadnb)
exp(coef(glmbadnb))
###############
head(badhealth)
any(is.na(badhealth))
hist(badhealth$numvisit, breaks=20, ADD=TRUE)
plot(jitter(log(numvisit)) ~ jitter(age), data=badhealth, subset=badh==0, xlab="age", ylab="log(visits)")
points(jitter(log(numvisit)) ~ jitter(age), data=badhealth, subset=badh==1, col="red")
#
# JAGS
#
library("rjags")
mod_string = " model {
for (i in 1:length(numvisit)) {
numvisit[i] ~ dpois(lam[i])
log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
}
int ~ dnorm(0.0, 1.0/1e6)
b_badh ~ dnorm(0.0, 1.0/1e4)
b_age ~ dnorm(0.0, 1.0/1e4)
b_intx ~ dnorm(0.0, 1.0/1e4)
} "
set.seed(102)
data_jags = as.list(badhealth)
params = c("int", "b_badh", "b_age", "b_intx")
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)
mod_sim = coda.samples(model=mod,variable.names=params,n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))
plot(mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
dic = dic.samples(mod, n.iter=1e3)
#
# To get a general idea of the model’s performance, we can look at predicted 
# values and residuals as usual. Don’t forget that we must apply the
# inverse of the link function to get predictions for \(\lambda\).
#
X = as.matrix(badhealth[,-1])
head(X)
X = cbind(X, with(badhealth, badh*age))
head(X)
(pmed_coef = apply(mod_csim, 2, median))
llam_hat = pmed_coef["int"] + X %*% pmed_coef[c("b_badh", "b_age", "b_intx")]
lam_hat = exp(llam_hat)
hist(lam_hat)
#
# Compare with computed with the existing value from the dataset
#
head(badhealth)
Y=cbind(badhealth,lam_hat)
head(Y)
hist(badhealth$numvisit) #, breaks=20)
#
# Rerun without the interaction term
#
mod1_string = " model {
for (i in 1:length(numvisit)) {
numvisit[i] ~ dpois(lam[i])
log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]}
int ~ dnorm(0.0, 1.0/1e6)
b_badh ~ dnorm(0.0, 1.0/1e4)
b_age ~ dnorm(0.0, 1.0/1e4)
} "
set.seed(102)
data1_jags = as.list(badhealth)
params1 = c("int", "b_badh", "b_age")
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, n.chains=3)
update(mod1, 1e3)
mod1_sim = coda.samples(model=mod1,variable.names=params1,n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))
plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
#autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
dic1 = dic.samples(mod1, n.iter=1e3)
#
# Quiz
#
exp(1.5-0.3*0.8+1.2)
ppois(21,30)
dat = read.csv(file="PoissonQuizData.csv", header=TRUE)
pairs(dat)
dat1=cbind(dat,with(dat,calls/days_active))
head(dat)
head(dat1)
pairs(dat1)
mod2_string = " model {
for (i in 1:length(calls)) {
calls[i] ~ dpois( lam[i] )
log(lam[i]) = b0 + b1*age[i] + b2*isgroup2[i]}
b0 ~ dnorm(0.0, 1.0/1e2)
b1 ~ dnorm(0.0, 1.0/1e2)
b2 ~ dnorm(0.0, 1.0/1e2)
} "
set.seed(102)
data2_jags = as.list(dat)
params2 = c("b0", "b1", "b2")
mod2 = jags.model(textConnection(mod2_string), data=data2_jags, n.chains=3)
update(mod2, 1e3)
mod2_sim = coda.samples(model=mod2,variable.names=params2,n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))
plot(mod2_sim)
summary(mod2_csim)
gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)
dic2 = dic.samples(mod2, n.iter=1e4)
dic2





