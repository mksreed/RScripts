library("car")
data("Leinhardt")
?Leinhardt
str(Leinhardt)
head(Leinhardt)
dat = na.omit(Leinhardt)
dat$logincome = log(dat$income)
dat$loginfant = log(dat$infant)
str(dat)
library("rjags")
mod_string = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = a[region[i]] + b[1]*log_income[i] + b[2]*is_oil[i]
}
for (j in 1:max(region)) {
a[j] ~ dnorm(a0, prec_a)
}
a0 ~ dnorm(0.0, 1.0/1.0e6)
prec_a ~ dgamma(1/2.0, 1*10.0/2.0)
tau = sqrt( 1.0 / prec_a )
for (j in 1:2) {
b[j] ~ dnorm(0.0, 1.0/1.0e6)
}
prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig = sqrt( 1.0 / prec )
} "
set.seed(116)
data_jags = list(y=dat$loginfant, log_income=dat$logincome,
is_oil=as.numeric(dat$oil=="yes"), region=as.numeric(dat$region))
data_jags$is_oil
table(data_jags$is_oil, data_jags$region)
params = c("a0", "a", "b", "sig", "tau")
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3) # burn-in
mod_sim = coda.samples(model=mod,variable.names=params,n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combine multiple chains
## convergence diagnostics
plot(mod_sim,ask=TRUE)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
dic.samples(mod, n.iter=1e3)
summary(mod_sim)
########################################################
library("MASS")
data("OME")
?OME
head(OME)
str(OME)
table(OME$ID)
dat1 = subset(OME, OME != "N/A")
str(dat1)
dat1$OME = factor(dat1$OME) # relabel OME
dat1$ID = as.numeric(factor(dat1$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod1_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat1, weights=Trials, family="binomial")
X = model.matrix(mod1_glm)[,-1]

## Original model (that needs to be extended)
library("rjags")
mod1_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	for (j in 1:max(ID)) {
		b0[j] ~ dnorm(b00, prec_b00)
	}	
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	b00 ~ dnorm(0.0, 1.0/1.0e2)
	prec_b00 ~ dgamma(1/2.0, 1/2.0)
#	tau = sqrt( 1.0 / prec_b00 )
#	prec ~ dgamma(5/2.0, 5*10.0/2.0)
#	sig = sqrt( 1.0 / prec )	
} "
params1 = c("b00", "b0", "b", "sig", "tau")
params1 = c("b00", "b0", "b")
data1_jags = as.list(as.data.frame(X))
data1_jags$y = dat1$Correct
data1_jags$n = dat1$Trials
data1_jags$ID = dat1$ID
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, n.chains=3)
update(mod1, 1e3) # burn-in
mod1_sim = coda.samples(model=mod1,variable.names=params1,n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim)) # combine multiple chains
plot(mod1_sim,ask=TRUE)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
dic.samples(mod1, n.iter=1e3)
summary(mod1_sim)
