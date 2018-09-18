data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)
boxplot(weight ~ group,data=PlantGrowth)
lmod=lm(weight~group,data=PlantGrowth)
summary(lmod)
anova(lmod)
## JAGS set up
library("rjags")
mod_string = " model {
    for (i in 1:length(y)) {							#Likelihood of the data
        y[i] ~ dnorm(mu[grp[i]], prec)
    }   
    for (j in 1:3) {				# priors for the coefficients
       mu[j] ~ dnorm(0.0, 1.0/1.0e6)	# Large variance , so essentially non-informative
    }    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)		  # prior for the variance.
    sig2 = 1.0 / prec                             # So variance in Inverse Gamma
    sig = sqrt(sig2)					  # std var.
} "
#
# set up the model --------------
#
set.seed(82)
# Specify the data set
data_jags=list(y=PlantGrowth$weight,grp=as.numeric(PlantGrowth$group))
#parameters to monitor
params=c("mu","sig")
# Initial value of different parameters.
inits = function() {
    inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
# running three chains.
mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
#
# Model is initalized now. Now give a burn-in run. 
#
update(mod, 1000) # burn-in
# actual posterior simulation that we store for analysis, now.
mod_sim = coda.samples(model=mod,variable.names=params,n.iter=5000)  # 5000 interations
mod_csim = as.mcmc(do.call(rbind, mod_sim)) 						# combine multiple chains
#
# Convergence Analysis ############################
#
plot(mod_sim) #trace plots, verify no long term trends are in the plots.
# gelman and rubin diagnostic , psrf is close 1,
# hence could have converged for each var and combined also.
gelman.diag(mod_sim)
#
# High values is not good. Effective sample size( large means mixed well)
# Low size values are okay if you looking for posterior mean. 
# For 95% interval probaility interval we may need to run further to get 
# bigger sample sizes. 
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
#
pm_params=colMeans(mod_csim)
coefficients(lmod) # to compare with non informative linear analysis
#
# Residual
yhat=pm_params[1:3][data_jags$grp]
resid=data_jags$y-yhat
plot(resid)
plot(yhat,resid)

summary(mod_sim)
#
#Below we calculate the intervals of highest posterior density for each parameter
#
HPDinterval(mod_csim) 
HPDinterval(mod_csim,0.9)

mean(mod_csim[,3] > mod_csim[,1]) # posterior probaility that grp 3 > grp 1
mean(mod_csim[,3] > 1.1*mod_csim[,1]) # check at least greater than 10% improvement with the treatment 3.

###################################################################
# Repeat the above for different variances
mod1_string = " model {
    for (i in 1:length(y)) {							#Likelihood of the data
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }   
    for (j in 1:3) {				# priors for the coefficients
       mu[j] ~ dnorm(0.0, 1.0/1.0e6)	# Large variance , so essentially non-informative
	 prec[j] ~ dgamma(5/2.0, 5*10.0/2.0)		  # prior for the variance.
       sig2[j] = 1.0 / prec[j]                             # So variance in Inverse Gamma
       sig[j] = sqrt(sig2[j])
    }    
} "
#
# set up the model --------------
#
set.seed(82)
# Specify the data set
data1_jags=list(y=PlantGrowth$weight,grp=as.numeric(PlantGrowth$group))
#parameters to monitor
params=c("mu","sig")
# Initial value of different parameters.
inits1 = function() {
    inits1 = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}
# running three chains.
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
#
# Model is initalized now. Now give a burn-in run. 
#
update(mod, 1000) # burn-in
# actual posterior simulation that we store for analysis, now.
mod1_sim = coda.samples(model=mod1,variable.names=params,n.iter=5000)  # 5000 interations
mod1_csim = as.mcmc(do.call(rbind, mod1_sim)) 						# combine multiple chains
#
# Convergence Analysis ############################
#
plot(mod1_sim) #trace plots, verify no long term trends are in the plots.
# gelman and rubin diagnostic , psrf is close 1,
# hence could have converged for each var and combined also.
gelman.diag(mod1_sim)
#
# High values is not good. Effective sample size( large means mixed well)
# Low size values are okay if you looking for posterior mean. 
# For 95% interval probaility interval we may need to run further to get 
# bigger sample sizes. 
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
#
pm1_params=colMeans(mod1_csim)
coefficients(lmod) # to compare with non informative linear analysis
#
# Residual
yhat1=pm1_params[1:3][data1_jags$grp]
resid1=data1_jags$y-yhat1
plot(resid1)
plot(yhat1,resid1)

summary(mod_sim)
#
#Below we calculate the intervals of highest posterior density for each parameter
#
HPDinterval(mod1_csim) 
HPDinterval(mod1_csim,0.9)

mean(mod1_csim[,3] > mod_csim[,1]) # posterior probaility that grp 3 > grp 1
mean(mod1_csim[,3] > 1.1*mod_csim[,1]) # check at least greater than 10% improvement with the treatment 3.
dic.samples(mod,n.iter=1e5)
dic.samples(mod1,n.iter=1e5)
summary(mod_csim)
summary(mod1_csim)

dic.samples(mod,n.iter=1e5)-dic.samples(mod1,n.iter=1e5)
mu[3]-mu[1]




