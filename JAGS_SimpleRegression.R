#co2 example
co2
str(co2)
help(co2)
class(co2) # would tell that this a time serie sobject "ts"
plot(co2, main='Atmospheric CO2 Concentration')
time(co2)
# -------Regression by hand
co2.values = as.numeric(co2)
co2.times = as.numeric( time(co2) )
SSxx = sum( (co2.times - mean(co2.times) ) * (co2.times - mean(co2.times) ) )
SSxy = sum( (co2.values - mean(co2.values) ) * (co2.times - mean(co2.times) ) )
( slope = SSxy / SSxx )
( intercept = mean(co2.values) - slope*mean(co2.times) )
co2.fitted.values = slope*co2.times + intercept
co2.fitted.residuals = co2.values - co2.fitted.values
#-----------Regression with R
co2.linear.model=lm(co2~co2.times)
( co2.residuals = resid( co2.linear.model ) )
#---- Using the par() command to see these plots together
par(mfrow=c(1,3))
( c02.residuals = resid( co2.linear.model ) )
hist(co2.residuals, main= "Histogram of CO2 Residuals")
qqnorm(c02.residuals, main= "Normal Probability Plot")
qqline(c02.residuals)
plot(c02.residuals ~ time(co2), main="Residuals on Time")
#----zoom
plot(c02.residuals ~ time(co2), xlim=c(1960, 1963), main="Zoomed in Residuals on Time")
plot(c02.residuals ~ time(co2), xlim=c(1996, 1999), main="Zoomed in Residuals on Time")
########################################################
# JAGS
########################################################
co2.times=seq(0,10,0.5)
co2.values=co2.times*5+3.3
library("rjags")
mod_string = "model {
	for (i in 1:n) {
		y[i] ~ dnorm(mu[i],prec)
		mu[i] = b0 + b1 * x[i]
	}
	b1 ~ dnorm(0.0,1.0/1.0e6)
	b0 ~ dnorm(0.0,1.0/1.0e6)
    	prec ~ dgamma(5/2.0, 5*10.0/2.0)		  # prior for the variance.
    	sig2 = 1.0 / prec                             # So variance in Inverse Gamma
    	sig = sqrt(sig2)
}"
set.seed(100)
data_jags=list(y=co2.values,n=length(co2.values),x=co2.times)
params=c("b0","b1","sig")
# running three chains.
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
#
# Model is initalized now. Now give a burn-in run. 
#
update(mod, 1000) # burn-in
mod_sim = coda.samples(model=mod,variable.names=params,n.iter=5000)  # 5000 interations
mod_csim = do.call(rbind, mod_sim) 	
#
# Convergence Analysis ############################
#
plot(mod_sim,ask=TRUE) #trace plots, verify no long term trends are in the plots.
# gelman and rubin diagnostic , psrf is close 1,
# hence could have converged for each var and combined also.
gelman.diag(mod_sim)
#
# High values is not good. Effective sample size( large means mixed well)
# Low size values are okay if you looking for a posterior mean. 
# For 95% interval probaility interval we may need to run further to get 
# bigger sample sizes. 
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)	
	
