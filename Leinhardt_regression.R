install.packages("car")
library("car")
?Leinhardt 
# income Per-capita income in U. S. dollars. 
# infant Infant-mortality rate per 1000 live births. 
# region A factor with levels: Africa; Americas; Asia, Asia and Oceania; Europe. 
# oil Oil-exporting country. A factor with levels: no, yes. 
head(Leinhardt)
str(Leinhardt)
#generate a scatter plot for each pair
pairs(Leinhardt)
#focus only on income vs infant
plot(infant~income,data=Leinhardt)
# Examine each of those variables.
hist(Leinhardt$infant)
hist(Leinhardt$income)
# Since they are right squewed and all postive, switch to log scale
Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)
plot(loginfant ~ logincome, data=Leinhardt) # looks good in log scale for linear regression
#
#modeling ( no prior was used, so un-informative flat prior)
lmod = lm(loginfant ~ logincome, data=Leinhardt)
summary(lmod)
# Delete missing value
dat = na.omit(Leinhardt)
########################################################
# JAGS
########################################################
library("rjags")
mod1_string = " model {
    for (i in 1:n) {							#Likelihood of the data
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] 
    }   
    for (i in 1:2) {				# priors for the coefficients
        b[i] ~ dnorm(0.0, 1.0/1.0e6)	# Large variance , so essentially non-informative
    }    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)		  # prior for the variance.
    sig2 = 1.0 / prec                             # So variance in Inverse Gamma
    sig = sqrt(sig2)					  # std var.
} "
#
# set up the model
#
set.seed(72)
# Specify the data set
data1_jags = list(y=dat$loginfant, n=nrow(dat),  log_income=dat$logincome)
#parameters to monitor
params1 = c("b", "sig")
# Initial value of different parameters.
inits1 = function() {
    inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
# running three chains.
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
#
# Model is initalized now. Now give a burn-in run. 
#
update(mod1, 1000) # burn-in
# actual posterior simulation that we store for analysis, now.
mod1_sim = coda.samples(model=mod1,variable.names=params1,n.iter=5000)  # 5000 interations
mod1_csim = do.call(rbind, mod1_sim) 						# combine multiple chains
#
# Convergence Analysis ############################
#
plot(mod1_sim,ask=TRUE) #trace plots, verify no long term trends are in the plots.
# gelman and rubin diagnostic , psrf is close 1,
# hence could have converged for each var and combined also.
gelman.diag(mod1_sim)
#
# High values is not good. Effective sample size( large means mixed well)
# Low size values are okay if you looking for a posterior mean. 
# For 95% interval probaility interval we may need to run further to get 
# bigger sample sizes. 
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
#
#  ----Compare simple linear model vs JAGS
summary(mod1_sim)
summary(lmod)
#
# Residual Analysis
# Checking residuals (the difference between the response and the model�s prediction for that value) 
# is important with linear models since residuals can reveal violations of the assumptions we made to 
# specify the model. In particular, we are looking for any sign that the model is not linear, 
# normally distributed, or that the observations are not independent (conditional on covariates).
# First, let�s look at what would have happened if we fit the reference linear model 
# to the un-transformed variables. 
lmod0 = lm(infant ~ income, data=Leinhardt)
plot(resid(lmod0)) # to check independence (looks okay, no special pattern discernable)
plot(predict(lmod0), resid(lmod0)) # to check for linearity, constant variance (looks bad,residual variance not constant)
qqnorm(resid(lmod0)) # to check Normality assumption (we want this to be a straight line)
#
# Look at residuals for log transformed variable.
X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income)
head(X)
(pm_params1 = colMeans(mod1_csim)) # posterior mean
yhat1 = drop(X %*% pm_params1[1:2])  # output is dropped into a vector format by drop function.
resid1 = data1_jags$y - yhat1    
plot(resid1) # against data index, looks random, no patterns or trends.
plot(yhat1, resid1) # against predicted values, looks random, no patterns or trends. 2 outliers
qqnorm(resid1) # checking normality of residuals  , should be linear. Looks reasonable.
plot(predict(lmod), resid(lmod)) # to compare with reference linear model
rownames(dat)[order(resid1, decreasing=TRUE)[1:5]] # which countries have the largest positive residuals?
