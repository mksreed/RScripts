library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables
mod2_lm=lm(Anscombe$education~Anscombe$income+Anscombe$young+Anscombe$urban)
plot(mod2_lm)
summary(mod2_lm)
library("rjags")

mod2_string = " model {
    for (i in 1:n) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*income[i] + b[3]*young[i] + b[4]*urban[i]
    }

    for (i in 1:4) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "
set.seed(80)
data2_jags = list(education=Anscombe$education,n=length(Anscombe$education),
				income=Anscombe$income,young=Anscombe$young,urban=Anscombe$urban)
#parameters to monitor
params2 = c("b", "sig")
# Initial value of different parameters.
inits2 = function() {
    inits = list("b"=rnorm(4,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
# running three chains.
mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
#
# Model is initalized now. Now give a 
#
update(mod2, 1000) # burn-in)
# actual posterior simulation that we store for analysis.
mod2_sim = coda.samples(model=mod2,variable.names=params2,n.iter=10000)  # 10000 interations
#
# Convergence
#
mod2_csim = do.call(rbind, mod2_sim) 
plot(mod2_sim)
gelman.diag(mod2_sim)  # gelman and rubdiagnostic , psrf is close 1, hence could have converged.
autocorr.diag(mod2_sim)
autocorr.plot(mod2_sim)
effectiveSize(mod2_sim)
summary(mod2_sim)
#
# Residual Analysis of Linear model
#
plot(resid(mod2_lm))
plot(predict(mod2_lm),resid(mod2_lm))
qqnorm(resid(mod2_lm))
#
# Residual Analysis of Bayesian
#
X = cbind(rep(1.0, data2_jags$n), data2_jags$income,data2_jags$young,data2_jags$urban)
head(X)
pm_params2=colMeans(mod2_csim)
yhat2=drop(X %*% pm_params2[1:4])
resid2=data2_jags$education-yhat2
plot(resid2)
plot(yhat2,resid2)
qqnorm(resid2)
rownames(Anscombe)[order(resid2, decreasing=TRUE)[1:5]]
dic.samples(mod2,n.iter=1e5)
################################################
# Model with urban dropped
#
mod3_string = " model {
    for (i in 1:n) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*income[i] + b[3]*young[i]
    }

    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "
set.seed(80)
data3_jags = list(education=Anscombe$education,n=length(Anscombe$education),
				income=Anscombe$income,young=Anscombe$young)
#parameters to monitor
params3 = c("b", "sig")
# Initial value of different parameters.
inits3 = function() {
    inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
# running three chains.
mod3 = jags.model(textConnection(mod3_string), data=data3_jags, inits=inits3, n.chains=3)
################################################
# Model with urban dropped +  interaction term income * youth
#
mod4_string = " model {
    for (i in 1:n) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*income[i] + b[3]*young[i] + b[4]*income[i]*young[i]
    }

    for (i in 1:4) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "
set.seed(80)
data4_jags = list(education=Anscombe$education,n=length(Anscombe$education),
				income=Anscombe$income,young=Anscombe$young)
#parameters to monitor
params4 = c("b", "sig")
# Initial value of different parameters.
inits4 = function() {
    inits = list("b"=rnorm(4,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
# running three chains.
mod4 = jags.model(textConnection(mod4_string), data=data4_jags, inits=inits4, n.chains=3)

dic.samples(mod2,n.iter=1e5)
dic.samples(mod3,n.iter=1e5)
dic.samples(mod4,n.iter=1e5)
###
head(mod2_csim)
temp=mod2_csim[,2]
head(temp)
sum(temp<0)




