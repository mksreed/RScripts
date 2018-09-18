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
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
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
#data2_jags = list(as.list(Anscombe),n=length(Anscombe$education),)
data2_jags = list(education=Anscombe$education,n=length(Anscombe$education),
				income=Anscombe$income,young=Anscombe$young,urban=Anscombe$urban)
#parameters to monitor
params2 = c("b", "sig")
# Initial value of different parameters.
inits2 = function() {
    inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
# running three chains.
mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
#
# Model is initalized now. Now give a 
#
update(mod2, 1000) # burn-in)

# actual posterior simulation that we store for analysis.
mod2_sim = coda.samples(model=mod2,variable.names=params2,n.iter=10000)  # 5000 interations
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
yhat2=drop(X %*% pm_params2[1:3])


plot(resid(mod2_lm))
plot(predict(mod2_lm),resid(mod2_lm))
qqnorm(resid(mod2_lm))

dic.samples(mod2,n.iter=1e4)



#test
v1=seq(1,10,1)
v2=v1*5
v3=v2/3+5
v=v1+2*v2+3*v3
mod_lm=lm(v3~v1+v2+v3)

