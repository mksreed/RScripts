install.packages("rjags")
library("rjags")
library("coda")
#1 Specify the Model
# 
# y_i|mu ~ N(mu,1) where i=1..n
#
# mu ~ t (0,1,1)
#
# JAGS needs models as strings
# Likelihood is within the for loop, Prior is also in the string, 
mod_string = "model {
	for(i in 1:n){
		y[i]~dnorm(mu,1.0/sig2)
	}
 #	mu~dt(0.0,1.0/1.0,1)
	mu ~ dnorm(0.0,1/20)
	sig2=dnorm(20.0,1/1)
}"

#2 Setup the model
set.seed(50)
y=c(1.2,1.4,-0.5,0.3,0.9,2.3,1.0,0.1,1.3,1.9)
n=length(y)
# Variables into JAG is as a list and parameters are in 'params'
data_jags=list(y=y,n=n)
params=c("mu", "sig2")

#initial value for mu, can be static or random.
inits=function(){
	inits=list("mu"=0.0)
}

mod=jags.model(textConnection(mod_string),data=data_jags,inits=inits)


#3. Run the MCMC sampler
update(mod,500)
mod_sim= coda.samples(model=mod, variable.names=params,n.iter=5000)

# 4. Post processing
plot(mod_sim,ask=TRUE)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
summary(mod_sim)
#################################################
mean(y)
var(y)



