install.packages("rjags")
library("rjags")
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
 	mu~dt(0.0,1.0/1.0,1)
	sig2=1.0
}"

#2 Setup the model
set.seed(50)


y=c(1.2,1.4,-0.5,0.3,0.9,2.3,1.0,0.1,1.3,1.9)
n=length(y)
# Variables into JAG is as a list and parameters are in 'params'
data_jags=list(y=y,n=n)
params=c("mu")

#initial value for mu, can be static or random.
inits=function(){inits=list("mu"=2.0)
}

mod=jags.model(textConnection(mod_string),data=data_jags,inits=inits)
mod1=jags.model(textConnection(mod_string),data=data_jags)

#3. Run the MCMC sampler
update(mod,500)
update(mod1,500)

mod_sim= coda.samples(model=mod, variable.names=params,n.iter=10000)
mod1_sim= coda.samples(model=mod1, variable.names=params,n.iter=10000)

# 4. Post processing
library("coda")

plot(mod_sim)
plot(mod1_sim)
summary(mod_sim)
summary(mod1_sim)


