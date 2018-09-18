library("boot")
data("urine")
?urine
head(urine)
urine
#
# Remove missing value lines
dat = na.omit(urine)
dim(urine)
dim(dat)
pairs(dat)
#
# Correlations
#
install.packages('corrplot')
library("corrplot")
Cor = cor(dat)
corrplot(Cor, type="upper", method="ellipse", tl.pos="d")
corrplot(Cor, type="lower", method="number", col="black",add=TRUE, diag=FALSE, tl.pos="n", cl.pos="n")
#
#Scaling , subtract mean and divide by sd
#
X = scale(dat[,-1], center=TRUE, scale=TRUE)
head(X)
head(X[,"gravity"])
colMeans(X)  # close to zero since we scaled.
apply(X, 2, sd) # find the column standard deviations, 1 means row, 2 for col.
#
# look for Prior, double exponential (aka Laplace)
#
ddexp = function(x, mu, tau) {0.5*tau*exp(-tau*abs(x-mu))}
curve(ddexp(x, mu=0.0, tau=1.0), from=-5.0, to=5.0, ylab="density", main="Double exponential\ndistribution") # double expone
#Initial distribution
curve(dnorm(x, mean=0.0, sd=1.0), from=-5.0, to=5.0, lty=2, add=TRUE) # normal distribution
legend("topright", legend=c("double exponential", "normal"), lty=c(1,2), bty="n")
#
# JAGS
#
library("rjags")
mod1_string = " model {
for (i in 1:length(y)) {
 y[i] ~ dbern(p[i])
 logit(p[i]) = int + b[1]*gravity[i] + b[2]*ph[i] + b[3]*osmo[i] + b[4]*cond[i] + b[5]*urea[i] + b[6]*calc[i]
}
 int ~ dnorm(0.0, 1.0/25.0)
 for (j in 1:6) {
  b[j] ~ ddexp(0.0, sqrt(2.0)) # has variance 1.0
 }
}"
set.seed(92)
head(X)
data_jags = list(y=dat$r, gravity=X[,"gravity"], ph=X[,"ph"], osmo=X[,"osmo"], cond=X[,"cond"], urea=X[,"urea"], calc=X[,"calc"])
params = c("int", "b")
mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)
mod1_sim = coda.samples(model=mod1,variable.names=params,n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))
## convergence diagnostics
plot(mod1_sim, ask=TRUE)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
## calculate DIC and Analysis for variable selection for second round.
dic1 = dic.samples(mod1, n.iter=1e3)
summary(mod1_sim)
par(mfrow=c(3,2))
densplot(mod1_csim[,1:6], xlim=c(-3.0, 3.0)) 
#
# We can see that b1,b4 and b6 have significant 
# non zero means from densplots above. b2 and b3 are similar to prior with near 0 mean b1 and b4 looked correlated before, 
# So we would keep b1,b4 and b6
#
mod2_string = " model {
 for (i in 1:length(y)) {
  y[i] ~ dbern(p[i])
  logit(p[i]) = int + b[1]*gravity[i] + b[2]*cond[i] + b[3]*calc[i]
 }
 int ~ dnorm(0.0, 1.0/25.0)
 for (j in 1:3) {
   b[j] ~ dnorm(0.0, 1.0/25.0) # noninformative for logistic regression
 }
} "
mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)
update(mod2, 1e3)
mod2_sim = coda.samples(model=mod2,variable.names=params,n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))
plot(mod2_sim, ask=TRUE)
gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)
autocorr.plot(mod2_sim)
effectiveSize(mod2_sim)
dic2 = dic.samples(mod2, n.iter=1e3)
#
# Comparisons
#
dic1  # higher penalty because of more variates.
dic2 # but we deviance is lower for 1st. So both are OK. A word of caution, the priors are not the same.
################################################
# Predictions
#
(pm_coef = colMeans(mod2_csim))
#Based on these, the point estimate for the probability of calcium oxalate crystals when gravity , cond ,
# and calc are at their average values is
1/(1 + exp(-(-0.149)))  # prediction at average value: since avg value is all 0 because we scaled
1/(1 + exp(-(-0.15+1.4*0.0-1.3*(-1.0)+1.9*(1.0)))) # prediction when  2nd var 1 SD below and 3rd var 1 SD above
#
# Compute the value at each of the data point
#
pm_Xb = pm_coef["int"] + X[,c(1,4,6)] %*% pm_coef[1:3]
phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)
plot(phat, jitter(dat$r)) # added jitter just to see better on the plot.
#
# Apply a cutoff of 0.5 for predictive probabilities
#
(tab0.5 = table(phat > 0.5, data_jags$y))
sum(diag(tab0.5)) / sum(tab0.5)
#
# Apply a cutoff of 0.3 for predictive probabilities
#
(tab0.3 = table(phat > 0.3, data_jags$y))
sum(diag(tab0.3)) / sum(tab0.3)