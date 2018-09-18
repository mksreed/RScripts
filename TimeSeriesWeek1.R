#TimeSeriesWeek1
install.packages('faraway')
data(rats, package='faraway')
data(rats)
str(rats)
###########
data(coagulation, package='faraway')
str(coagulation)
plot( coag ~ diet, data=coagulation)  # or
plot( coagulation$coag ~ coagulation$diet)
summary(coagulation)
hist(coagulation$coag,freq=FALSE)
ourModel = lm(coag~diet-1, coagulation)
summary(ourModel)
############
#  box plot for survival times on treatments
data(rats, package='faraway')
plot(time~treat,data=rats)
#################
# We are commenting
# c() for entering dataset in R
# data = {35, 8, 10, 23, 42}
data.1=c(35, 8, 10, 23, 42)
summary(data.1)
mean(data.1)
sum(data.1)/5
sd(data.1)
#############
small.size.dataset=c(91,49,76,112,97,42,70, 100, 8, 112, 95, 90, 78, 62, 56, 94, 65, 58, 109, 70, 109, 91, 71, 76, 68, 62, 134, 57, 83, 66)
hist(small.size.dataset, xlab='My data points', main='Histogram of my data')
hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green')
lines(density(small.size.dataset))
hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10, xlim=c(-30,200),ylim=c(0,0.05))
lines(density(small.size.dataset), col='red', lwd=5)
hist1<-hist(small.size.dataset, plot=FALSE, freq=FALSE)
sum(hist1$density)
hist(small.size.dataset,plot=F)
curve(dnorm(x,mean=mean(small.size.dataset),sd=sd(small.size.dataset)),add=T,col='red',lwd=5)
####################
set.seed(2016)  # There is a typo in the video (set.seed=2016)
Test_1_scores=round(rnorm(50, 78, 10))
Test_2_scores=round(rnorm(50, 78, 14))
plot(Test_2_scores~Test_1_scores)
plot(Test_2_scores~Test_1_scores, main='Test scores for two exams (50 students)', xlab='Test_1_scores', ylab='Test 2 scores', col='blue')
##############################
###co2 example
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
##############################
sleep
plot(extra~group, data=sleep, main = "Extra Sleep in Gossett Data by Group")
str(sleep)
attach(sleep)
extra.1=extra[group==1]
extra.2=extra[group==2]
t.test(extra.1, extra.2, paired=TRUE, alternative="two.sided")
diffs = extra.1-extra.2
qqnorm(diffs, main= "Normal Probability Plot")
qqline(diffs)
#-------
plot(extra.2~extra.1, xlab='extra sleep with drug 1', ylab='extra sleep with drug 2' , main='Extra Sleep Drug 2 against Extra Sleep Drug 1')
sleep.linear.model = lm(extra.2 ~ extra.1 )
abline(sleep.linear.model)
summary(sleep.linear.model)









