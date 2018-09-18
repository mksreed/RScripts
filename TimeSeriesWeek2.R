##Time Series Analysis and Its Applications: With R Examples (4th ed) by R.H. Shumway and D.S. Stoffer. Springer Texts in Statistics, 2017.
install.packages(astsa)
require(astsa)
help(astsa)
#######
help(jj)
class(jj)
plot(jj,type='o',main="Johnson & Johnson",xlab="Years",ylab="Earnings")
#######
help(flu)
class(flu)
plot(flu,type='o',main="Monthly pneumonia and influenza deaths in the U.S., ",xlab="Years",ylab="Deaths per 10,000 people ")
######
help(globtemp)
class(globtemp)
plot(globtemp,type='o',main="Global mean land-ocean temperature deviations , ",xlab="Years",ylab="Global mean land-ocean temperature deviations ")
######
help(globtempl)
class(globtempl)
plot(globtempl,type='o',main="Global mean land temperature deviations , ",xlab="Years",ylab="Global mean land-ocean temperature deviations ")
######
help(star)
class(star)
plot(star,type='o',main="The magnitude of a star taken at midnight for 600 consecutive days",xlab="Days",ylab="Magnitude ")
#######
set.seed=100
r1=ts(rnorm(100,0,1))
r2=ts(rnorm(100,0,1))
(acf(r1,type='covariance')) # covariance
(acf(r1)) # correlation

