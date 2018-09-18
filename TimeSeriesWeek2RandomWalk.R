x=NULL
set.seed=100
x[1]=2
for (i in 2:1000){
	x[i]=x[i-1]+rnorm(1)
}
#print (x)
plot (x)
random_walk=ts(x)
plot(random_walk,main="Random Walk",ylab=" ", xlab="Days", col="blue")
acf(random_walk)
plot(diff(random_walk)) # gives difference 
acf((diff(random_walk)))
##########################QUIZ ####################
set.seed(2017)
t=seq(0,1,1/100)
ts1=ts(2+3*t+rnorm(length(t)))
plot(ts1)
(acf(ts1,type='covariance'))
(acf(ts1))
#--------------------
set.seed(2^10)
z=NULL
z=rnorm(1000)
data=NULL
for(i in 4:1000){
data[i-3]=z[i]+0.2*z[i-1]+0.3*z[i-2]+0.4*z[i-3]
}
data=ts(data)
plot(data)
(acf(data))

################################################
# Generate Noise
noise=rnorm(10000)

#Introduce a variable
ma_2=NULL

#Loop for generating Moving Avergae(2) process
for (i in 3:10000){
	ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}

# shift data to left by 2 units
moving_average_process=ma_2[3:10000]
plot(moving_average_process)

#time series structure
moving_average_process=ts(moving_average_process)

#Partition output graphics as a multi frame of 2 rows and 1 column
par(mfrow=c(2,1))

#plot the process and plot its ACF
plot(moving_average_process,main='A moving average process of order 2',ylab=' ',col='blue')
acf(moving_average_process,main='Correlogram of a moving average process of order 2')
##############################################################################
