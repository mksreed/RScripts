tempwd=getwd()
setwd("C:/WSS/R/rspace")
install.packages("dplyr")
library("dplyr")
###############################################
setwd("C:/Users/msreed/Documents/personal/Cousera/RProgramming/Week4")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
colnames(outcome)
str(outcome)
nrow(outcome)
ncol(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
outcome[,2] # Hospital names
outcome[,7] # States
hospital <- read.csv("hospital-data.csv",colClasses="character")
colnames(hospital)
df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
df1<-data.frame(hospital=df[,2],state=df[,8],mortality=df[,11])
colnames(df1)
str(df1)

################################################
best<-function(state,outcome){
	outcomes=c("heart attack","heart failure","pneumonia")
	outcomes_index=c(11,17,23)
	if(!any(state==state.abb))
	{
		print("Invalid State")
		return("")
	}
	if(!any(outcome==outcomes))
	{
		print("Invalid Outcome")
		return("")
	}
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	icol=outcomes_index[which(outcomes==outcome)]
	df1<-df[,icol][df$State==state]
	df0<-df[,2][df$State==state]
	df2<-(as.numeric(df1))
	df2[is.na(df2)]=100
	irow<-which(df2==min(df2))
	df0[irow]
}
setwd("C:/Users/msreed/Documents/personal/Cousera/RProgramming/Week4")
best("TN", "heart attack")
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
##################################################
rankhospital<-function(state,outcome,num="best"){

	outcomes=c("heart attack","heart failure","pneumonia")
	outcomes_index=c(11,17,23)
	if(!any(state==state.abb))
	{
		print("Invalid State")
		return("")
	}
	if(!any(outcome==outcomes))
	{
		print("Invalid Outcome")
		return("")
	}
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	icol=outcomes_index[which(outcomes==outcome)]
	df1<-df[c(2,7,icol)]
   	df2<-subset(df1,State==state)

 	df3=data.frame(df2[,1],df2[,2],as.numeric(df2[,3]))
	df4<-df3[complete.cases(df3),]
	colnames(df4)<-c("Hospital","State","Rate")
	attach(df4)
 	df5<-df4[order(Rate,Hospital),]
#	str(df5)
	if(num=="best") ihospital=1
	if(num=="worst") ihospital=nrow(df5)
	if(is.numeric(num)) ihospital=as.numeric(num)
	df5$Hospital[ihospital]
}
rm(df)
setwd("C:/Users/msreed/Documents/personal/Cousera/RProgramming/Week4")
rankhospital("TX","heart failure",10)
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
##################################################
rankall <- function(outcome, num = "best") {
	outcomes=c("heart attack","heart failure","pneumonia")
	outcomes_index=c(11,17,23)
	if(!any(outcome==outcomes)) stop("Invalid Outcome")
	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	icol=outcomes_index[which(outcomes==outcome)]
	df1<-df[c(2,7,icol)]
  	df2=data.frame(df1[,1],df1[,2],as.numeric(df1[,3]))
 	df3<-df2[complete.cases(df2),]
 	colnames(df3)<-c("Hospital","State","Rate")
 	attach(df3)
  	df4<-df3[order(State,Rate,Hospital),]
# 	str(df4)
 	if(num=="best") ihospital=1
 	if(num=="worst") ihospital=nrow(df4)
	if(is.numeric(num)) ihospital=as.numeric(num)
	statelist<-"State"
	hospitallist<-"Hospital"
	df6 <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
	for(istate in unique(df4$State)){
		df5<-subset(df4,State==istate)
		statelist<-c(statelist,istate)
		hospital<-df5$Hospital[ihospital]
		df6<-rbind(df6,data.frame(hospital=hospital,state=istate))		
	}
 	df6
}
setwd("C:/Users/msreed/Documents/personal/Cousera/RProgramming/Week4")
rankall("heart attack",20)
##################################################
df<-data.frame(x=(1:200),y=state.abb,z=rnorm(200),zz=runif(200))
df1<-df[c(1,2)]
df1<-data.frame(df$x[df$z>1],df$y[df$z>1])
df1<-data.frame(df[][df$z>1])
df1
str(df)
min(df$x[(df$y=="IL") & (df$z>2)])
min(df[,1][(df$y=="IL")])
ll=(rnorm(20))
which(ll==min(ll))
y=state.abb
class(y)




	df <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
	 he = data.frame(NN=c(11,17,23), OO=c("heart attack", "heart failure", "pneumonia"))
	df1=df[c(2,7,11)]
	str(df2)
	colnames[df]
	df2<-subset(df1,State=="TX")
	df2[order(df2[,3])]
	nrow(df1)
	nrow(df)
	df1<-df[,][which(df$State=="TN")]
	icol=outcomes_index[which(outcomes==outcome)]
	df1<-df[,icol][df$State==state]
	df2<-(as.numeric(df1))
	df2[is.na(df2)]=100

