tempwd=getwd()
setwd("C:/WSS/R/rspace")
########################################################################
pollutantmean<-function(directory,pollutant,id=1:332){
j=1
if(pollutant=="sulfate") icol=2
if(pollutant=="nitrate") icol=3
	for(i in id)
	{
		tempfilename=sprintf(paste(directory,"/",sprintf("%03d",i),".csv",sep=""))
		tempdf=read.csv(tempfilename)
		if (j==1)
			totaldf=tempdf
		else
			totaldf=rbind(totaldf,tempdf)
		j=j+1
	}
	mean(na.omit(totaldf[,icol]))
}
setwd("C:/Users/msreed/Documents/personal/Cousera/RProgramming/")
print(R.version.string)
source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata","nitrate",70:72)
pollutantmean("specdata","nitrate",23)
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
###########################################################################
complete<-function(directory,id=1:332){
	j=1
	for(i in id)
	{
		tempfilename=sprintf(paste(directory,"/",sprintf("%03d",i),".csv",sep=""))
		tempdf=read.csv(tempfilename)
		if (j==1)
			totaldf=na.omit(tempdf)
		else
			totaldf=rbind(totaldf,na.omit(tempdf))
		j=j+1
	}
 	templist1=tapply(totaldf$ID,totaldf$ID,length)
	j=1
	print(paste("##","    id","nobs"))
	for(i in id)
	{
		print(sprintf("##  %2d %2d %4d",j,i,templist1[j]))	
		j=j+1
	}	
}
setwd("C:/Users/msreed/Documents/personal/Cousera/RProgramming/")
print(R.version.string)
complete("specdata",1)
complete("specdata",c(2,4,8,10,12))
complete("specdata",30:25)
complete("specdata",3)
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
cc <- complete("specdata", 54)
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
use
####################################################################
corr<-function(directory,threshold=0){
	j=1
	corrs=c()
	for(i in dir(directory))
	{
		tempfilename=sprintf(paste(directory,"/",i,sep=""))
		tempdf=read.csv(tempfilename)
		tempdfna=na.omit(tempdf)
		temprows=nrow(tempdfna)
		if(temprows>threshold)
		{
			c1<-cor(tempdfna$sulfate,tempdfna$nitrate)
			corrs=c(corrs,c1)
		}
	}
	corrs
}

setwd("C:/Users/msreed/Documents/personal/Cousera/RProgramming/")
print(R.version.string)
cr<-corr("specdata",150)
head(cr)
summary(cr)
cr<-corr("specdata",400)
head(cr)
summary(cr)
cr<-corr("specdata",5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5))], 4)
print(out)
cr

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


###########################################################################
a=matrix(seq(1:30),10,3)
as.data.frame(a)



pollutantmean("C:/Users/msreed/Documents/personal/Cousera/RProgramming/specdata/","nitrate",8:9)
setwd(tempwd)

setwd("C:/Users/msreed/Documents/personal/Cousera/RProgramming/")
t1="C:/Users/msreed/Documents/personal/Cousera/RProgramming/specdata/001.csv"
t2=read.csv(t1)
na.omit(t2)
t3="C:/Users/msreed/Documents/personal/Cousera/RProgramming/specdata/002.csv"
t4=read.csv(t3)
t5=na.omit(rbind(t2,t4))
t6=tapply(t5$ID,t5$ID,length)

groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
tapply(groups, groups, length) #- is almost the same as
table(groups)


na.omit(t5$sulfate)
mean(na.omit(t5$sulfate))
sprintf("%03d",5)
for(i in seq(1:5)) { print(i)}
dir("C:/Users/msreed/Documents/personal/Cousera/RProgramming/specdata")