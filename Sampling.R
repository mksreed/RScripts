x <- 1:12
# a random permutation
sample(x)
# bootstrap resampling -- only if length(x) > 1 !
sample(x, replace = TRUE)

# 100 Bernoulli trials
sample(c(0,1), 100, replace = TRUE)

## More careful bootstrapping --  Consider this when using sample()
## programmatically (i.e., in your function or simulation)!

# sample()'s surprise -- example
x <- 1:10
    sample(x[x >  8]) # length 2
    sample(x[x >  9]) # oops -- length 10!
    sample(x[x > 10]) # length 0
    sample(x[x <  5]) # length 4

resample <- function(x, ...) x[sample.int(length(x), ...)]
resample(x[x >  8]) # length 2
resample(x[x >  9]) # length 1
resample(x[x > 10]) # length 0

## R 3.x.y only
sample.int(1e10, 12, replace = TRUE)
sample.int(1e10, 12) # not that there is much chance of duplicates
##########################################Regression to Mediocrity
x<-seq(70,100,1)
t1=sample(x,40,replace=TRUE)
t2=sample(x,40,replace=TRUE)
plot(t1,t2,xlab="test 1", ylab="test2",lwd=4,col="BLUE",xlim=c(70,100),ylim=c(70,100))
#-----
t1=(t1-mean(t1))/sd(t1)
t2=(t2-mean(t2))/sd(t2)
plot(t1,t2,xlab="test 1", ylab="test2",lwd=4,col="BLUE",xlim=c(min(t1),max(t1))*5,ylim=c(min(t2),max(t2))*5)
cor(t1,t2)
#--------
n=length(t1)
diff=t1-t2
mean(diff)
sd(diff)
testStat=sqrt(n)*mean(diff)/sd(diff)
2*pt(abs(testStat),n-1,lower.tail=FALSE) # COmpted the P Value
testStat
t.test(diff)

