f1=read.csv(file="Anscombe.csv)
attach(f1)
plot()
cor(x1,y1)
points(x1,y1,col="red",pch=19,cex=1.5)
points(x2,y2,col="blue",pch=19,cex=1.5)
points(x3,y3,col="green",pch=19,cex=1.5)
points(x4,y4,col="black",pch=19,cex=1.5)
par(mfrow=c(2,2))
plot(x1,y1,col="red",pch=19,cex=1.5)
plot(x2,y2,col="blue",pch=19,cex=1.5)
plot(x3,y3,col="green",pch=19,cex=1.5)
plot(x4,y4,col="black",pch=19,cex=1.5)
fl.rownames
# Colors for different datatypes
df=data.frame(rnorm(20),rnorm(20),rep(c(1,2,3),c(7,7,6)))
colnames(df)=c("xx","yy","zz")
statcol=c("red","green","blue")
attach(df)
plot(xx,yy)
plot(xx,yy,col=statcol[zz],pch=20,cex=2)
###Colors with melted data
library(reshape2)
data(mtcars)
attach(mtcars)
df<-mtcars[c("mpg","cyl","wt")]
attach(df)
df1<-df[order(cyl),]
df1=melt(df,id.vars=c("cyl","gear"))
attach(df1)
statcol=c("red","blue","green")
plot(wt,mpg,col=statcol[cyl],pch=20,cex=1.5)
#
# 4 figures arranged in 2 rows and 2 columns
attach(mtcars)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
# 4 figures arranged in 2 rows and 2 columns
attach(mtcars)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
# One figure in row 1 and two figures in row 2
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
# One figure in row 1 and two figures in row 2
# row 1 is 1/3 the height of row 2
# column 2 is 1/4 the width of the column 1 
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), 
   widths=c(3,1), heights=c(1,2))
hist(wt)
hist(mpg)
hist(disp)
# Add boxplots to a scatterplot
par(fig=c(0,0.8,0,0.8), new=TRUE)
plot(mtcars$wt, mtcars$mpg, xlab="Car Weight",
  ylab="Miles Per Gallon")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(mtcars$mpg, axes=FALSE)
mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)
#########################################################
