if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/restaurants.csv",method="curl")
restData <- read.csv("./data/restaurants.csv")
#--sequence command
seq(1,10,by=2) ; seq(1,10,length=3);x <- c(1,3,8,25,100); seq(along = x)
#--subsetting variables
restData$nearMe = restData$neighborhood %in% c("Roland Park", "Homeland")
table(restData$nearMe)
#---Creating binary variables
restData$zipWrong = ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong,restData$zipCode < 0)
#Creating categorical variables
restData$zipGroups = cut(restData$zipCode,breaks=quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups,restData$zipCode)
a<-rnorm(1000)
b<-cut(a,breaks=quantile(a,c(0.1,0.2,0.5)))
c<-cut(a,breaks=quantile(a))
table(b);table(c);table(c,a)
#Easier cutting
library(Hmisc)  ## Error with loading this
restData$zipGroups = cut2(restData$zipCode,g=4)
table(restData$zipGroups)
b<-cut2(a,g=4)
table(b)
#Creating factor variables
restData$zcf <- factor(restData$zipCode)
restData$zcf[1:5]
class(restData$zcf)
#Levels of factor variables
yesno <- sample(c("yes","no"),size=10,replace=TRUE)
yesnofac = factor(yesno,levels=c("yes","no"))
relevel(yesnofac,ref="no")
as.numeric(yesnofac)
##Cutting produces factor variables
library(Hmisc)
restData$zipGroups = cut2(restData$zipCode,g=4)
table(restData$zipGroups)
##Using the mutate function
library(Hmisc); library(plyr)
restData2 = mutate(restData,zipGroups=cut2(zipCode,g=4))
table(restData2$zipGroups)
##Start with reshaping
library(reshape2)
head(mtcars)
##Melting data frames
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp"))
head(carMelt,n=3)
tail(carMelt,n=3)
Casting data frames
cylData <- dcast(carMelt, cyl ~ variable)
cylData
  cyl mpg hp
1   4  11 11
2   6   7  7
3   8  14 14
cylData <- dcast(carMelt, cyl ~ variable,mean)
cylData
  cyl   mpg     hp
1   4 26.66  82.64
2   6 19.74 122.29
3   8 15.10 209.21
http://www.statmethods.net/management/reshape.html

Averaging values
head(InsectSprays)
  count spray
1    10     A
2     7     A
3    20     A
4    14     A
5    14     A
6    12     A
tapply(InsectSprays$count,InsectSprays$spray,sum)
  A   B   C   D   E   F 
174 184  25  59  42 200 
http://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/

Another way - split
spIns =  split(InsectSprays$count,InsectSprays$spray)
spIns
$A
 [1] 10  7 20 14 14 12 10 23 17 20 14 13

$B
 [1] 11 17 21 11 16 14 17 17 19 21  7 13

$C
 [1] 0 1 7 2 3 1 2 1 3 0 1 4

$D
 [1]  3  5 12  6  4  3  5  5  5  5  2  4

$E
 [1] 3 5 3 5 3 6 1 1 3 2 6 4

$F
 [1] 11  9 15 22 15 16 13 10 26 26 24 13
Another way - apply
sprCount = lapply(spIns,sum)
sprCount
$A
[1] 174

$B
[1] 184

$C
[1] 25

$D
[1] 59

$E
[1] 42

$F
[1] 200
Another way - combine
unlist(sprCount)
  A   B   C   D   E   F 
174 184  25  59  42 200 
sapply(spIns,sum)
  A   B   C   D   E   F 
174 184  25  59  42 200 
Another way - plyr package
ddply(InsectSprays,.(spray),summarize,sum=sum(count))
  spray sum
1     A 174
2     B 184
3     C  25
4     D  59
5     E  42
6     F 200
Creating a new variable
spraySums <- ddply(InsectSprays,.(spray),summarize,sum=ave(count,FUN=sum))
dim(spraySums)
[1] 72  2
head(spraySums)
  spray sum
1     A 174
2     A 174
3     A 174
4     A 174
5     A 174
6     A 174
More information
A tutorial from the developer of plyr - http://plyr.had.co.nz/09-user/
A nice reshape tutorial http://www.slideshare.net/jeffreybreen/reshaping-data-in-r
A good plyr primer - http://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/
See also the functions
acast - for casting as multi-dimensional arrays
arrange - for faster reordering without using order() commands
mutate - adding new variables