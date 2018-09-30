library(reshape2)
head(mtcars)
str(mtcars)
## Each variable forms a column
## Each observation forms a row
## Each table/file stores data about one kind of observation (e.g. people/hospitals).
mtcars$carname <- rownames(mtcars)
##Melting data frames
carMelt <- melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp"))
head(carMelt,n=3)
tail(carMelt,n=3)
#####Casting data frames
cylData <- dcast(carMelt, cyl ~ variable)
cylData
mtcars[mtcars$cyl>6,"carname"]
cylData <- dcast(carMelt, cyl ~ variable,mean)
cylData
###
head(InsectSprays)
tapply(InsectSprays$count,InsectSprays$spray,sum)
###
spIns =  split(InsectSprays$count,InsectSprays$spray)
spIns
sprCount = lapply(spIns,sum)
sprCount
unlist(sprCount)
sapply(spIns,sum)
##Another way - plyr package
install.packages("dplyr")
library(dplyr)
ddply(InsectSprays,.(spray),summarize,sum=sum(count))
##Creating a new variable
spraySums <- ddply(InsectSprays,.(spray),summarize,sum=ave(count,FUN=sum))
dim(spraySums)
