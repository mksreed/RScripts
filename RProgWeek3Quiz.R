library(datasets)
data(iris)
?iris
head(iris)
str(tapply)
tapply(iris$Sepal.Length,iris$Species,mean)

apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)
?mtcars
str(mtcars)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$mpg, mtcars$cyl, mean)
with(mtcars, tapply(mpg, cyl, mean))

m=sapply(split(mtcars$hp, mtcars$cyl), mean)
str(m)
m[3]-m[1]

debug(ls)
ls()