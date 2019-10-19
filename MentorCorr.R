#####Random
x=seq(1:10)
y= runif(10)
cor(x,y)
x
y
plot(x,y,type="l")
##################Perfect Correlation
x=seq(1:10)
y= x
cor(x,y)
x
y
plot(x,y,type="l")
##################
##################Negatively Correlation
x=seq(1:10)
y=-x
cor(x,y)
x
y
plot(x,y,type="l")
##################
data(mtcars)
attach(mtcars)
str(mtcars)
cor(mpg,cyl)
plot(wt,mpg)
plot(mtcars$wt,mtcars$mpg)
################
cor(mtcars)
pairs(mtcars)
################
data("USArrests") 
head(USArrests)
attach(USArrests)
cor(Murder,Assault)