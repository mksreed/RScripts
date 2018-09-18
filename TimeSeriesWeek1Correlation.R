# Measuring Linear Association
data(trees)
pairs(trees)
pairs(trees, pch = 21, bg = c("red"))
cov(trees)  # units can mislead you
cor(trees)  # units no longer in the picture
##################### Quiz
data=c(37,86,79,95,61,93,19,98,121,26,39,11,26,75,29,130,42,8)
mean(data)
sum(data)/length(data) 
summary(data)
library(faraway)
data(cheddar)
attach(cheddar)
m=lm(taste~H2S)
summary(m)
sum(resid(m))
sum(m$fitted)