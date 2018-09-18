######Histogram
data(islands)
hist(islands,col="GRAY")
hist(log(islands),col="GRAY")
#######Stem and Leaf Plots
stem(log10(islands))
######## DotChart
dotchart(log10(islands))
#######
data(InsectSprays)
attach(InsectSprays)
plot(c(.5, 6.5), range(count))
sprayTypes <- unique(spray)
for (i in 1 : length(sprayTypes)){
y <- count[spray == sprayTypes[i]]
n <- sum(spray == sprayTypes[i])
points(jitter(rep(i, n), amount = .1), y)
lines(i + c(.12, .28), rep(mean(y), 2), lwd = 3)
lines(rep(i + .2, 2),
mean(y) + c(-1.96, 1.96) * sd(y) / sqrt(n)
)
}
#####BoxPlot
boxplot(InsectSprays)
###############
data(faithful)
d <- density(faithful$eruptions, bw = "sj")
plot(d)
##############Mosaic Plot
library(MASS)
data(caith)
caith
typeof(caith)
class(caith)
mosaicplot(caith,color=topo.colors(4),main="Mosaic plot")
#-----
library(isdals)
data(bodyfat)
attach(bodyfat)
pcor(cbind(Fat,Triceps,Thigh,Midarm))
mosaicplot(as.data.frame(cor(cbind(Fat,Triceps,Thigh,Midarm))))
mosaicplot(as.data.frame(pcor(cbind(Fat,Triceps,Thigh,Midarm))))
install.packages(ppcor)
# data
y.data <- data.frame(
				hl=c(7,15,19,15,21,22,57,15,20,18),
				disp=c(0.000,0.964,0.000,0.000,0.921,0.000,0.000,1.006,0.000,1.011),
				deg=c(9,2,3,4,1,3,1,3,6,1),
				BC=c(1.78e-02,1.05e-06,1.37e-05,7.18e-03,0.00e+00,0.00e+00,0.00e+00
              ,4.48e-03,2.10e-06,0.00e+00)
			)
# partial correlation
pcor(y.data)

