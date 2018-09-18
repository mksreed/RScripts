dat4=matrix(c(80,60,5,100),2)
chisq.test(dat4)

dat4=matrix(c(80,60,15,30,5,10),2)
dat4
chisq.test(dat4)

dat1=matrix(c(4,1,2,6),2)
fisher.test(dat1)

dat4=matrix(c(0.53*290,140,0.35*290,100,0.12*290,50),2)
dat4=matrix(c(154,140,101,100,35,50),2)
dat4
chisq.test(dat4)

dat4=matrix(c(43,8,4,45),2)
chisq.test(dat4)

dat4=matrix(c(46,52,54,52,49,52,51,52),2)
dat4
chisq.test(dat4)

dat2=matrix(c(3,2,1,4),2)
fisher.test(dat2,alternative="g")
?fisher.test