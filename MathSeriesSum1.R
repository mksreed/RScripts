#################################
fun1<-function(n){
	for (i in 1:n)
	{
		if (i==1) { s=0}
		s=s+1/i^2
	}
	return(s)
}
for (i in seq(1,10000,length=10))
{	
	print(paste(i,fun1(i)-pi^2/6))
}
##################################
C=pi/2
a=1
n=10
for (i in seq(0,10))
{
	B=C-C/n*i
	A=pi-C-B
	b=a*sin(B)/sin(A)
	c=sqrt(a^2+b^2)
#---------- Inverse Pythogeraus
	A1=C-B
	A2=C-A
	a1=a*cos(A1)
	a2=b*sin(A)
#----------
#	print(paste(a,b,c,A*180/pi,B*180/pi,C*180/pi))
	lp1=format(c(a,b,c,A*180/pi,B*180/pi,C*180/pi,a1,a2,1/a^2+1/b^2-1/a1^2),digit=2,width=5)
	print(noquote(lp1))
}
###########################################

x=rep(c(2,3,4,6),c(7,6,4,3))
y=seq(1:length(x))
plot(x,y)
lm1=lm(y~x)
abline(lm1,lwd=2,col="blue")
e=resid(lm1)
yhat=predict(lm1)
for(i in 1:length(y)){
lines(c(x[i],x[i]),c(y[i],yhat[i]),col="red",lwd=2)
}

