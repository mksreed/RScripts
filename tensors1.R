###################################################################
#-----Function to plot covector lines-----------#
covect<-function(c1){
	a=c1[1]
	b=c1[2]
#	plot(NULL,xlim=c(-10,10),ylim=c(-10,10),xlab="X",ylab="Y")
	abline(v=0,h=0,col="blue",lwd=2)
	print(a+b)
	for (i in seq(-40,40,1))
	{	
 		x1=-10;x2=10
		y1=(i-a*x1)/b
		y2=(i-a*x2)/b
		lines(c(x1,x2),c(y1,y2))
	}
}
#-----------------------------------------------
plotgrid<-function(){
	plot(NULL,xlim=c(-10,10),ylim=c(-10,10),xlab="X",ylab="Y")
}
#-----------------------------------------------
plotv<-function(v1){
	x=v1[1]
	y=v1[2]
	lines(c(0,x),c(0,y),lwd=3,col="red")
}
###################################################################
c1<-c(-1,-2)
v1<-c(2,3)
c2<-c(1,1)
v2<-c(-2,-3)
plotgrid()
covect(c1)
plotv(v1)
covect(c2)
plotv(v2)

