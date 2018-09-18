################################BJsales
?BJsales
par(mfrow=c(3,1))
plot(BJsales)
plot(diff(BJsales))
plot(diff(diff(BJsales)))
ddata=(diff(BJsales))
dddata=diff((diff(BJsales)))
pacf(dddata)
acf(dddata)
#---
d=2
for(p in 1:4){
	for(q in 1:2){
		if(p+d+q<=6){
			model=
		}
	}
}

(acf(dddata))
(pacf(dddata))
ar(dddata)
var(dddata)