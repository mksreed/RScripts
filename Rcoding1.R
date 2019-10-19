*************************************
#function to print Hello World
fun_hw<-function()
{
	print("Hello World")
}
fun_hw()
*************************************
# function to print the difference between the 
# largest and the smallest in an vector
fun_ls<-function(a)
{
	i=max(a)
	j=min(a)
	print(i-j)
}
b=c(1,5,2,7,8)
fun_ls(b)
c=seq(1,10,2)
fun_ls(c)
#*************************************
# function to print an input 100 times
fun_rp<-function(a)
{
	for(i in seq(1,100))
	{
		print(a)
	}
}
fun_rp("Hello World")
fun_rp(c)
#************************************
# take input from the user and prints of it is postive number or not
fun_inp<-function()
{
	input = as.integer(readline(prompt = "Enter a number: "))
	if(input <= 0) 
	{
		print("Not a positive number")
	} 
	else
	{
		print("Positve Number")
	}
}
fun_inp()
#************************************
# take a number and prints Hello World that many times.
fun1_inp<-function()
{
	num = as.integer(readline(prompt = "Enter a  number: "))
	for(i in 1:num)
	{
		print("Hello World")
	}
}
fun1_inp()

#***************************************
## multiplication tables
f_mult<-function(a)
{
	for(i in 1:16) 
	{
		print(paste(a,'x', i, '=', a*i))
	}
}
f_mult(99)
#******************************************
## takes a string and prints characters in an increasing and the decreasing fashion
f_expand<-function(a)
{
	for(i in 1:nchar(a)) 
	{
		print(substr(a,1,i))
	}
	for(i in 1:nchar(a)) 
	{
		print(substr(a,1,nchar(a)-i))
	}
}
f_expand("asdflkjhsdfasdfghhj")
#******************************************
# Write a function to take a vector and print out the sum-mean
# Write  a function to print a three saw tooth
#*******************************************
#################################################### 09/25/2019
## Matrix creation and indexing and operations
a<-matrix(1:20,nrow=5)
for (i in 1:nrow(a))
{
	for(j in 1:ncol(a))
	{
		print(a[i,j])
	}

}
for(i in 1:ncol(a))
{
	print(sum(a[,i]))
}
for(i in 1:nrow(a))
{
	print(sum(a[i,]))
}
for (i in 1:nrow(a))
{
	sum=0
	for(j in 1:ncol(a))
	{
		sum=sum+a[i,j]
	}
	print(paste("Sum of col",i," is ", sum))
}
#################################################
st_ebb<-state.abb
st_area<-state.area
st_region<-state.region
st_matrix<-state.x77
a<-matrix<-state.x77
a["Ohio","Population"]
a[2,"Population"]	
a[2,3]
mean(a[,"Population"])	

