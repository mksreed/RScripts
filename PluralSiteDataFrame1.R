###############Create DataFrame#####################
df<-data.frame()
df<-data.frame(a=c(1,2,3),b=c(4,5,6),c=c("aa","bb","cc")) #  vectors (same length) becomes  columns
dim(df) # returns dimension
head(df,8);tail(df,8);str(df) # displays top/bottom 8 (6 is default) rows and structure
summary(df)  # gives summary like mean etc for numerical data
names(df);colnames(df)  # returns column names
names(df)=c("x","y","z") # assign column names
df<-rbind(df,c(8,9,"cc"))
df<-cbind(df,u=c(11,12,13,14))
rownames(df)  # gives the rownames, immutable hence cannot assign
nrow(df);ncol(df) 	# num of rows and columns
################ Slicing Dataframes###################
df[3,2]; df$y[3] 	# 3rd row 2nd col
df[4,]		# 4rth row a subset df
df[,3];df[,1]	# 3rd column as a vector
df[3:4,]		# data frame with 2 rows 3 and 4
df[c(1,3,4),] 	# data frame with rows 1,3 and 4	
df[,c(1,3,4)]     # data frame with columns 1,3 and 4
df[c(1,3,4),c(2,4)] # data frame with rows 1,3, 4 and cols 2 and 4
df[2,1]==df[2,][1];df[2,1]==df$x[2]; df[2,1]==df[,1][2] # all same
################## Filtering Rows and changing colname by Boolean logic ###########
df[TRUE,]			# returns all rows
df[,TRUE]			# returns all columns
df[FALSE,]			# return col names and a message
df[,FALSE]			# returns a message
df[df$x>2,]			# returns rows for which x>2 as a dataframe
mtcars[mtcars$mpg>20,]	# returns all rows for which mpg >20
mpg_20<-mtcars$mpg>20	# returns a boolean vector
hp_100<-mtcars$hp>100	# returns a boolean vector
mpg_20&hp_100		# returns a boolean vector after AND condition
mpg_20|hp_100		# returns a boolean vector after  OR condition
mtcars[mtcars$mpg>20 & mtcars$hp>100,] # returns a dataframe
mtcars$disp[mtcars$hp>50]	# returns a vector
names(df)[names(df)=="x"]<-"X"	# updates the colname of x with X 
############## Dplyr Package#########################
install.packages("dplyr")
library(dplyr)
mtcars[,c(1,2)];select(mtcars,mpg,cyl)		# same output
select(mtcars,-mpg)					# remove mpg col
select(df,Y=y)						# select col y and rename colname from y to Y
rename(df,Z=z)						# rename col name z to Z
select(mtcars,Miles=mpg,disp)				# select mpg and disp, and renames colname mpg to Miles
filter(mtcars,mpg>20,hp>100)				# returns dataframe sans the rownames
mtcars$make_model=rownames(mtcars); filter(mtcars,mpg>20,hp>100)	# So add the make_model and then apply the filter				
mtcars%>% select(mpg,wt) %>% arrange(wt)		# pipe df to first command, then pipe output to the next command.			

