## Subsetting - quick review
set.seed(13435)
X <- data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X <- X[sample(1:5),]; X$var2[c(1,3)] = NA
X
X[,1]
X[,"var1"]
X[1:2,"var2"]
X[(X$var1 <= 3 & X$var3 > 11),]
X[(X$var1 <= 3 | X$var3 > 15),]
X[which(X$var2 > 8),]
sort(X$var1)
sort(X$var1,decreasing=TRUE)
sort(X$var2,na.last=TRUE)
X[order(X$var1),]
X[order(X$var1,X$var3),]
#############plyr
library(plyr)
arrange(X,var1)
arrange(X,desc(var1))
X$var4 <- rnorm(5)
Y <- cbind(X,rnorm(5))
############### Read From WEb and Summarize
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
##download.file(fileUrl,destfile="./data/restaurants.csv",method="curl") # Curl is for MAC
download.file(fileUrl,destfile="./data/restaurants.csv")
restData <- read.csv("./data/restaurants.csv")
head(restData,n=3)
tail(restData,n=3)
summary(restData)
str(restData)
quantile(restData$councilDistrict,na.rm=TRUE)
quantile(restData$councilDistrict,probs=c(0.5,0.75,0.9))
quantile(restData$councilDistrict,probs=seq(.1,1.0,.1))
table(restData$zipCode,useNA="ifany") # last parameter display any missing value counts also
table(restData$councilDistrict,restData$zipCode)
sum(is.na(restData$councilDistrict)) # if any missing 1 else 0, so sum gives the count.
any(is.na(restData$councilDistrict))
all(restData$zipCode > 0) # checks if all greater than 0
colSums(is.na(restData)) # Lists the count of NA values in each column
all(colSums(is.na(restData))==0) 
table(restData$zipCode %in% c("21212")) # Find all zip codes == 21212
table(restData$zipCode %in% c("21212","21213")) ## Find all zip codes equal to either one of the two.
restData[restData$zipCode %in% c("21212","21213"),] # Lists only the rows that have these two zipcode values
######################## Cross Tabs using standard R datasets
data(UCBAdmissions)
DF = as.data.frame(UCBAdmissions)
summary(DF)
xt <- xtabs(Freq ~ Gender + Admit,data=DF)
xt

warpbreaks$replicate <- rep(1:9, len = 54)
xt = xtabs(breaks ~.,data=warpbreaks)
xt

ftable(xt)

fakeData = rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData),units="Mb")


