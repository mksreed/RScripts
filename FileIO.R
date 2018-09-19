wd<-getwd()
# Create a folder in the home drive
if (!file.exists("data")) {
    dir.create("data")
}
#Download a file from the web
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv", method = "curl")
download.file(fileUrl, destfile = "./data/cameras.csv")
download.file(fileUrl, destfile = "cameras.csv")
list.files("./data") # or list.files() or list.files(wd)
dateDownloaded <- date()
dateDownloaded
#######################
#Example: Baltimore camera data
cameraData <- read.table("./data/cameras.csv", sep = ",", header = TRUE)
cameraData <- read.csv("./data/cameras.csv") # same as previous line.
head(cameraData)
#quote - you can tell R whether there are any quoted values quote="" means no quotes.
#na.strings - set the character that represents a missing value.
#nrows - how many rows to read of the file (e.g. nrows=10 reads 10 lines).
#skip - number of lines to skip before starting to read
#In my experience, the biggest trouble with reading flat files are 
# quotation marks ` or " placed in data values, setting quote="" often resolves these
########################
install.packages("xlsx")
library(xlsx)
library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
download.file(fileUrl, destfile = "./data/simple.xml")
list.files("./data")
doc <- xmlTreeParse("simple.xml",useInternal=TRUE)
rootNode <- xmlRoot(doc)
rootNode[[1]]
rootNode[[1]][[1]]
xmlSApply(rootNode,xmlValue)
xpathSApply(rootNode,"//food",xmlValue)
xpathSApply(rootNode,"//name",xmlValue)
#---------------------
doc <- xmlTreeParse("catalog.xml",useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
rootNode[[1]]
rootNode[[1]][[1]]
xmlSApply(rootNode,xmlValue)
xpathSApply(rootNode,"//country",xmlValue)
########################
xmlfile=xmlParse("catalog.xml")
class(xmlfile) #"XMLInternalDocument" "XMLAbstractDocument"
xmltop = xmlRoot(xmlfile) #gives content of root
class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
xmlName(xmltop) #give name of node
xmlSize(xmltop) #how many children in node
xmlName(xmltop[[1]]) #name of root's children
# have a look at the content of the first child entry
xmltop[[1]]
# have a look at the content of the 2nd child entry
xmltop[[2]]
#Root Node's children
xmlSize(xmltop[[1]]) #number of nodes in each child
xmlSApply(xmltop[[1]], xmlName) #name(s)
xmlSApply(xmltop[[1]], xmlAttrs) #attribute(s)
xmlSApply(xmltop[[1]], xmlSize) #size
##############################
library(jsonlite)
#--------------
fileUrl <- "https://api.github.com/users/jtleek/repos"
download.file(fileUrl, destfile = "./data/sample.json")
list.files("./data")
jsonData<-fromJSON("./data/sample.json")
names(jsonData)
jsonData$id
#---------------
jsonData<-fromJSON("sample1.json")
names(jsonData)
names(jsonData$quiz)
names(jsonData$quiz$maths)
names(jsonData$quiz$maths$q1)
df<-fromJSON(jsonData)
#-------
js<-toJSON(iris)
js
js<-toJSON(iris,pretty=TRUE)
js
iris2<-fromJSON(js)
head(iris2)
#####################################
k={print(10);5}
print(k)