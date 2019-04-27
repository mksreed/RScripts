pt0
  [4] "12/28/2018 3:01\t246"  "12/27/2018 3:06\t181"  "12/25/2018 3:07\t251" 
  [7] "12/24/2018 3:02\t7"    "12/22/2018 3:03\t277"  "12/21/2018 3:08\t163" 

pt0<-gsub("\t"," ",pt)
pt00=strsplit(pt0," ")

tt<-readClipboard()
tt0<-gsub("\t"," ",tt)
tt00<-strsplit(tt0," ")
tt00[[1]][1]
tt00[[1]][45]
lapply(tt00, function(x) paste(x[1],x[45]))

setNames(do.call(rbind.data.frame,pt00),c("dt","tm", "docs"))
sapply(strsplit(tt0[[1]]," "), length)
lapply(tt0, function(x) sapply(strsplit(x," "), length) )

##########################
tt<-readClipboard()
# [1] "adobe 967363"                 "cleancapsbppbpt 153455"      
# [3] "CopiedFromRootC 1594015653"   "db2lic 299810"               
# [5] "exstream 786465219"           "Exstream61 153552177
tt00<-strsplit(tt," ")
df0<-setNames(do.call(rbind.data.frame,tt00),c("dir","docs"))
##########################
d1<-read.csv("OnDemandDocCount03202018thru03202019.csv",header=TRUE)
str(d1)
aggregate(cbind(Docs,Pages)~AG, data=d1, FUN=sum)
d2<-cbind(d1,d1$Size/d1$Docs/1024,d1$Pages/d1$Docs,rep(1,length(d1$Size)))
colnames(d2)[6:8]<-c("DocSize","DocPage","Count")
d3<-aggregate(cbind(Docs,DocPage, DocSize, Count)~AG, data=d2, FUN=sum)
d4<-aggregate(cbind(Docs,DocPage, DocSize, Count)~AG, data=d2, FUN=mean)
write.csv(d3,"ODCountsSum.csv")
write.csv(d4,"ODCountsAvg.csv")

