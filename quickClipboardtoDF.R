aa=a
for (i in 1:15)
{
aa=gsub("  "," ",aa)
}
aaa=strsplit(aa," ")
l=aaa
df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))
write.csv(df,"temp.txt")
