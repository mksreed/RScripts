# Reading files
f <- read.table(file = "clipboard", sep = "_", header=FALSE)
write.csv(as.data.frame(table(ff.df$V1)),"temp.txt")
# Graphics
par(mfrow=c(2,1))