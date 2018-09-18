# Read from WEB pages.
con<-url("https://home.allstate.com","r")
xf<-readLines(con)
head(xf)
tail(xf)