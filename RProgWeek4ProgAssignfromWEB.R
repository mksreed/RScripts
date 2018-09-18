best<-function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  
  if(state %in% data[,7] == FALSE) {stop("invalid state")}
  if(outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {stop("invalid outcome")}
  
  he = data.frame(NN=c(11,17,23), OO=c("heart attack", "heart failure", "pneumonia"))
  
  y = subset(he,he$OO == outcome)[,1]
  
  dataofstate <- subset(data,data[,7] == state)
  dataofmin <- subset(dataofstate,as.numeric(dataofstate[,y]) == min(as.numeric(dataofstate[,y]),na.rm=TRUE))
  
  dataofmintie <- dataofmin[c(order(dataofmin[,2])),]   ## handling ties
  dataofmintie[1,2]
  
}
###########################
rankhospital<-function(state,outcome,num="best"){
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  
  if(state %in% data[,7] == FALSE) {stop("invalid state")}
  if(outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {stop("invalid outcome")}
  
  he = data.frame(NN=c(11,17,23), OO=c("heart attack", "heart failure", "pneumonia"))
  
  y = subset(he,he$OO == outcome)[,1]
  
  dataofstate <- subset(data,data[,7] == state & is.na(data[,y]) == FALSE)
  dataofstateabc <- dataofstate[c(order(dataofstate[,2])),]
  dataofstateabc123 <- dataofstateabc[c(order(as.numeric(dataofstateabc[,y]))),]
  dataofstateabc123$Rank <- c(1:nrow(dataofstate))
  
  if(num == "best"){num<-1}
  if(num == "worst"){num<-nrow(dataofstate)}
  if(num > nrow(dataofstate)){NA}
  else{
  dataofrank <- subset(dataofstateabc123,dataofstateabc123$Rank == num)
  dataofrank[,2]}
}
################################################
rankall<-function(outcome,num="best"){
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  
  if(outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {stop("invalid outcome")}
  
  he = data.frame(NN=c(11,17,23), OO=c("heart attack", "heart failure", "pneumonia"))
  
  y = subset(he,he$OO == outcome)[,1]

  frame = data.frame(hospital=1,state=1)
  
  for(i in 1:length(levels(factor(data$State)))){
    state=levels(factor(data$State))[i]
    
    dataofstate <- subset(data,data[,7] == state & is.na(data[,y]) == FALSE)
    dataofstateabc <- dataofstate[c(order(dataofstate[,2])),]
    dataofstateabc123 <- dataofstateabc[c(order(as.numeric(dataofstateabc[,y]))),]
    dataofstateabc123$Rank <- c(1:nrow(dataofstate))
    
    if(num == "worst") {NUM <- nrow(dataofstate)}
    else{NUM = num}
    
    if(num == "best") {NUM <- 1}
    
    if(NUM > nrow(dataofstate)){hos = NA}
    else{
      dataofrank <- subset(dataofstateabc123,dataofstateabc123$Rank == NUM)
      hos = dataofrank[,2]
    }
    
    plus=c(hos,state)
    frame=rbind(frame,plus)
  }
  frame[-1,]
}