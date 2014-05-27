rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
#  setwd("U:/School/Coursera/R Programming/RPW4/rprog-data-ProgAssignment3-data/")
  setwd("/Users/pesto/Documents/school/coursera/rprogramming")
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  ## State
  stateList<-tolower(unique(outcomeFile$State))
  stateList<-stateList[!duplicated(stateList)]
  print(stateList)
  if(any(stateList==tolower(state))) {}
  else{stop("invalid state")}
  ## Outcome
  outcomeList<-c("pneumonia","heart attack","heart failure")
  if(any(outcomeList==tolower(outcome))) {}
  else{stop("invalid outcome")}
  ## Return hospital name in that state with the given rank
  if(tolower(outcome)=="heart attack") {
    hasub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    hasub1<-subset(hasub,hasub[3]!="Not Available")
    hasub1[3]<-lapply(hasub1[3],as.numeric)
    order.hasub1<-order(hasub1[3],hasub1[1])
    ordered.hasub1<-hasub1[order.hasub1,]
    ranks<-data.frame(rank=1:nrow(ordered.hasub1))
    ordered.hasub1<-cbind(ordered.hasub1,ranks)
    if(num=="best") {getRank<-1}
    else if(num=="worst") {getRank<-nrow(ordered.hasub1)}
    else {getRank<-num}
    haresultdf<-subset(ordered.hasub1,ordered.hasub1$rank==getRank)
    return(haresultdf[1,1])
    
  }
    if(tolower(outcome)=="heart failure") {
      hfsub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      hfsub1<-subset(hfsub,hfsub[3] != "Not Available")
      hfsub1[3]<-lapply(hfsub1[3],as.numeric)
      order.hfsub1<-order(hfsub1[3],hfsub1[1])
      ordered.hfsub1<-hfsub1[order.hfsub1,]
      ranks<-data.frame(rank=1:nrow(ordered.hfsub1))
      ordered.hfsub1<-cbind(ordered.hfsub1,ranks)
      if(num=="best") {getRank<-1}
      else if(num=="worst") {getRank<-nrow(ordered.hfsub1)}
      else {getRank<-num}
      hfresultdf<-subset(ordered.hfsub1,ordered.hfsub1$rank==getRank)
      return(hfresultdf[1,1])
      
    }
  if(tolower(outcome)=="pneumonia") {
    psub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    psub1<-subset(psub,psub[3] != "Not Available")
    psub1[3]<-lapply(psub1[3],as.numeric)
    order.psub1<-order(psub1[3],psub1[1])
    ordered.psub1<-psub1[order.psub1,]
    ranks<-data.frame(rank=1:nrow(ordered.psub1))
    ordered.psub1<-cbind(ordered.psub1,ranks)
    if(num=="best") {getRank<-1}
    else if(num=="worst") {getRank<-nrow(ordered.psub1)}
    else {getRank<-num}
    presultdf<-subset(ordered.psub1,ordered.psub1$rank==getRank)
    return(presultdf[1,1])
  }
  ## 30-day death rate
  
}

