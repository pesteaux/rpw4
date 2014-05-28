rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  #setwd("U:/School/Coursera/R Programming/RPW4/rprog-data-ProgAssignment3-data/")
    setwd("/Users/pesto/Documents/school/coursera/rprogramming")
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## State
  stateList<-tolower(unique(outcomeFile$State))
  stateList<-stateList[!duplicated(stateList)]

  if(outcome=="heart failure") {outcomeCol<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
  else if(outcome=="heart attack") {outcomeCol<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  else {outcomeCol<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" }
  results<-data.frame(hospital=1:54,state=stateList,stringsAsFactors=FALSE)
  #names(results,c("Name","State")
  #names(results)
  for (i in 1:54){ 
  newcol<-getAllRank(outcomeCol,stateList[i],num,outcomeFile)
  results<-rbind(results,newcol)

  }
  print(results)
}

getAllRank<-function(goutcome,gstate,gnum,dataframe=outcomeFile) {
#  print(gnum)
#  print(dataframe[1,1])
  garSub<-subset(dataframe,tolower(dataframe$State)==gstate,select=c("Hospital.Name","State",goutcome))
  garSub1<-subset(garSub,garSub[3]!="Not Available")
#  print(garSub1)
  garSub1[3]<-lapply(garSub1[3],as.numeric)
  order.garSub1<-order(garSub1[3],garSub1[1])
  ordered.garSub1<-garSub1[order.garSub1,]
  ranks<-data.frame(rank=1:nrow(ordered.garSub1))
  ordered.garSub1<-cbind(ordered.garSub1,ranks)
 # print(ordered.garSub1)
  if(gnum=="best") {getRank<-1}
  else if(gnum=="worst") {getRank<-nrow(ordered.garSub1)}
  else {getRank<-gnum}
  garresultdf<-subset(ordered.garSub1,ordered.garSub1$rank==getRank,select=c("Hospital.Name","State"))
  names(garresultdf)<-c("hospital","state")
#  garresult<-c(garresultdf$Hospital.Name,garresultdf$State)
#  print(garresult)
return(garresultdf)
}