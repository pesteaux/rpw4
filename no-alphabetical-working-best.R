best <- function(state, outcome = c("pneumonia","heart attack","heart failure")) {
  
  usage <- "Enter the two-letter abbreviation of a state (with no period) and an outcome of either \'pneumonia\', \'heart attack\' or \'heart failure\'."

  ## Read outcome data
  setwd("U:/School/Coursera/R Programming/RPW4/rprog-data-ProgAssignment3-data")
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  ## State
  stateList<-tolower(unique(outcomeFile$State))
  if(any(stateList==tolower(state))) {}
  else{return("invalid state")}
  ## Outcome
  outcomeList<-c("pneumonia","heart attack","heart failure")
  if(any(outcomeList==tolower(outcome))) {}
  else{return("inavlid outcome")}
  ## Return hospital name in that state with lowest 30-day death
    if(tolower(outcome)=="heart attack") {
    hasub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    hasub1<-subset(hasub,hasub[3]!="Not Available")
    order.hasub1<-order(hasub1[3])
    ordered.hasub1<-hasub1[order.hasub1,]
    print(ordered.hasub1[1,1])
  }
    if(tolower(outcome)=="heart failure") {
      hfsub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      hfsub1<-subset(hfsub,hfsub[3] != "Not Available")
      order.hfsub1<-order(hfsub1[3])
      ordered.hfsub1<-hfsub1[order.hfsub1,]
      print(ordered.hfsub1[1,1])
      
    }
  if(tolower(outcome)=="pneumonia") {
    psub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    psub1<-subset(psub,psub[3] != "Not Available")
    order.psub1<-order(psub[3])
    ordered.psub1<-psub1[order.psub1,]
    print(ordered.psub1[1,1])
  }
  ## Rate 

}

