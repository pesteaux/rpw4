best <- function(state, outcome = c("pneumonia","heart attack","heart failure")) {
  
  usage <- "Enter the two-letter abbreviation of a state (with no period) and an outcome of either \'pneumonia\', \'heart attack\' or \'heart failure\'."

  ## Read outcome data
  setwd("U:/School/Coursera/R Programming/RPW4/rprog-data-ProgAssignment3-data")
  outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  ## State
  stateList<-tolower(unique(outcomeFile$State))
  if(any(stateList==tolower(state))) {}
  else{stop("invalid state")}
  ## Outcome
  outcomeList<-c("pneumonia","heart attack","heart failure")
  if(any(outcomeList==tolower(outcome))) {}
  else{stop("inavlid outcome")}
  ## Return hospital name in that state with lowest 30-day death
    if(tolower(outcome)=="heart attack") {
    hasub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    hasub1<-subset(hasub,hasub[3]!="Not Available")
    hasub1[3]<-lapply(hasub1[3],as.numeric)
    order.hasub1<-order(hasub1[3])
    ordered.hasub1<-hasub1[order.hasub1,]
    lowestha<-ordered.hasub1[1,3]
    allLowest<-subset(ordered.hasub1,ordered.hasub1[3]==lowestha,select=c(Hospital.Name))
    order.allLowest<-allLowest[order(allLowest$Hospital.Name),]
    return(order.allLowest)
    
  }
    if(tolower(outcome)=="heart failure") {
      hfsub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      hfsub1<-subset(hfsub,hfsub[3] != "Not Available")
      hfsub1[3]<-lapply(hfsub1[3],as.numeric)
      order.hfsub1<-order(hfsub1[3])
      ordered.hfsub1<-hfsub1[order.hfsub1,]
      lowesthf<-ordered.hfsub1[1,3]
      allLowesthf<-subset(ordered.hfsub1,ordered.hfsub1[3]==lowesthf,select=c(Hospital.Name))
      order.allLowesthf<-allLowesthf[order(allLowesthf$Hospital.Name),]
      return(order.allLowesthf)
      
    }
  if(tolower(outcome)=="pneumonia") {
    psub<-subset(outcomeFile,outcomeFile$State==state,select=c(Hospital.Name,State,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    psub1<-subset(psub,psub[3] != "Not Available")
    psub1[3]<-lapply(psub1[3],as.numeric)
    order.psub1<-order(psub1[3])
    ordered.psub1<-psub1[order.psub1,]
    lowestp<-ordered.psub1[1,3]
    allLowestp<-subset(ordered.psub1,ordered.psub1[3]==lowestp,select=c(Hospital.Name))
    order.allLowestp<-allLowestp[order(allLowestp$Hospital.Name),]
    return(order.allLowestp)
  }
  ## Rate 

}

