rankhospital <- function(state, outcome = c("pneumonia","heart attack","heart failure"), num="best") {
  
  usage <- "Enter the two-letter abbreviation of a state (with no period) and an outcome of either \'pneumonia\', \'heart attack\' or \'heart failure\'."

  ## Read outcome data
#  setwd("U:/School/Coursera/R Programming/RPW4/rprog-data-ProgAssignment3-data")
  setwd("/Users/pesto/Documents/school/coursera/rprogramming")
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
  ## Return hospital name in that state with the given rank
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
#      print(class(hfsub1$rank))
      hfsub1[3]<-lapply(hfsub1[3],as.numeric)
      print(class(hfsub1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      hfsub1$comb<-paste(hfsub1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,hfsub1$Hospital.Name)
      hfsub1$rank<-rank(hfsub1$comb)
      order.hfsub1<-order(hfsub1[5])
      ordered.hfsub1<-hfsub1[order.hfsub1,]
#      lowesthf<-ordered.hfsub1[1,3]
#      allLowesthf<-subset(ordered.hfsub1,ordered.hfsub1[3]==lowesthf,select=c(Hospital.Name))
     print(head(ordered.hfsub1))      
      hfresultdf<-subset(hfsub1,hfsub1$rank==num)
      return(hfresultdf)
      
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
  ## 30-day death rate
  
}

