# install.packages("tidyverse")
# install.packages("dplyr")

library(stringr)
library(dplyr)

# read in the outcome data
outcome_file <- "/Users/jtang/Projects/R/self-learning/Cousera - Data Science specialization from Johns Hopkins/jhu_r_programming/data/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv"
## Read outcome data
outcome <- read.csv(outcome_file)

# finding the best hospital in a state
rankHospital <- function(state, disease, rank) {
  
  theRankHospital <- NULL
  rate <- NULL
  
  rankByHeartAttack <- function(s, x, r) {
    sorted <- inHeartAttack(s, x)
    r <- adjustRank(r, nrow(sorted))
    sorted[r,]
  }
  
  inHeartAttack <- function(s, x) {
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available', c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    
    # sort by hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    # coerce to rate to numeric
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    # sort by rate
    arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  }
  
  rankByHeartFailure <- function(s, x, r) {
    sorted <- inHeartFailure(s, x)
    r <- adjustRank(r, nrow(sorted))
    sorted[r,]
  }
  
  inHeartFailure <- function(s, x) {
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available', c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    
    # sort by hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    # coerce to rate to numeric
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    # sort by rate
    arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  }
  
  rankByPneumonia <- function(s, x, r) {
    sorted <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    r <- adjustRank(r, nrow(sorted))
    sorted[r,]
  }
  
  inPneumonia <- function(s, x) {
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available', c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    
    # sort by hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    # coerce to rate to numeric
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    # sort by rate
    arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  }
  
  adjustRank <- function(r, max) {
    if (r == "best") {
      r = 1
    } else if (r == "worst") {
      r = max
    } else {
      r
    }
  }
  
  if (disease == "heart attack") {
    if (sum(str_detect(outcome$State, state)) > 0) {
      theRank <- rankByHeartAttack(state, outcome, rank)
      theRankHospital <- theRank[1,c("Hospital.Name")]
      rate <- theRank[1,2]
    } else {
      print (paste("Error in best(", state, ", ", disease, ") : invalid state", sep=""))
    }
  } else if (disease == "heart failure") {
    if (sum(str_detect(outcome$State, state)) > 0) {
      theRank <- rankByHeartFailure(state, outcome, rank)
      theRankHospital <- theRank[1,c("Hospital.Name")]
      rate <- theRank[1,2]
    } else {
      print (paste("Error in best(", state, ", ", disease, ") : invalid state", sep=""))
    } 
  } else if (disease == "pneumonia") {
    if (sum(str_detect(outcome$State, state)) > 0) {
      theRank <- rankByPneumonia(state, outcome, rank)
      theRankHospital <- theRank[1,c("Hospital.Name")]
      rate <- theRank[1,2]
    } else {
      print (paste("Error in best(", state, ", ", disease, ") : invalid state", sep=""))
    }
  } else {
    print(paste("Error in best(", state, ", ", outcome,") : invalid outcome", sep=""))
  }
  
  theRankHospital
}

rankAll <- function(disease, num = "best"){
  states <- unique(select(outcome, State))
  states <- arrange(states,State)
  ctr <- 1
  end <- nrow(states)
  dataset <- data.frame("hospital"=character(0),"state"=character(0))
  
  while (ctr <= end){
    rh <- rankHospital(states[ctr,], disease, num)
    
    if (length(rh[1]) == 0){
      rh <- "NA"
    }
    newDataSet <- data.frame("hospital" = rh[1], "state" = states[ctr,])
    dataset <- rbind(dataset, newDataSet)
    ctr <- ctr + 1
  }
  
  dataset
  
}
