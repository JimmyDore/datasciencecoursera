## 2 - Finding the best hospital in a state
## -----------------------------------------
## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.


best <- function(state, outcome) {

  ## Read outcome data
  setwd("/home/jimmydore/Documents/Coursera/Repo_Github/datasciencecoursera/course_r_programming/programming_assignment_3")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  data <- subset(data,State == state)
  if (nrow(data)==0){
    stop("invalid state")
  }
  
  possible_outcomes = c("heart attack","heart failure","pneumonia")
  possible_columns = c("Heart.Attack","Heart.Failure","Pneumonia")
  if (!(is.element(outcome,possible_outcomes))){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  #Remove all rows where is no value in the column
  index_column = possible_columns[match(outcome,possible_outcomes)]
  column = paste("Hospital.30.Day.Death..Mortality..Rates.from.",index_column,sep='')
  
  data <- data[,c(column,"Hospital.Name")]
  
  
  data[, column] <- as.numeric(data[, column])
  
  data <- na.omit(data)
  
  #na.omit function behaviour : default : remove rows when na on all columns, 
  # but param cols of na.omit enables you to 
  # specify on which column you want to verify the na, here, useless
  
  #get the rows with the minimum value
  data <- data[which.min(data[,column]),]
  
  #sort by alphabetical order if necessary
  if (nrow(data) > 1){
    data <- data[order(data[,"Hospital.Name"]),]
  }
  
  print(data[1,"Hospital.Name"])
}

best("TX", "heart attack") # [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure") # [1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack") # [1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia") # [1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack") # Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack") # Error in best("NY", "hert attack") : invalid outcome