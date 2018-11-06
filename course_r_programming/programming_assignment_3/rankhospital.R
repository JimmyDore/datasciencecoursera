## 3 - Ranking hospitals by outcome in a state
## ----------------------------------------------
## Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument. For example, the call
## rankhospital("MD", "heart failure", 5)
## would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.

rankhospital <- function(state, outcome, num = "best") {
  
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  #Remove all rows where is no value in the column
  index_column = possible_columns[match(outcome,possible_outcomes)]
  column = paste("Hospital.30.Day.Death..Mortality..Rates.from.",index_column,sep='')
  
  data <- data[,c(column,"Hospital.Name")]
  
  data[, column] <- as.numeric(data[, column])
  
  #according to the examples, we want to keep NA
  data <- na.omit(data)
  
  #order function enables me to sort the dataset, first according to the column, 
  #then according to the name of hospital, play with param decreasing to sort lower or upper
  data <- data[order(data[,column],data[,"Hospital.Name"]),]
  
  if (num == "best"){
    print(data[1,"Hospital.Name"])
  }else if(num == "worst"){
    print(data[nrow(data),"Hospital.Name"])
  }else{
    print(data[num,"Hospital.Name"])
  }
}

rankhospital("TX", "heart failure", 4) # [1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst") # [1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000) # [1] NA