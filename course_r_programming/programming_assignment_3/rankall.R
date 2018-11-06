## 4 - Ranking hospitals in all states
## ------------------------------------------
## Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking 
## (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. The function should return a value
## for every state (some may be NA). The first column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  setwd("/home/jimmydore/Documents/Coursera/Repo_Github/datasciencecoursera/course_r_programming/programming_assignment_3")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state (not state actually) and outcome are valid
  
  possible_outcomes = c("heart attack","heart failure","pneumonia")
  possible_columns = c("Heart.Attack","Heart.Failure","Pneumonia")
  if (!(is.element(outcome,possible_outcomes))){
    stop("invalid outcome")
  }
    
  
  #Remove all rows where is no value in the column
  index_column = possible_columns[match(outcome,possible_outcomes)]
  column = paste("Hospital.30.Day.Death..Mortality..Rates.from.",index_column,sep='')
  
  data <- data[,c(column,"Hospital.Name","State")]
  
  data[, column] <- as.numeric(data[, column])
  
  #according to the examples, we want to keep NA
  data <- na.omit(data)
  
  data <- data[order(data[,column],data[,"Hospital.Name"]),]

  f_test <- function (state){
    data_temp <- subset(data,State == state)
    if (num == "best"){
      return(c(data_temp[1,"Hospital.Name"],state))
    }else if(num == "worst"){
      return(c(data_temp[nrow(data_temp),"Hospital.Name"],state))
    }else{
      return(c(data_temp[num, "Hospital.Name"],state))
    }
  }
  result <- sapply(sort(unique(data$State)),f_test)
  result <- (data.frame(result[1,],result[2,]))
  colnames(result) <- c("hospital", "state")
  
  return(result)
}

print(head(rankall("heart attack", 20), 10))