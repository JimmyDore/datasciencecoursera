setwd("/home/jimmydore/Documents/Coursera/Datas/r-programming-lesson-week2-programming-assignment")

load_files <- function(directory,id){
  files <- vector(mode="list",length=length(id))
  j<-1
  for (i in id){
    pre_name <- ''
    if(i<10){
      pre_name<-'00'
    }else if(i<100){
      pre_name<-'0'
    }
    namefile <- paste(pre_name,i,sep='')
    files[[j]] <- paste('./',directory,'/',namefile,".csv",sep = '')
    j = j+1
  }
  my_list <- lapply(files,read.csv)
  df <- do.call(rbind.data.frame,my_list)
}

#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 
#'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate 
#'#matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant
#'# across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows


pollutantmean <- function (directory, pollutant, id = 1:332){
  data_frame <- load_files(directory,id)
  mean(data_frame[,pollutant],na.rm = TRUE)
}

#Write a function that reads a directory full of files and reports the number of completely
#observed cases in each data file. The function should return a data frame where the first 
#column is the name of the file and the second column is the number of complete cases. A prototype of this function follows
complete <- function(directory, id = 1:332){
  df <- load_files(directory,id)
  sub <- subset(df,(!is.na(df[,'nitrate'])) & (!is.na(df[,'sulfate'])))

  vec_nobs <- c()
  
  for(i in id){
    vec_nobs <-c(vec_nobs,nrow(subset(sub,sub['ID']==i)))
  }
  df2 <- data.frame(id,vec_nobs)
  names(df2)<-c("id","nobs")
  df2
}

#Write a function that takes a directory of data files and a threshold for complete cases 
#and calculates the correlation between sulfate and nitrate for monitor locations where the 
##number of completely observed cases (on all variables) is greater than the threshold. The 
#function should return a vector of correlations for the monitors that meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector of 
#length 0. A prototype of this function follows
corr <- function(directory, threshold = 0){
  # df_complete_obs <- complete("specdata")
  # selected_id = subset(df_complete_obs,nobs > threshold)
  # if (length(selected_id) > 0){
  #   df <- load_files(directory,selected_id$id)
  #   #sub <- subset(df,(!is.na(df[,'nitrate'])) & (!is.na(df[,'sulfate'])))
  #   result <- cor(df[,'nitrate'],df[,'sulfate'],use='complete.obs')
  #   result
  # }else{
  #   c(0)
  # }
  tcorr <- function(fname) {
    data <- read.csv(file.path(directory, fname))
    nobs <- sum(complete.cases(data))
    if (nobs > threshold) {
      return (cor(data$nitrate, data$sulfate, use="complete.obs"))
    }
  }
  tcorrs <- sapply(list.files(directory), tcorr) #get all correlations + NULLs
  tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) #remove NULLs
  return (tcorrs)
}




