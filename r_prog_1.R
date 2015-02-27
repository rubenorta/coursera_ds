pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  setwd(directory)
 
  pollutant_data <- c()
  for (name in id) {
    if (name < 100 ) {
      if (name < 10) {
        zero <- '00'    
      } else {
        zero <- '0'
      }
    }
    
    file_name <- paste(c(zero, name,'.csv'), collapse = '')
    raw_data <- read.csv(file_name)
    good <- complete.cases(raw_data)
    clean_data <- raw_data[good,]
    pollutant_data <- c(pollutant_data,clean_data[[pollutant]]) 
  }

  result <- mean(pollutant_data)
  
  return(result)
}