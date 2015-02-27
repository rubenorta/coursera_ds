get_file_name <- function(directory, name) {
  zero <- ''
  if (name < 100 ) {
    if (name < 10) {
      zero <- '00'    
    } else {
      zero <- '0'
    }
  }
  file <- paste(c(directory,'/',zero, name,'.csv'), collapse = '') 
  return(file)
}

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
  pollutant_data <- c()
  for (name in id) {
    
    file_name <- get_file_name(directory, name)
    raw_data <- read.csv(file_name)
    pollutant_data <- c(pollutant_data,raw_data[[pollutant]]) 
  }
  
  bad <- is.na(pollutant_data)
  clean_data <- pollutant_data[!bad]
  
  result <- mean(clean_data)
  return(result)
}

run <- function(ex = 1:3) {
 
  if (1 %in% ex) {
    good <- 4.064
    out <- pollutantmean("specdata", "sulfate", 1:10)
  }

  if (2 %in% ex) {
    good <- 1.706
    out <- pollutantmean("specdata", "nitrate", 70:72)
  }
  
  if (3 %in% ex) {
    good <- 1.281
    out <- pollutantmean("specdata", "nitrate", 23)
  }
}



