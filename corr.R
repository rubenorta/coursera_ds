corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  files <- list.files(directory)
  correlations <- c()
  for (file in files) {
    file_name <- paste(c(directory,'/',file), collapse = '')
    raw_data <- read.csv(file_name)
    good <- complete.cases(raw_data)
    complete <- sum(good)
    clean <- raw_data[good,]
    if (complete > threshold) {
          correlations <- c(correlations, cor(clean['nitrate'], clean['sulfate']))
    }
  }
  return(correlations)
}
