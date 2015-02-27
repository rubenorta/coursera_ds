inget_file_name <- function(directory, name) {
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

complete <- function(directory, id = 1:332) { 
  camera <- c()
  nobs <- c()
  for (name in id) {
    file_name <- get_file_name(directory, name)
    raw_data <- read.csv(file_name)
    good <- complete.cases(raw_data)
    camera <- c(camera, name)
    nobs <- c(nobs, sum(good))
  }
  
  result <- data.frame(id = camera,nobs = nobs) 
  return(result)
}