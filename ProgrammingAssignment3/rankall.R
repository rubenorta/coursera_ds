get_outcome <- function(outcome) {
  if (outcome == 'heart attack') col_pos <- 11
  else if (outcome == 'heart failure') col_pos <- 17
  else if (outcome == 'pneumonia') col_pos <- 23
  else stop('invalid outcome')
}

rankall <- function(outcome, num = "best") {
  col_pos <- get_outcome(outcome)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  bestdata <- data[order(as.numeric(data[[col_pos]]), data[["Hospital.Name"]]),]
  
  all_states <- c('AK','AL','AR','AZ','CA','CO','CT','DE','FL','GA','HI','ID','IA','IL','IN','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
  
  states <- c()
  hospital_names <- c()
  
  for (state in all_states[order(all_states)]) {
    bestdata <- bestdata[bestdata$State == state,]
    
    if (num == 'best') {
      name <- head(bestdata$Hospital.Name,1)
    } else if (num == 'worst') {
      bestdata <- bestdata[complete.cases(bestdata),]
      name <- tail(bestdata$Hospital.Name,1)
    } else if (num > nrow(bestdata)) {
      name <- 'NA'
    } else {
      my_list <- head(bestdata$Hospital.Name,num)
      name <- tail(my_list,1)
    }

    states <- c(states,state)
    hospital_names <- c(hospital_names, name)
  }
  
  data.frame(hospital = hospital_names,state = states) 
}

rankall("heart attack", 20)