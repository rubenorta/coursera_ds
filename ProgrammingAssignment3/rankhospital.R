get_outcome <- function(outcome) {
  if (outcome == 'heart attack') col_pos <- 11
  else if (outcome == 'heart failure') col_pos <- 17
  else if (outcome == 'pneumonia') col_pos <- 23
  else stop('invalid outcome')
}

check_state <- function(state) {
  all_states <- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
  if (state %in% all_states) TRUE
  else stop('invalid state')
}

rankhospital <- function(state, outcome, num = "best") {
  check_state(state)
  col_pos <- get_outcome(outcome)
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  best_state <- data[data$State == state,]
  bestdata <- best_state[order(as.numeric(best_state[[col_pos]]), best_state[["Hospital.Name"]]),]
  
  if (num == 'best') {
    head(bestdata$Hospital.Name,1)
  } else if (num == 'worst') {
    bestdata <- bestdata[complete.cases(bestdata),]
    tail(bestdata$Hospital.Name,1)
  } else if (num > nrow(bestdata)) {
    'NA'
  } else {
    my_list <- head(bestdata$Hospital.Name,num)
    tail(my_list,1)
  }
  
}