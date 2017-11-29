best <- function(state, outcome) {
  ## Read data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid  
  # If state does not match any of those in the data, stop the model and throw message
  if(! ( state %in% levels(factor(data$State)) ) ) {
    stop("invalid state")
  }
  
  # If outcome does not match the three stated, stop the model and throw message 
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  # Remove row by state and columns state
  data = data[data$State==state,] # Keep rows that contains desired state
  
  # Remove columns by outcome, only left HospitalName and Deaths by outcome
  if(outcome == "heart attack") {
    data = data[,c(2,11)]
  } else if(outcome == "heart failure") {
    data = data[,c(2,17)]
  } else if(outcome == "pneumonia") {
    data = data[,c(2,23)]
  }
  names(data)[2] = "Outcome"
  data[, 2] = suppressWarnings(as.numeric(data[, 2]))
  
  # Remove rows with NA
  data = data[!is.na(data$Outcome),]
  
  # Order by Outcome, and then HospitalName
  data = data[order(data$Outcome, data$Hospital.Name),]
  
  # Return
  return (data$Hospital.Name[1])
}