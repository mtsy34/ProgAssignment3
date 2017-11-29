rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% levels(factor(data$State)) )) {
    stop("invalid state")
  }
  
  if (!(outcome == "heart failure" || outcome == "heart attack" || outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  data <- data[(data$State == state),]
  
  # Remove columns by outcome
  if (outcome == "heart attack") {
    data <- data[, c(2, 11)]
  }
  if (outcome == "heart failure") {
    data <- data[, c(2, 17)]
  }
  if (outcome == "pneumonia") {
    data <- data[, c(2, 23)]
  }

  names(data)[2] <- "Deaths"
  data[,2] <- suppressWarnings(as.numeric(data[,2]))
  
  # Remove NA values 
  data <- data[!is.na(data$Deaths),]
  
  # If num > nrows then return NA
  if(class(num) == "numeric" & num > nrow(data)) {
    return("NA")
  }
  
  # Order by deaths and then hospital name
  data <- data[order(data$Deaths, data$Hospital.Name),]
  
  # Result
  if(class(num) == "character") {
    if (num == "best") {
      return(data$Hospital.Name[1])
    }
    else if (num == "worst") {
      return(data$Hospital.Name[length(data$Hospital.Name)])
    }
  }
  else {
    return(data$Hospital.Name[num])
  }
  }