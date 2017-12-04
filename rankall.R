rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  
  if (!(outcome == "heart failure" || outcome == "heart attack" || outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  # Removing columns by outcome
  if (outcome == "heart attack") {
    data <- data[, c(2, 7, 11)]
  }
  if (outcome == "heart failure") {
    data <- data[, c(2, 7, 17)]
  }
  if (outcome == "pneumonia") {
    data <- data[, c(2, 7, 23)]
  }
  
  names(data)[3] <- "Deaths"
  data$Deaths <- suppressWarnings(as.numeric(data$Deaths))
  
  # Remove NA values 
  data <- data[!is.na(data$Deaths),]
  
  # Order by deaths and then hospital name
  data <- data[order(data$Deaths, data$Hospital.Name),]
  
  ## Find hospital of given rank
  
  # Creating a list of all the states
  Statelist <- as.character(unique(data$State))
  Statelist <- Statelist[order(Statelist)]
  
  Final <- data.frame()
  
  for (i in 1:length(Statelist)) {
    StateData <- subset(data, State == Statelist[i])
    # Specifying exact input value for num input
    if (num == "worst") {
      newnum <- nrow(StateData)
      } 
      else if (num == "best") {
        newnum <- 1
      }
      else {
        newnum <- num
      }
    # Creating each row for final DF
    tmp <- data.frame(StateData[newnum, "Hospital.Name"], Statelist[i])
    colnames(tmp) <- c("hospital", "state")
    Final <- rbind(Final, tmp) # Adding each row
  }
  Final
}