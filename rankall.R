rankall <- function(outcome, num = "best") {
  Outcome.name <- c("heart attack", "heart failure", "pneumonia")
  Outcome.col  <- c(11, 17, 23)
  foo <- data.frame(Outcome.name, Outcome.col)
  
  ## Check if outcome is valid
  if(!(outcome %in% foo[,1])) stop("invalid outcome")
  foo <- subset(foo, Outcome.name == outcome)
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, foo[,2]] <- as.numeric(data[, foo[,2]])
  data <- data[complete.cases(data[, foo[,2]]),]
  
  ## List all states
  states <- unique(data[,7])
  
  ## Create the data frame to save the results
  result <- data.frame()
  
  for(state in states) {
    
    # Filter data by state
    state.data <- subset(data, data[, 7] == state)
    
    # Order results by outcome and hospital name ASC
    state.data <- state.data[ order(state.data[,foo[,2]], state.data[,2]), ]
    
    if(num == "best") {
      rank <- 1
    } else if(num == "worst") {
      rank <- nrow(state.data)
    } else {
      rank <- num 
    }
    
    new.result <- data.frame(hospital = state.data[rank,2], state = state)
    result <- rbind(result, new.result)
    
  }
  
  result$hospital <- as.character(result$hospital)
  result$state <- as.character(result$state)
  result <- result[order(result$state),]
}