best <- function(state, outcome) {
  Outcome.name <- c("heart attack", "heart failure", "pneumonia")
  Outcome.col  <- c(11, 17, 23)
  foo <- data.frame(Outcome.name, Outcome.col)
  
  ## Check if outcome is valid
  if(!(outcome %in% foo[,1])) stop("invalid outcome")
  foo <- subset(foo, Outcome.name == outcome)
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check if state is valid
  if(!(state %in% data[, 7])) stop("invalid state")
  state.data <- subset(data, data[, 7] == state)

  ## Remove hospitals without data
  state.data[, foo[,2]] <- as.numeric(state.data[, foo[,2]])
  state.data <- state.data[complete.cases(state.data[, foo[,2]]),]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  state.data <- state.data[ order(state.data[,foo[,2]], state.data[,2]), ]
  
  state.data[1,2]
}