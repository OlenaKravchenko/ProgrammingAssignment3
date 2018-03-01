rankall <- function(outcome, num = "best") {
  ## Read the data from the working directory
  dat <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors = FALSE)
  ## Check validity of state and outcome, chooses the outcome value depending of an input
  states <- unique(dat[, 7])
  switch(outcome, 'pneumonia' = {
    out = 23
  }, 'heart attack' = {
    out = 11
  }, 'heart failure' = {
    out = 17
  }, return("invalid outcome"))
  
  ## Return the data frame that contains only needed data: hospital name, state and outcome
  dat[, out] <- as.numeric(dat[, out])
  dat <- dat[, c(2, 7, out)]
  dat <- na.omit(dat)
  colnames(dat) <- c("hospital", "state", "outcome")
  ## Return the hospital name by the given rank in a certain state
  state_rank <- function(state) {
    ## Define the dataframe for aeach state, we will use it further
    state_data <- dat[dat[, 2] == state, ]
    nhosp <- nrow(state_data)
    ## Choose the num value daponding on the input
    switch(num, "best" = {
      num <- 1
    }, "worst" = {
      num <- nhosp
    })
    if(num > nhosp) {
      result <- NA
    } 
    ## Order data by outcome and hospital name
    ordered_data <- order(state_data[, 3], state_data[, 1])
    result <- state_data[ordered_data, ][num, 1]
    c(result, state)
  }
  ## Return the final result of a function
  output <- do.call(rbind, lapply(states, state_rank))
  output <- output[order(output[, 2]), ]
  rownames(output) <- output[,2]
  colnames(output) <- c("hospital name", "state")
  data.frame(output)
}
