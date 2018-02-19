# ProgrammingAssignment3

best <- function(state, outcome) {
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors = FALSE)
  data_raw <- as.data.frame(cbind(dat[, 2], ## hospital
                               dat[, 7], ## state
                               dat[, 11], ## heart attack
                               dat[, 17], ## heart failure
                               dat[, 23])) ## pneumonia
  colnames(data_raw) <- c("hospital name", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% data_raw[, "state"]) {
    stop("invalid state")
  }
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
     stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death rate
  sta <- which(data_raw[, "state"] == state) 
  searched_state <- data_raw[sta, ]
  heart_attack <- searched_state[order(searched_state$`heart attack`), ]
  heart_failure <- searched_state[order(searched_state$`heart failure`), ]
  pneumonia <- searched_state[order(searched_state$pneumonia), ]
  
  if(outcome == "heart attack") {
    return(heart_attack[1,1])
  }
  if(outcome == "pneumonia") {
    return(pneumonia[1, 1])
  }
  if(outcome == "heart failure") {
    return(heart_failure[1,1])
  }
    
  else
    stop("lol")
} 

