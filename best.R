best <- function(state, outcome) {
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors = FALSE)
  ha <- c("heart attack")
  hf <- c("heart failure")
  pn <- c("pneumonia")

  heart_attack_all <- if(outcome == ha) {
    ha_a <- as.data.frame(cbind(dat[, 2], ## hospital
                                dat[, 7], ## state
                                dat[, 11])) ## heart attack
    colnames(ha_a) <- c("hospital name", "state", "outcome")
    ha_a[, 3] <- as.numeric(ha_a[, 3])
    return(ha_a) 
  }
  heart_failure_all <- if(outcome == hf) {
    hf_a <- as.data.frame(cbind(dat[, 2], ## hospital
                                dat[, 7], ## state
                                dat[, 17])) ## heart failure
    colnames(hf_a) <- c("hospital name", "state", "outcome")
    hf_a[, 3] <- as.numeric(hf_a[, 3])
    return(hf_a) 
  }
  pneumonia_all <- if(outcome == pn) {
    pn_a <- as.data.frame(cbind(dat[, 2], ## hospital
                                dat[, 7], ## state
                                dat[, 23])) ## pneumonia
    colnames(pn_a) <- c("hospital name", "state", "outcome")
    pn_a[, 3] <- as.numeric(pn_a[, 3])
    return(pn_a) 
  }
  
  ## Check that state and outcome are valid
  if(!state %in% data_raw[, "state"]) {
    stop("invalid state")
  }
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
     stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death rate
  
  if(outcome == ha) {
    ha_state <- which(ha_a[, "state"] == "MD")
    ha_state_sear <- ha_a[ha_state, ]
    colnames(ha_state_sear) <- c("hospital name", "state", "outcome")
    min_ha <- as.data.frame(ha_state_sear[order(ha_state_sear$outcome), ], stringsAsFactors = FALSE)
    
  }
  
} 

