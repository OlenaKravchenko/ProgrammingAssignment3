best <- function(state, outcome) {
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors = FALSE)
  ha <- c("heart attack")
  hf <- c("heart failure")
  pn <- c("pneumonia")

  heart_attack_all <- if(outcome == ha) {
    heart_attack_all <- dat[c(2, 7, 11)]
    colnames(heart_attack_all) <- c("hospital name", "state", "outcome")
    heart_attack_all[, 3] <- as.numeric(heart_attack_all[, 3])
    heart_attack_all
  }
  heart_failure_all <- if(outcome == hf) {
    heart_failure_all <- dat[c(2, 7, 17)]
    colnames(heart_failure_all) <- c("hospital name", "state", "outcome")
    heart_failure_all[, 3] <- as.numeric(heart_failure_all[, 3])
    heart_failure_all
  }
  pneumonia_all <- if(outcome == pn) {
    pneumonia_all <- dat[c(2, 7, 23)]
    colnames(pneumonia_all) <- c("hospital name", "state", "outcome")
    pneumonia_all[, "outcome"] <- as.numeric(pneumonia_all[, "outcome"])
    pneumonia_all
  }
  
  ## Check that state and outcome are valid
  if(!state %in% dat[, "State"]) {
    stop("invalid state")
  }
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death rate
  
  if(outcome == pn) {
    pn_state <- which(pneumonia_all[, "state"] == state)
    pn_state_sear <- pneumonia_all[pn_state, ]
    colnames(pn_state_sear) <- c("hospital name", "state", "outcome")
    min_pn <- pn_state_sear[order(pn_state_sear$outcome), ]
    min_pn[1,1]
  }
  else 
  if(outcome == hf) {
    hf_state <- which(heart_failure_all[, "state"] == state)
    hf_state_sear <- heart_failure_all[hf_state, ]
    colnames(hf_state_sear) <- c("hospital name", "state", "outcome")
    min_hf <- hf_state_sear[order(hf_state_sear$outcome), ]
    min_hf[1,1]
  }
  else
  if(outcome == ha) {
    ha_state <- which(heart_attack_all[, "state"] == state)
    ha_state_sear <- heart_attack_all[ha_state, ]
    colnames(ha_state_sear) <- c("hospital name", "state", "outcome")
    min_ha <- ha_state_sear[order(ha_state_sear$outcome), ]
    min_ha[1,1]
  }
  
} 


