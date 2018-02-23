rankhospital <- function(state, outcome, num) {
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
  ## If the number given by num is larger than the number of hospitals in that
  ## state, then the function should return NA. 
  
  
  ## returns a character vector with the name  of the hospital 
  ## that has the ranking specified by the num argument.
  
  if(outcome == pn) {
    pn_state <- which(pneumonia_all[, "state"] == state)
    pn_state_sear <- pneumonia_all[pn_state, ]
    colnames(pn_state_sear) <- c("hospital name", "state", "outcome")
    min_pn <- pn_state_sear[order(pn_state_sear$outcome, pn_state_sear$`hospital name`), ]
    good_min_pn <- min_pn[complete.cases(min_pn), ]
    if(num == "best") {
      num_best <- 1
      num <- num_best
      num
    }
    if(num == "worst" & outcome == pn) {
      num_worst_pn <- as.numeric(nrow(good_min_pn))
      num <- num_worst_pn
      num
    }
    if(outcome == pn & num > nrow(good_min_pn)) {
      return("NA")
    }
    good_min_pn[num, 1]
  }
  else 
    if(outcome == hf) {
      hf_state <- which(heart_failure_all[, "state"] == state)
      hf_state_sear <- heart_failure_all[hf_state, ]
      colnames(hf_state_sear) <- c("hospital name", "state", "outcome")
      min_hf <- hf_state_sear[order(hf_state_sear$outcome, hf_state_sear$`hospital name`), ]
      good_min_hf <- min_hf[complete.cases(min_hf), ]
      if(num == "best") {
        num_best <- 1
        num <- num_best
        num
      }
      if(num == "worst" & outcome == hf) {
        num_worst_hf <- as.numeric(nrow(good_min_hf))
        num <- num_worst_hf
        num
      }
      if(outcome == hf & num > nrow(good_min_hf)) {
        return("NA")
      }
      good_min_hf[num, 1]
    }
  else
    if(outcome == ha) {
      ha_state <- which(heart_attack_all[, "state"] == state)
      ha_state_sear <- heart_attack_all[ha_state, ]
      colnames(ha_state_sear) <- c("hospital name", "state", "outcome")
      min_ha <- ha_state_sear[order(ha_state_sear$outcome, ha_state_sear$`hospital name`), ]
      good_min_ha <- min_ha[complete.cases(min_ha), ]
      if(num == "best") {
        num_best <- 1
        num <- num_best
        num
      }
      if(num == "worst" & outcome == ha) {
        num_worst_ha <- as.numeric(nrow(good_min_ha))
        num <- num_worst_ha
        num
      }
      if(outcome == ha & num > nrow(good_min_ha)) {
        return("NA")
      }
      good_min_ha[num,1]
    }
}
