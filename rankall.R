rankall <- function(outcome, num) {
  ## read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors = FALSE)
  ha <- c("heart attack")
  hf <- c("heart failure")
  pn <- c("pneumonia")
  ## check whether the state and outcome are valid
  
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
  

  ## For each state, find the hospital of the given rank
  rankHospitals <- function(x, num) {
    if (num=="best") {
      head(x, 1)
    } else if (num=="worst") {
      tail(x, 1)
    } else {
      x[num]
    }
  }
  if(outcome == ha) {
    ha_ordered <- heart_attack_all[order(heart_attack_all$outcome, heart_attack_all$'hospital name'), ]
    good_ha_ordered <- ha_ordered[complete.cases(ha_ordered), ]
    ha_by_state <- with(good_ha_ordered, split(good_ha_ordered, good_ha_ordered[, 2]))
    ha_by_state
    if(num == "best") {
      num_best <- 1
      num <- num_best
      best_ha <- for (i in seq_along(ha_by_state)) {
        rbind(ha_by_state[1, 1:2])
      }
      best_ha
    }
    if(num == "worst" & outcome == pn) {
      num_worst_pn <- as.numeric(nrow(good_min_pn))
      num <- num_worst_pn
      num
    }
    if(outcome == pn & num > nrow(good_min_pn)) {
      return("NA")
    }
  }
  
}
