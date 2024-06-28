#'Expected Value as a Function of the Number of Risky Choices in the Keep Condition
#'
#'This function calculates the expected payoff given the endowment, the remaining trials, the number of risky choices, and the
#'payoffs and associated probabilities for the Keep condition 
#'(i.e., condition, where participants can keep what they already earned but don't get any payoff for trials after when going extinct).
#' @param e The endowment
#' @param n The remaining number of trials 
#' @param NR The number of risky choices
#' @param r1 The probability of extinction when choosing the risky lottery (outcome 1)
#' @param r2 The probability of outcome 2 when choosing the risky lottery 
#' @param r3 The probability of outcome 3 when choosing the risky lottery
#' @param s1 the probability of outcome 1 when choosing the safe lottery
#' @param s2 the probability of outcome 2 when choosing the safe lottery
#' @param payoff_r2 The payoff of outcome 2 when choosing the risky lottery
#' @param payoff_r3 The payoff of outcome 3 when choosing the risky lottery
#' @param payoff_s1 The payoff of outcome 1 when choosing the safe lottery
#' @param payoff_s2 The payoff of outcome 2 when choosing the safe lottery
#' @param strategy This vector can specify the choices manually for using different orders than the optimal order
#' @returns A list with the expected payoff when following the optimal strategy, the optimal next choice, a
#' and a softmax transformation with inverse temperature 50 containing the probabilities of the next choice 
#' (this helps pick up on cases, where there are only tiny EV differences between playing safe and risky)
#' @export
#' @examples
#' #Optimal Strategy for Maier et al. (2024)
#' ev_keep(e = 0, n = 100, NR = 44, r1 = 0.05, r2 = 0.475, r3 = 0.475, s1 = 0.5,
#'  s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, 
#'  strategy = c(NA))
ev_keep <- function(e, n, NR, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1,  strategy = c(NA)){
  
  if(is.numeric(NR) & is.na(strategy[1])){
    E1 <- payoff_s1*s1 + payoff_s2*s2
    E2 <- (r2+r1/2)*payoff_r2 + (r3+r1/2)*payoff_r3
    ps <- (1-r1)
    EARN <- e
    
    if(NR > 0){
      ev <- (1-ps)*sum(ps^(1:(NR-1))*(1:(NR-1))*E2) + ps^NR*NR*E2 + E1*(n-NR) + EARN # Doesn't work for NR < 1
    } else {
      ev <- E1*(n) + EARN
    }
  } 
  
  if(!is.na(strategy[1])){
    NR <- sum(strategy == 2)
    E1 <- payoff_s1*s1 + payoff_s2*s2
    E2 <-  r2*payoff_r2 + r3*payoff_r3
    ps <- (1-r1)
    
    ev <- 0
    palive <- 1
    for(i in 1:n){
      if(strategy[i] == 1){
        ev <- ev + E1*palive
      }
      if(strategy[i] == 2){
        palive <- palive*ps
        ev <- ev + E2*palive
      }
    }
  } 
  
  return(ev)
}

#'Optimal Number of Risky Choices in the Keep Condition
#'
#'Loops over ev_keep to estimate the optimal number of risky choices
#' @param e The endowment
#' @param n The remaining number of trials 
#' @param r1 The probability of extinction when choosing the risky lottery (outcome 1)
#' @param r2 The probability of outcome 2 when choosing the risky lottery 
#' @param r3 The probability of outcome 3 when choosing the risky lottery
#' @param s1 the probability of outcome 1 when choosing the safe lottery
#' @param s2 the probability of outcome 2 when choosing the safe lottery
#' @param payoff_r2 The payoff of outcome 2 when choosing the risky lottery
#' @param payoff_r3 The payoff of outcome 3 when choosing the risky lottery
#' @param payoff_s1 The payoff of outcome 1 when choosing the safe lottery
#' @param payoff_s2 The payoff of outcome 2 when choosing the safe lottery
#' @returns A list with the expected payoff when following the optimal strategy, the optimal next choice, a
#' and a softmax transformation with inverse temperature 50 containing the probabilities of the next choice 
#' (this helps pick up on cases, where there are only tiny EV differences between playing safe and risky)
#' @export
#' @examples
#' #Optimal Strategy for Maier et al. (2024)
#' max_ev_keep(e = 0, n = 100, r1 = 0.05, r2 = 0.475, r3 = 0.475, s1 = 0.5,
#' s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1)
max_ev_keep <- function(n, e, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1){
  EV <- vector(mode = "numeric", length = n+1)  # Adjusting the length to N+1 to accommodate 0-indexing
  
  for(i in 0:n){
    EV[i+1] <- ev_keep(e, n, NR = i, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1,  strategy = c(NA))
  }
  
  res <- data.frame(NR = 0:n, EV)
  return(c(subset(res, EV == max(res$EV))$NR, max(res$EV)))
}


