#'Expected Value and Optimal Next Decision in the Lose Condition
#'
#'This function calculates the optimal choice and associated expected payoff given the endowment, the remaining trials, and the
#'payoffs and associated probabilities for the Lose Condition (i.e., condition, where particpants don't get any bonus if they draw the extinction event).
#' @param e The current endowment
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
#' @param amb_risk The background extinction risk independent of the players action.
#' @returns A list with the expected payoff when following the optimal strategy, the optimal next choice, a
#' and a softmax transformation with inverse temperature 50 containing the probabilities of the next choice 
#' (this helps pick up on cases, where there are only tiny EV differences between playing safe and risky)
#' @export
#' @examples
#' cache <- list()
#' E_lose(e = 0, n = 100, r1 = 0.05, r2 = 0.475, r3 = 0.475, s1 = 0.5, s2 = 0.5,
#'  payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1)
E_lose <- function(e, n, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, amb_risk = 0) {
  key <- paste(e, n, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, amb_risk, sep = "-")
  amb_surv <- 1-amb_risk
  if (key %in% names(cache)) {
    return(cache[[key]])
  }
  if (n == 0) {
    result <- list(value = e, play_risky = NULL)
  } else {
    E_risky <- amb_surv*r2 * E_lose(e + payoff_r2, n - 1, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, amb_risk)$value + 
      amb_surv*r3 * E_lose(e + payoff_r3, n - 1, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, amb_risk)$value
    E_safe <- amb_surv*s1 * E_lose(e + payoff_s1, n - 1, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, amb_risk)$value + 
      amb_surv*s2 * E_lose(e + payoff_s2, n - 1, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, amb_risk)$value
    play_risky <- E_risky > E_safe
    prob_risky <- reservr::softmax(50 * c(E_risky, E_safe))
    result <- list(value = max(E_risky, E_safe), play_risky = play_risky, prob_risky = prob_risky)
  }
  
  cache[[key]] <<- result
  return(result)
}


#'Expected Value and Probability of Next Decision of Optimal Strategy in the Lose Condition Using Softmax
#'
#'This function calculates the optimal choice and associated expected payoff given the endowment, the remaining trials, and the
#'payoffs and associated probabilities for the Lose Condition (i.e., condition, where particpants don't get any bonus if they draw the extinction event). 
#'Unlike E_lose it uses a softmax transformation to convert the difference 
#'in EV between the risky and safe choice to a choice probability 
#' @param e The current endowment
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
#' @param inv_temp Inverse temperature for the softmax decision function
#' @returns A list with the expected payoff when following the optimal softmax strategy and the probability of playing safe and risky
#' @export
#' @examples
#' soft_cache <- list()
#' E_soft_lose(e = 0, n = 10, r1 = 0.05, r2 = 0.475, r3 = 0.475, s1 = 0.5, 
#' s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1)
E_soft_lose <- function(e, n, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, inv_temp = 10) {
  key <- paste(e, n, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, inv_temp, sep = "-")
  
  if (key %in% names(soft_cache)) {
    return(soft_cache[[key]])
  }
  
  if (n == 0) {
    result <- list(value = e, risky_prob = NULL, safe_prob = NULL)
  } else {
    E_risky <- r2 * E_soft_lose(e + payoff_r2, n - 1, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, inv_temp)$value + 
      r3 * E_soft_lose(e + payoff_r3, n - 1, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, inv_temp)$value
    
    E_safe <- s1 *  E_soft_lose(e + payoff_s1, n - 1, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, inv_temp)$value + 
      s2 * E_soft_lose(e + payoff_s2, n - 1, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, inv_temp)$value
    
    probs <- reservr::softmax(inv_temp * c(E_risky, E_safe))
    risky_prob <- probs[1]
    safe_prob <- probs[2]
    
    expected_winnings <- risky_prob * E_risky + safe_prob * E_safe
    
    result <- list(value = expected_winnings, risky_prob = risky_prob, safe_prob = safe_prob)
  }
  
  soft_cache[[key]] <<- result
  return(result)
}

