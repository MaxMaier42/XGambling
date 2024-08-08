#'Function to Simulate a Large Number of Agents Playing the Task and Following the Optimal Strategy
#'
#'This function simulates a number of agents who play the extinction gambling task following the optimal dynamic strategy.
#'It only applies to the Lose and the Reset condition as the Keep condition does not have a dynamic strategy
#' @importFrom utils tail
#' @param e The current endowment
#' @param max_n The remaining number of trials 
#' @param r1 The probability of extinction when choosing the risky lottery (outcome 1)
#' @param r2 The probability of outcome 2 when choosing the risky lottery 
#' @param r3 The probability of outcome 3 when choosing the risky lottery
#' @param s1 the probability of outcome 1 when choosing the safe lottery
#' @param s2 the probability of outcome 2 when choosing the safe lottery
#' @param payoff_r2 The payoff of outcome 2 when choosing the risky lottery
#' @param payoff_r3 The payoff of outcome 3 when choosing the risky lottery
#' @param payoff_s1 The payoff of outcome 1 when choosing the safe lottery
#' @param payoff_s2 The payoff of outcome 2 when choosing the safe lottery
#' @param soft whether softmax or deterministic version should be used (TRUE or FALSE)
#' @param type whether "lose" or "reset" version is used
#' @param inv_temp inverse temperature for the softmax transformation
#' @param precision the number of agents being simulated
#' @param amb_risk The background extinction risk independent of the players action (currently only for E_lose).
#' @returns A list containing "total_winnings" a vector with the earnings for each agent, "num_extinctions" containing the number of extinctions, 
#' "all_risky_plays" a dataframe with the risky plays over time for each agent, and "all_alive" denoting for each agent and trial whether they are alive.
#' @export
#' @examples
#' soft_cache <- list()
#' simulate(e = 0, max_n = 10, r1 = 0.05, r2 = 0.475, r3 = 0.475, s1 = 0.5, 
#' s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, 
#' soft = TRUE, type = "lose", inv_temp = 10, precision = 10)
simulate <- function(e, max_n, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, 
                     soft = TRUE, type = "lose", inv_temp = 10, precision = 1000, amb_risk = 0) {
  num_extinctions <- 0
  
  all_risky_plays <- matrix(FALSE, nrow=precision, ncol=max_n)
  all_alive <- matrix(FALSE, nrow=precision, ncol=max_n)
  total_winnings <- vector(mode = "numeric", length = precision)
  
  for (round in 1:precision) {
    outcome <- play(e, max_n, r1, r2, r3, s1, s2, payoff_r2 = payoff_r2, payoff_r3 = payoff_r3, payoff_s1 = payoff_s1, payoff_s2 = payoff_s2, 
                    soft = soft, type = type, inv_temp = inv_temp, amb_risk = amb_risk)
    total_winnings[round] <- tail(outcome$cumulative_winnings, n=1)
    
    if (outcome$extinction) {
      num_extinctions <- num_extinctions + 1
    }
    
    all_risky_plays[round, ] <- outcome$risky_plays
    all_alive[round, ] <- outcome$rounds_alive
    print(round)
  }
  
  return(list("total_winnings" = total_winnings, 
              "num_extinctions" = num_extinctions,
              "all_risky_plays" = all_risky_plays, 
              "all_alive" = all_alive))
}