#'Function to Simulate a Single Agent Playing the Task and Following the Optimal Strategy
#'
#'This function simulates an agent that plays the extinction gambling task following the optimal dynamic strategy.
#'It only applies to the Lose and the Reset condition as the Keep condition does not have a dynamic strategy
#' @importFrom stats runif
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
#' @param amb_risk The background extinction risk independent of the players action (currently only for E_lose).
#' @returns A list with the cumulative winnings during the task, whether the agent played risky or safe on each trial, 
#' the number of risky plays, whether the agent went extinction, and how many rounds they were alive.
#' @export
#' @examples
#' #Optimal Strategy for Maier et al. (in prep)
#' soft_cache_lc <- list()
#' play(e = 0, max_n = 10, r1 = 0.05, r2 = 0.475, r3 = 0.475, s1 = 0.5, 
#' s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, 
#' soft = TRUE, type = "reset", inv_temp = 50)
play <- function(e, max_n, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, soft=FALSE, type="lose", inv_temp=NULL, amb_risk = 0) {
  rounds_alive <- rep(TRUE, max_n)
  cumulative_winnings <- rep(0, max_n)
  risky_plays <- rep(FALSE, max_n)
  num_risky <- 0
  winnings_so_far <- e
  amb_surv <- 1-amb_risk
  
  for (n in 1:max_n) {
    if (soft) {
      if (type == "lose") {
        result_soft <- E_soft_lose(winnings_so_far, max_n - (n - 1), r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, inv_temp)
      } else if (type == "reset") {
        result_soft <- E_soft_reset(winnings_so_far, max_n - (n - 1), r1, r2, r3, s1, s2, inv_temp, payoff_r2, payoff_r3, payoff_s1, payoff_s2)
      }
      play_risky <- runif(1) < result_soft$risky_prob
    } else {
      if (type == "lose") {
        result <- E_lose(winnings_so_far, max_n - (n - 1), r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, amb_risk = amb_risk)
      } else if (type == "reset") {
        result <- E_reset(winnings_so_far, max_n - (n - 1), r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2)
      }
      play_risky <- result$play_risky
    }
    
    risky_plays[n] <- play_risky
    
    if (play_risky) {
      num_risky <- num_risky + 1
      outcome <- sample(c("X", "again"), 1, prob = c(amb_risk, amb_surv))
      if(outcome == "again"){
        outcome <- sample(c("X", "L", "W"), 1, prob=c(r1, r2, r3))
      }
      if (outcome == "X" & type == "lose") {
        if (n != max_n) {
          rounds_alive[(n+1):max_n] <- FALSE
          return(list(cumulative_winnings=cumulative_winnings, risky_plays=risky_plays, 
                      rounds=num_risky, extinction=TRUE, rounds_alive=rounds_alive))
        } else {
          return(list(cumulative_winnings=cumulative_winnings, risky_plays=risky_plays, 
                      rounds=num_risky, extinction=TRUE, rounds_alive=rounds_alive))
        }
      }
      if (outcome == "X" & type == "reset") {
          winnings_so_far <- 0
      }
      else if (outcome == "W") {
        winnings_so_far <- winnings_so_far + payoff_r3
      } else {
        winnings_so_far <- winnings_so_far + payoff_r2
      }
    } else {
      
      outcome <- sample(c("X", "again"), 1, prob = c(amb_risk, amb_surv))
      if (outcome == "X" & type == "lose") {
        if (n != max_n) {
          rounds_alive[(n+1):max_n] <- FALSE
          return(list(cumulative_winnings=cumulative_winnings, risky_plays=risky_plays, 
                      rounds=num_risky, extinction=TRUE, rounds_alive=rounds_alive))
        } else {
          return(list(cumulative_winnings=cumulative_winnings, risky_plays=risky_plays, 
                      rounds=num_risky, extinction=TRUE, rounds_alive=rounds_alive))
        }
      } 
      if (outcome == "X" & type == "reset") {
        winnings_so_far <- 0
      } 
      else {
        winnings_so_far <- winnings_so_far + sample(c(payoff_s1, payoff_s2), 1, prob=c(s1, s2))
      }
      
    }
    
    cumulative_winnings[n] <- winnings_so_far
  }
  
  return(list(cumulative_winnings=cumulative_winnings, risky_plays=risky_plays, 
              rounds=num_risky, extinction=FALSE, rounds_alive=rounds_alive))
}


