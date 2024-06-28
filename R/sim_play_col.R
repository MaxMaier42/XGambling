#'Function to Play a Single Run Through of the Collective Goods Game Version of the Extinction Gambling Task
#'
#'This function simulates n agents that plays the extinction gambling task following the optimal dynamic strategy.
#' @param e The current endowment
#' @param max_n The remaining number of trials for all players (e.g., if 5 players play 5 trials each this number would be 25)
#' @param n_players The number of players
#' @param r1 The probability of extinction when choosing the risky lottery (outcome 1)
#' @param r2 The probability of outcome 2 when choosing the risky lottery 
#' @param r3 The probability of outcome 3 when choosing the risky lottery
#' @param s1 the probability of outcome 1 when choosing the safe lottery
#' @param s2 the probability of outcome 2 when choosing the safe lottery
#' @param payoff_r2 The payoff of outcome 2 when choosing the risky lottery
#' @param payoff_r3 The payoff of outcome 3 when choosing the risky lottery
#' @param payoff_s1 The payoff of outcome 1 when choosing the safe lottery
#' @param payoff_s2 The payoff of outcome 2 when choosing the safe lottery
#' @param coord Whether players can coordinate (e.g., through text) to hit a specific target number of risky choices or not 
#' @returns A list with the expected payoff when following the optimal strategy and the optimal next choice.
#' @export
#' @examples
#' cache_col <- list()
#' play_col(e = 0, max_n = 20, n_players = 5, r1 = 0.05, r2 = 0.475, 
#' r3 = 0.475, s1 = 0.5, s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, 
#' payoff_s2 = 1, coord = FALSE)
play_col <- function(e, max_n, n_players, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, coord = FALSE) {
  
  rounds_alive <- rep(TRUE, max_n / n_players)
  cumulative_winnings <- rep(0, max_n / n_players)
  risky_plays <- rep(FALSE, max_n / n_players)
  num_risky <- 0
  winnings_so_far <- 0
  
  ns <- seq(1, max_n, n_players)
  for (i in 1:(max_n / n_players)) {
    n <- ns[i]
    result <- E_col(e = winnings_so_far, n_trials = max_n - (n - 1), n_players = n_players, 
                    r1 = r1, r2 = r2, r3 = r3, s1 = s1, s2 = s2, 
                    payoff_r2 = payoff_r2, payoff_r3 = payoff_r3, payoff_s1 = payoff_s1, payoff_s2 = payoff_s2, coord = coord)
    play_risky <- result$play_risky
    
    risky_plays[i] <- play_risky
    
    num_risky <- num_risky + risky_plays[i]
    outcome_risky <- sample(c("X", "L", "W"), play_risky, prob = c(r1, r2, r3), replace = TRUE)
    outcome_safe <- sample(c("L", "W"), n_players - play_risky, prob = c(s1, s2), replace = TRUE)
    
    if (sum(outcome_risky == "X") > 0) {
      if (i != (max_n / n_players)) {
        rounds_alive[(i + 1):(max_n / n_players)] <- FALSE
        return(list(cumulative_winnings = cumulative_winnings, risky_plays = risky_plays, 
                    rounds = num_risky, extinction = TRUE, rounds_alive = rounds_alive))
      } else {
        return(list(cumulative_winnings = cumulative_winnings, risky_plays = risky_plays, 
                    rounds = num_risky, extinction = TRUE, rounds_alive = rounds_alive))
      }
    }
    winnings_so_far <- winnings_so_far + sum(outcome_risky == "W") * payoff_r3
    winnings_so_far <- winnings_so_far + sum(outcome_risky == "L") * payoff_r2
    winnings_so_far <- winnings_so_far + sum(outcome_safe == "W") * payoff_s2
    winnings_so_far <- winnings_so_far + sum(outcome_safe == "L") * payoff_s1
    
    cumulative_winnings[i] <- winnings_so_far
  }
  
  return(list(cumulative_winnings = cumulative_winnings, risky_plays = risky_plays, 
              rounds = num_risky, extinction = FALSE, rounds_alive = rounds_alive))
}


#'Function to Simulate Many Runds of the Collective Goods Game Version of the Extinction Gambling Task
#'
#'This function simulates a number of games of players playing the collective extintion gambling task and following the optimal dynamic strategy.
#' @param e The current endowment
#' @param max_n The remaining number of trials for all players (e.g., if 5 players play 5 trials each this number would be 25)
#' @param n_players The number of players
#' @param r1 The probability of extinction when choosing the risky lottery (outcome 1)
#' @param r2 The probability of outcome 2 when choosing the risky lottery 
#' @param r3 The probability of outcome 3 when choosing the risky lottery
#' @param s1 the probability of outcome 1 when choosing the safe lottery
#' @param s2 the probability of outcome 2 when choosing the safe lottery
#' @param payoff_r2 The payoff of outcome 2 when choosing the risky lottery
#' @param payoff_r3 The payoff of outcome 3 when choosing the risky lottery
#' @param payoff_s1 The payoff of outcome 1 when choosing the safe lottery
#' @param payoff_s2 The payoff of outcome 2 when choosing the safe lottery
#' @param coord Whether players can coordinate (e.g., through text) to hit a specific target number of risky choices or not 
#' @param precision How many games should be simulated
#' @returns A list with the expected payoff when following the optimal strategy and the optimal next choice.
#' @export
#' @examples
#' cache_col <- list()
#' simulate_col(e = 0, max_n = 20, n_players = 5, r1 = 0.05, r2 = 0.475, 
#' r3 = 0.475, s1 = 0.5, s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, 
#' payoff_s2 = 1, coord = FALSE, precision = 1000)
simulate_col <- function(e, max_n, n_players, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, coord = FALSE, precision = 1000) {
  num_extinctions <- 0
  
  all_risky_plays <- matrix(FALSE, nrow = precision, ncol = max_n / n_players)
  all_alive <- matrix(FALSE, nrow = precision, ncol = max_n / n_players)
  total_winnings <- vector(mode = "numeric", length = precision)
  
  for (round in 1:precision) {
    outcome <- play_col(e, max_n, n_players, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, coord)
    total_winnings[round] <- tail(outcome$cumulative_winnings, n = 1)
    
    if (outcome$extinction) {
      num_extinctions <- num_extinctions + 1
    }
    
    all_risky_plays[round, ] <- outcome$risky_plays
    all_alive[round, ] <- outcome$rounds_alive
    print(round)
  }
  
  return(list("outcome" = outcome, 
              "total_winnings" = total_winnings, 
              "num_extinctions" = num_extinctions,
              "all_risky_plays" = all_risky_plays, 
              "all_alive" = all_alive))
}





