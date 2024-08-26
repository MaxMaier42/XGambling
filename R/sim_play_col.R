
#' Simulate a Single Playthrough of the Lose Condition with Coordination Options
#'
#' This function simulates a single round of the Lose Condition (where players do not get any bonus if they draw the extinction event).
#' It tracks cumulative winnings, the number of risky plays, the number of rounds completed, and whether extinction occurred.
#'
#' @param e The starting endowment (initial winnings) for the player(s)
#' @param max_n The maximum number of trials (total rounds) for the simulation
#' @param n_players The number of players participating in the simulation
#' @param r1 The probability of extinction when choosing the risky lottery (outcome 1)
#' @param r2 The probability of outcome 2 when choosing the risky lottery
#' @param r3 The probability of outcome 3 when choosing the risky lottery
#' @param s1 The probability of outcome 1 when choosing the safe lottery
#' @param s2 The probability of outcome 2 when choosing the safe lottery
#' @param payoff_r2 The payoff of outcome 2 when choosing the risky lottery (default = 0)
#' @param payoff_r3 The payoff of outcome 3 when choosing the risky lottery (default = 10)
#' @param payoff_s1 The payoff of outcome 1 when choosing the safe lottery (default = 0)
#' @param payoff_s2 The payoff of outcome 2 when choosing the safe lottery (default = 1)
#' @param coord Boolean indicating whether players can coordinate on risky choices or not (default = FALSE)
#' @param soft Should softmax version be used? (default = FALSE)
#' @param inv_temp Inverse temperature for softmax (default = 10)
#' @returns A list containing the following elements:
#' \item{cumulative_winnings}{A vector of the cumulative winnings over the course of the round}
#' \item{risky_plays}{A logical vector indicating whether risky plays were made in each round}
#' \item{rounds}{The total number of risky plays made}
#' \item{extinction}{A boolean indicating whether an extinction event occurred during the round}
#' \item{rounds_alive}{A logical vector indicating whether players remained alive during each round of the game}
#' @export
#' @examples
#' # Example: Simulate a single round with default payoffs and no coordination
#' cache_col <- list()
#' play_col(e = 0, max_n = 20, n_players = 5, r1 = 0.05, r2 = 0.475, r3 = 0.475, 
#'          s1 = 0.5, s2 = 0.5)
play_col <- function(e, max_n, n_players, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, coord = FALSE, soft = FALSE, inv_temp = 10) {
  
  rounds_alive <- rep(TRUE, max_n / n_players)
  cumulative_winnings <- rep(0, max_n / n_players)
  risky_plays <- rep(FALSE, max_n / n_players)
  num_risky <- 0
  winnings_so_far <- 0
  
  ns <- seq(1, max_n, n_players)
  for (i in 1:(max_n/n_players)) {
    n <- ns[i]
    if(!soft){
      result <- E_col(winnings_so_far, n_trials = max_n - (n - 1), n_players, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, coord)
      play_risky <- result$play_risky
    }
    if(soft){
      result <- E_soft_col(winnings_so_far, n_trials = max_n - (n - 1), n_players, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, coord, inv_temp)
      play_risky <- sample(1:6, 1, prob = result$prob_risky)-1
    }
    
    risky_plays[i] <- play_risky
    num_risky <- num_risky + risky_plays[i]
    
    # Simulate risky outcomes
    outcome <- sample(c("X", "L", "W"), risky_plays[i], prob = c(r1, r2, r3), replace = TRUE)
    
    if (sum(outcome == "X") > 0) {  # Extinction event occurred
      if (i != (max_n / n_players)) {
        rounds_alive[(i + 1):(max_n / n_players)] <- FALSE
      }
      return(list(cumulative_winnings = cumulative_winnings, risky_plays = risky_plays, 
                  rounds = num_risky, extinction = TRUE, rounds_alive = rounds_alive))
    }
    
    # Update winnings
    winnings_so_far <- winnings_so_far + sum(outcome == "W") * payoff_r3
    if(n_players - risky_plays[i] > 0){
      winnings_so_far <- winnings_so_far + sum(sample(c(0, 1), n_players - risky_plays[i], prob = c(s1, s2), replace = TRUE)) * payoff_s2
    }
    #print(winnings_so_far)
    cumulative_winnings[i] <- winnings_so_far
  }
  
  return(list(cumulative_winnings = cumulative_winnings, risky_plays = risky_plays, 
              rounds = num_risky, extinction = FALSE, rounds_alive = rounds_alive))
}


#' Simulate Multiple Playthroughs of the Lose Condition with Coordination Options
#'
#' This function simulates multiple rounds of the Lose Condition (where players do not get any bonus if they draw the extinction event). 
#' It tracks cumulative winnings, the number of extinctions, the number of risky plays, and rounds alive across multiple iterations.
#'
#' @param e The starting endowment (initial winnings) for each player
#' @param max_n The maximum number of trials (total rounds) for the simulation
#' @param n_players The number of players participating in the simulation
#' @param r1 The probability of extinction when choosing the risky lottery (outcome 1)
#' @param r2 The probability of outcome 2 when choosing the risky lottery
#' @param r3 The probability of outcome 3 when choosing the risky lottery
#' @param s1 The probability of outcome 1 when choosing the safe lottery
#' @param s2 The probability of outcome 2 when choosing the safe lottery
#' @param payoff_r2 The payoff of outcome 2 when choosing the risky lottery (default = 0)
#' @param payoff_r3 The payoff of outcome 3 when choosing the risky lottery (default = 10)
#' @param payoff_s1 The payoff of outcome 1 when choosing the safe lottery (default = 0)
#' @param payoff_s2 The payoff of outcome 2 when choosing the safe lottery (default = 1)
#' @param precision The number of iterations to simulate (default = 1000)
#' @param clearCache Set to "col" to clear the cache
#' @param coord Boolean indicating whether players can coordinate on risky choices or not (default = FALSE)
#' @param soft Should softmax version be used?
#' @param inv_temp Inverse temperature for softmax
#' @returns A list containing the following elements:
#' \item{outcome}{The outcome of the last round of simulation (includes cumulative winnings, risky plays, etc.)}
#' \item{total_winnings}{A vector of total winnings for each iteration}
#' \item{num_extinctions}{The total number of extinctions that occurred across all iterations}
#' \item{all_risky_plays}{A matrix tracking the risky plays for each player across all iterations}
#' \item{all_alive}{A matrix tracking whether players remained alive across all iterations}
#' @export
#' @examples
#' cache_col <- list()
#' simulate_col(e = 0, max_n = 20, n_players = 5, r1 = 0.05, r2 = 0.475, r3 = 0.475, 
#'              s1 = 0.5, s2 = 0.5, payoff_r2 = 5, payoff_r3 = 20, 
#'              payoff_s1 = 0, payoff_s2 = 2, precision = 1000, coord = TRUE)

simulate_col <- function(e, max_n, n_players, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, precision = 1000, clearCache = "None", coord = FALSE, soft = FALSE, inv_temp = 10) {
  
  num_extinctions <- 0
  
  all_risky_plays <- matrix(FALSE, nrow = precision, ncol = max_n / n_players)
  all_alive <- matrix(FALSE, nrow = precision, ncol = max_n / n_players)
  total_winnings <- vector(mode = "numeric", length = precision)
  
  if (clearCache == "col") {
    cache_col <<- list()
  }
  
  for (round in 1:precision) {
    outcome <- play_col(e, max_n, n_players, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, coord, soft, inv_temp)
    
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
