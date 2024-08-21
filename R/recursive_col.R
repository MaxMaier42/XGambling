get_perms_risky <- function(n_risky, r1, r2, r3){
  if(n_risky == 0){
    perm_risky <- data.frame(0, 0, 0, 1)
    colnames(perm_risky) <- c("nX", "n0r", "n10r", "prob")
    return(perm_risky)
  }
  
  X <- t(as.matrix(expand.grid(0:n_risky, 0:n_risky))); X <- X[, colSums(X) <= n_risky]
  X <- rbind(X, n_risky - colSums(X))
  X <- t(X)
  X <- cbind(X, apply(X, 1, function(x) dmultinom(x, prob = c(r1, r2, r3))))
  perm_risky <- data.frame(X)
  colnames(perm_risky) <- c("nX", "n0r", "n10r", "prob")
  perm_risky <- subset(perm_risky, nX == 0)
  return(perm_risky)
}

get_perms_safe <- function(n_safe, s1, s2){
  if(n_safe == 0){
    perm_safe <- data.frame(0, 0, 1)
    colnames(perm_safe) <- c("n0s", "n1s", "prob")
    return(perm_safe)
  }
  Y <- t(as.matrix(expand.grid(0:n_safe))); Y <- Y[, colSums(Y) <= n_safe]
  Y <- rbind(Y, n_safe - Y)
  Y <- t(Y)
  Y <- cbind(Y, apply(Y, 1, function(x) dmultinom(x, prob = c(s1, s2))))
  perm_safe <- data.frame(Y)
  colnames(perm_safe) <- c("n0s", "n1s", "prob")
  return(perm_safe)
}

#'Expected Value and Optimal Next Decision in the Collective Game Condition
#'
#'This function calculates the optimal choice and associated expected payoff given the endowment, the remaining trials, and the
#'payoffs and associated probabilities for the Lose Condition (i.e., condition, where participants don't get any bonus if they draw the extinction event).
#' @param e The current endowment
#' @param n_trials The remaining number of trials for all players (e.g., if 5 players play 5 trials each this number would be 25)
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
#' E_col(e = 0, n_trials = 20, n_players = 5, r1 = 0.05, r2 = 0.475, 
#' r3 = 0.475, s1 = 0.5, s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, 
#' payoff_s2 = 1)
E_col <- function(e, n_trials, n_players, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, coord = FALSE) {

  if (n_trials %% n_players != 0) {
    stop("Error: n_trials must be divisible by n_players.")
  }
  
  key <- paste(e, n_trials, n_players, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, coord, sep = "-")
  if (key %in% names(cache_col)) {
    return(cache_col[[key]])
  }
  
  if (n_trials == 0) {
    result <- list(value = e, play_risky = NULL)
  } else {
    n_risky <- 0:n_players
    EV_n <- rep(0, n_players + 1)
    for (i in n_risky) {
      # Calculate the possible survival outcomes and associated probabilities
      perm_risky <- get_perms_risky(i, r1, r2, r3)
      perm_safe <- get_perms_safe(n_players - i, s1, s2)
      m <- merge(perm_risky, perm_safe, by = NULL)
      m$p <- m$prob.x * m$prob.y
      
      # Calculate the expected value 
      for (j in 1:nrow(m)) {
        EV_n[i + 1] <- EV_n[i + 1] + m$p[j] * E_col(e + payoff_r3 * m$n10r[j] + payoff_s2 * m$n1s[j], n_trials - n_players, n_players, r1, r2, r3, s1, s2, payoff_r2 = payoff_r2, payoff_r3 = payoff_r3, payoff_s1 = payoff_s1, payoff_s2 = payoff_s2, coord = coord)$value
      }
    }
    index <- which(EV_n == max(EV_n))


    if(length(index > 1)) index <- index[1]
    if (!coord) {
      if (index > 1) {
        result <- list(value = EV_n[n_players + 1], play_risky = n_players)
      } else {
        result <- list(value = max(EV_n), play_risky = index - 1)
      }
    }
    if (coord) {
      result <- list(value = max(EV_n), play_risky = index - 1)
    }
  }
  
  # Print key
  cache_col[[key]] <<- result
  return(result)
}



#'Softmax Expected Value and Optimal Next Decision in the Lose Condition
#'
#'This function calculates the optimal choice and associated expected payoff given the endowment, the remaining trials, and the
#'payoffs and associated probabilities for the Lose Condition (i.e., condition, where participants don't get any bonus if they draw the extinction event).
#' @param e The current endowment
#' @param n_trials The remaining number of trials for all players (e.g., if 5 players play 5 trials each this number would be 25)
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
#' @param inv_temp Softmax inverse temperature
#' @returns A list with the expected payoff when following the optimal strategy and the optimal next choice.
#' @export
#' @examples
#' cache_soft_col <- list()
#' E_soft_col(e = 0, n_trials = 20, n_players = 5, r1 = 0.05, r2 = 0.475, 
#' r3 = 0.475, s1 = 0.5, s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, 
#' payoff_s2 = 1)
E_soft_col <- function(e, n_trials, n_players, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, coord = FALSE, inv_temp = 10) {
  # Check if n_trials is divisible by n_players
  if (n_trials %% n_players != 0) {
    stop("Error: n_trials must be divisible by n_players.")
  }
  
  key <- paste(e, n_trials, n_players, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, coord, sep = "-")
  if (key %in% names(cache_soft_col)) {
    return(cache_soft_col[[key]])
  }
  
  if (n_trials == 0) {
    result <- list(value = e, play_risky = NULL)
  } else {
    n_risky <- 0:n_players
    EV_n <- rep(0, n_players + 1)
    for (i in n_risky) {
      # Calculate the possible survival outcomes and associated probabilities
      perm_risky <- get_perms_risky(i, r1, r2, r3)
      perm_safe <- get_perms_safe(n_players - i, s1, s2)
      m <- merge(perm_risky, perm_safe, by = NULL)
      m$p <- m$prob.x * m$prob.y
      
      # Calculate the expected value 
      for (j in 1:nrow(m)) {
        EV_n[i + 1] <- EV_n[i + 1] + m$p[j] * E_soft_col(e + payoff_r3 * m$n10r[j] + payoff_s2 * m$n1s[j], n_trials - n_players, n_players, r1, r2, r3, s1, s2, payoff_r2 = payoff_r2, payoff_r3 = payoff_r3, payoff_s1 = payoff_s1, payoff_s2 = payoff_s2, coord = coord, inv_temp)$value
      }
    }
    
    probs <- reservr::softmax(inv_temp * EV_n)  

    if (!coord) {
      EV <- probs[1]*EV_n[1] + sum(probs[2:6])*EV_n[n_players+1]
      result <- list(value = EV, prob_risky = c(probs[1], rep(0, n_players -1), sum(probs[2:6])))
      
    }
    if (coord) {
      result <- list(value = sum(probs*EV_n), prob_risky = probs)
    }
  }
  
  # Print key
  cache_soft_col[[key]] <<- result
  return(result)
}
# cache_soft_col <- list()
# E_soft_col(e = 0, n_trials = 20, n_players = 5, r1 = 0.05, r2 = 0.475, 
# r3 = 0.475, s1 = 0.5, s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, 
# payoff_s2 = 1, coord = TRUE)
# cache_col <- list()
# E_col(e = 0, n_trials = 20, n_players = 5, r1 = 0.05, r2 = 0.475, 
#            r3 = 0.475, s1 = 0.5, s2 = 0.5, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, 
#            payoff_s2 = 1, coord = TRUE)

#' Simulate a Single Round of the Lose Condition with Coordination Options
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
#' @param soft Should softmax version be used?
#' @param inv_temp Inverse temperature for softmax
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
    print(winnings_so_far)
    cumulative_winnings[i] <- winnings_so_far
  }
  
  return(list(cumulative_winnings = cumulative_winnings, risky_plays = risky_plays, 
              rounds = num_risky, extinction = FALSE, rounds_alive = rounds_alive))
}

#' Simulate Multiple Rounds of the Lose Condition with Coordination Options
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

simulate_col <- function(e, max_n, n_players, r1, r2, r3, s1, s2, payoff_r2 = 0, payoff_r3 = 10, payoff_s1 = 0, payoff_s2 = 1, precision = 1000, clearCache = "None", coord = FALSE, soft, inv_temp) {
  
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
