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

#'Expected Value and Optimal Next Decision in the Lose Condition
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
  # Check if n_trials is divisible by n_players
  if (n_trials %% n_players != 0) {
    stop("Error: n_trials must be divisible by n_players.")
  }
  
  key <- paste(e, n_trials,  n_players, r1, r2, r3, s1, s2, payoff_r2, payoff_r3, payoff_s1, payoff_s2, coord = FALSE, sep = "-")
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

