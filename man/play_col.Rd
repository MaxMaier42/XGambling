% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_play_col.R
\name{play_col}
\alias{play_col}
\title{Simulate a Single Playthrough of the Lose Condition with Coordination Options}
\usage{
play_col(
  e,
  max_n,
  n_players,
  r1,
  r2,
  r3,
  s1,
  s2,
  payoff_r2 = 0,
  payoff_r3 = 10,
  payoff_s1 = 0,
  payoff_s2 = 1,
  coord = FALSE,
  soft = FALSE,
  inv_temp = 10
)
}
\arguments{
\item{e}{The starting endowment (initial winnings) for the player(s)}

\item{max_n}{The maximum number of trials (total rounds) for the simulation}

\item{n_players}{The number of players participating in the simulation}

\item{r1}{The probability of extinction when choosing the risky lottery (outcome 1)}

\item{r2}{The probability of outcome 2 when choosing the risky lottery}

\item{r3}{The probability of outcome 3 when choosing the risky lottery}

\item{s1}{The probability of outcome 1 when choosing the safe lottery}

\item{s2}{The probability of outcome 2 when choosing the safe lottery}

\item{payoff_r2}{The payoff of outcome 2 when choosing the risky lottery (default = 0)}

\item{payoff_r3}{The payoff of outcome 3 when choosing the risky lottery (default = 10)}

\item{payoff_s1}{The payoff of outcome 1 when choosing the safe lottery (default = 0)}

\item{payoff_s2}{The payoff of outcome 2 when choosing the safe lottery (default = 1)}

\item{coord}{Boolean indicating whether players can coordinate on risky choices or not (default = FALSE)}

\item{soft}{Should softmax version be used? (default = FALSE)}

\item{inv_temp}{Inverse temperature for softmax (default = 10)}
}
\value{
A list containing the following elements:
\item{cumulative_winnings}{A vector of the cumulative winnings over the course of the round}
\item{risky_plays}{A logical vector indicating whether risky plays were made in each round}
\item{rounds}{The total number of risky plays made}
\item{extinction}{A boolean indicating whether an extinction event occurred during the round}
\item{rounds_alive}{A logical vector indicating whether players remained alive during each round of the game}
}
\description{
This function simulates a single round of the Lose Condition (where players do not get any bonus if they draw the extinction event).
It tracks cumulative winnings, the number of risky plays, the number of rounds completed, and whether extinction occurred.
}
\examples{
# Example: Simulate a single round with default payoffs and no coordination
cache_col <- list()
play_col(e = 0, max_n = 20, n_players = 5, r1 = 0.05, r2 = 0.475, r3 = 0.475, 
         s1 = 0.5, s2 = 0.5)
}
