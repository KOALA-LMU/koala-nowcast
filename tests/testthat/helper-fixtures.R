# Build the long seat-distribution data.frame expected by calc_allCoalProbs():
# one row per (simulation, party). seat_matrix has one row per simulation and
# one column per party, in the same order as `parties`.
make_seats <- function(parties, seat_matrix) {
  nsim <- nrow(seat_matrix)
  data.frame(
    sim   = rep(seq_len(nsim), each = length(parties)),
    party = rep(parties, times = nsim),
    seats = as.vector(t(seat_matrix)),
    stringsAsFactors = FALSE
  )
}

# Named share matrix (rows = simulations, cols = parties) for the Dirichlet draws.
make_shares <- function(parties, share_matrix) {
  colnames(share_matrix) <- parties
  share_matrix
}

# A wide survey row shaped like the scrapers return it.
make_wide_poll <- function(..., pollster = "insa", date = as.Date("2026-08-01"),
                           respondents = 1000) {
  data.frame(pollster = pollster, date = date, respondents = respondents, ...,
             stringsAsFactors = FALSE)
}

# Long poll rows, one per party, as they look after collapse_parties()/unnest().
make_long_poll <- function(pollster, date, parties, percent) {
  data.frame(
    pollster = pollster,
    date     = as.Date(date),
    party    = parties,
    percent  = percent,
    stringsAsFactors = FALSE
  )
}
