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

# Throwaway project tree a run of calc_coalProbs() can live in: the two scripts
# it needs, a minimal election config and `polls` as data/surveys/.../polls.json.
# The working directory becomes the temp dir and both are reverted when the
# calling test ends. The scripts are sourced into the caller's frame, so
# calc_coalProbs() is callable there and finds its helpers in the same place.
# Returns the config path. Call it twice in one test to get two runs over the
# same tree (that is what exercises the merge with already-saved results).
local_calc_project <- function(polls, env = parent.frame()) {
  repo <- normalizePath(file.path("..", ".."))
  tmp  <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(tmp, .local_envir = env)

  dir.create("scripts", recursive = TRUE, showWarnings = FALSE)
  dir.create("config/elections", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/surveys/test-election", recursive = TRUE, showWarnings = FALSE)

  for (f in c("calc_coalProbs.R", "calc_coalProbs_helpers.R"))
    stopifnot(file.copy(file.path(repo, "scripts", f), file.path("scripts", f)))

  source("scripts/calc_coalProbs_helpers.R", local = env)
  source("scripts/calc_coalProbs.R",         local = env)

  config_path <- "config/elections/test.yml"
  writeLines(
    c("id: test-election",
      "name: Test Election",
      "parliament:",
      "  seats: 20",
      "  hurdle: 5",
      "  seat_allocation: sls",
      "parties:",
      "  - id: cdu",
      "    label: CDU",
      "    color: '#111111'",
      "    required: true",
      "  - id: spd",
      "    label: SPD",
      "    color: '#cc0000'",
      "    required: true",
      "  - id: greens",
      "    label: Greens",
      "    color: '#00aa00'",
      "    required: true",
      "  - id: others",
      "    label: Others",
      "    color: '#999999'",
      "    required: false",
      "coalitions:",
      "  - parties: [cdu]",
      "    label: CDU",
      "    color: '#111111'",
      "  - parties: [spd]",
      "    label: SPD",
      "    color: '#cc0000'",
      "  - parties: [cdu, spd]",
      "    label: CDU-SPD",
      "    color: '#111111'",
      "analyses:",
      "  biggest_party:",
      "    - parties: [cdu, spd, greens]"),
    config_path
  )

  write_polls(polls)
  config_path
}

# (Over)write the survey file the project built by local_calc_project() reads.
write_polls <- function(polls) {
  jsonlite::write_json(polls, "data/surveys/test-election/polls.json",
                       auto_unbox = TRUE, pretty = TRUE)
}

# Poll rows as scrape_election() leaves them in polls.json: one row per
# (pollster, date, party), for both a single institute and the pooled estimate.
make_polls_json <- function(dates, pct = c(cdu = 34, spd = 28, greens = 18, others = 20)) {
  # `pct` must not be called `percent`: inside transmute() the new column of that
  # name would shadow the lookup vector and `votes` would come out all NA.
  expand.grid(party = names(pct), pollster = c("insa", "pooled"), date = as.Date(dates),
              stringsAsFactors = FALSE) %>%
    transmute(pollster, date, party,
              percent  = unname(pct[party]),
              votes    = unname(pct[party]) * 10,
              election = "test-election")
}
