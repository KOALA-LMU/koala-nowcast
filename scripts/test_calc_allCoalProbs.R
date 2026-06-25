
# Test script for calc_allCoalProbs (and the simplified strongest-party logic)
# Run from the project root: Rscript scripts/test_calc_allCoalProbs.R

library(dplyr)
source("scripts/calc_coalProbs_helpers.R")

# ── Helpers ──────────────────────────────────────────────────────────────────

# Build the long seat-distribution data.frame expected by calc_allCoalProbs.
# seat_matrix: one row per simulation, one column per party (in same order as parties).
# Total seats per simulation must be consistent (all rows should sum to the same value).
make_seats <- function(parties, seat_matrix) {
  nsim <- nrow(seat_matrix)
  data.frame(
    sim   = rep(seq_len(nsim), each = length(parties)),
    party = rep(parties, times = nsim),
    seats = as.vector(t(seat_matrix)),   # row-major: sim1-party1, sim1-party2, sim2-party1, …
    stringsAsFactors = FALSE
  )
}

# Build a named share matrix (rows = sims, cols = parties) for the Dirichlet draws.
make_shares <- function(parties, share_matrix) {
  colnames(share_matrix) <- parties
  share_matrix
}

pass <- function(desc) cat(sprintf("  PASS  %s\n", desc))
fail <- function(desc, msg) cat(sprintf("  FAIL  %s — %s\n", desc, msg))

check <- function(desc, expr) {
  result <- tryCatch(expr, error = function(e) e)
  if (inherits(result, "error")) {
    fail(desc, conditionMessage(result))
  } else if (isTRUE(result)) {
    pass(desc)
  } else {
    fail(desc, paste("got:", deparse(result)))
  }
}

run <- function(desc, ...) {
  tryCatch(calc_allCoalProbs(...), error = function(e) {
    cat(sprintf("  ERROR in '%s': %s\n", desc, conditionMessage(e)))
    NULL
  })
}

# ── Test 1: No strongest-party coalitions ────────────────────────────────────
cat("\nTest 1: no strongest_party_coals — basic majority and subset logic\n")
{
  parties <- c("cdu", "spd", "greens")
  # 100 seats total per sim
  # sim 1: cdu alone has majority (55 > 50)
  # sim 2: no solo majority; spd+greens have majority (40+35=75 > 50), cdu does not (25)
  seats <- make_seats(parties, matrix(c(
    55, 22, 23,
    25, 40, 35
  ), nrow = 2, byrow = TRUE))

  shares <- make_shares(parties, matrix(c(
    0.55, 0.22, 0.23,
    0.25, 0.40, 0.35
  ), nrow = 2, byrow = TRUE))

  res <- run("Test 1", seats, parties, shares)
  if (!is.null(res)) {
    cp <- res$coalProbs

    check("all coalition names unique", !anyDuplicated(cp$coalition))

    # sim 1 only: cdu wins alone → prob = 0.5
    check("cdu solo prob = 0.5",
      abs(cp$coal_prob[cp$coalition == "cdu"] - 0.5) < 1e-9)

    # sim 2 only: spd|greens wins (no subset has majority) → prob = 0.5
    check("spd|greens prob = 0.5", {
      # combn puts spd before greens since spd is earlier in parties
      p <- cp$coal_prob[cp$coalition == "spd|greens"]
      length(p) == 1 && abs(p - 0.5) < 1e-9
    })

    # 3-party coalition never needed (subset always suffices or solo wins)
    check("3-party coalition prob = 0",
      all(cp$coal_prob[cp$coal_size == 3] == 0))
  }
}

# ── Test 2: 2-party strongest-party coalitions ───────────────────────────────
cat("\nTest 2: 2-party strongest_party_coals — cdu|spd vs spd|cdu\n")
{
  # Three parties so neither cdu nor spd alone has a majority.
  # cdu+spd always form the winning coalition (40+35=75 seats out of 100).
  parties <- c("cdu", "spd", "greens")

  seats <- make_seats(parties, matrix(c(
    40, 35, 25,   # sim 1: cdu+spd win; cdu leads in vote share
    40, 35, 25,   # sim 2: cdu+spd win; spd leads in vote share
    40, 35, 25    # sim 3: cdu+spd win; cdu leads in vote share
  ), nrow = 3, byrow = TRUE))

  # Vote shares decide the leader for strongest-party logic
  shares <- make_shares(parties, matrix(c(
    0.45, 0.30, 0.25,   # sim 1: cdu leads
    0.30, 0.45, 0.25,   # sim 2: spd leads
    0.50, 0.25, 0.25    # sim 3: cdu leads
  ), nrow = 3, byrow = TRUE))

  spc <- c("cdu|spd", "spd|cdu")
  res <- run("Test 2", seats, parties, shares, strongest_party_coals = spc)
  if (!is.null(res)) {
    cp <- res$coalProbs

    check("all coalition names unique", !anyDuplicated(cp$coalition))
    check("cdu|spd present in results", "cdu|spd" %in% cp$coalition)
    check("spd|cdu present in results", "spd|cdu" %in% cp$coalition)

    # cdu leads in sims 1 & 3 → prob = 2/3
    check("cdu|spd prob = 2/3",
      abs(cp$coal_prob[cp$coalition == "cdu|spd"] - 2/3) < 1e-9)

    # spd leads in sim 2 → prob = 1/3
    check("spd|cdu prob = 1/3",
      abs(cp$coal_prob[cp$coalition == "spd|cdu"] - 1/3) < 1e-9)

    # The two ordered variants together account for all 3 simulations
    check("cdu|spd + spd|cdu probs sum to 1", {
      p <- sum(cp$coal_prob[cp$coalition %in% spc])
      abs(p - 1) < 1e-9
    })

    # Unordered variants must NOT appear (only the ordered ones should)
    check("no unordered cdu|spd duplicate",
      sum(cp$coalition == "cdu|spd") == 1)
  }
}

# ── Test 3: 3-party strongest-party coalitions ───────────────────────────────
cat("\nTest 3: 3-party strongest_party_coals\n")
{
  # 4 parties; no 3-party subset has majority on its own.
  # Only the full {cdu, spd, greens} 3-way coalition ever wins.
  parties <- c("cdu", "spd", "greens", "fdp")

  # cdu+spd+greens = 34+33+33 = 100 seats (majority = 51); fdp gets 0 here for simplicity
  seats <- make_seats(parties, matrix(c(
    34, 33, 33, 0,   # sim 1: cdu leads in vote share
    33, 34, 33, 0,   # sim 2: spd leads
    33, 33, 34, 0    # sim 3: greens leads
  ), nrow = 3, byrow = TRUE))

  shares <- make_shares(parties, matrix(c(
    0.34, 0.33, 0.33, 0.00,
    0.33, 0.34, 0.33, 0.00,
    0.33, 0.33, 0.34, 0.00
  ), nrow = 3, byrow = TRUE))

  spc <- c("cdu|spd|greens", "spd|cdu|greens", "greens|spd|cdu")
  res <- run("Test 3", seats, parties, shares, strongest_party_coals = spc)
  if (!is.null(res)) {
    cp <- res$coalProbs

    check("all coalition names unique", !anyDuplicated(cp$coalition))
    check("cdu|spd|greens present",  "cdu|spd|greens"  %in% cp$coalition)
    check("spd|cdu|greens present",  "spd|cdu|greens"  %in% cp$coalition)
    check("greens|spd|cdu present",  "greens|spd|cdu"  %in% cp$coalition)

    check("cdu-led prob = 1/3",
      abs(cp$coal_prob[cp$coalition == "cdu|spd|greens"] - 1/3) < 1e-9)
    check("spd-led prob = 1/3",
      abs(cp$coal_prob[cp$coalition == "spd|cdu|greens"] - 1/3) < 1e-9)
    check("greens-led prob = 1/3",
      abs(cp$coal_prob[cp$coalition == "greens|spd|cdu"] - 1/3) < 1e-9)

    check("3-party ordered probs sum to 1",
      abs(sum(cp$coal_prob[cp$coalition %in% spc]) - 1) < 1e-9)

    # YAML orderings must be used exactly (not internally remapped)
    check("coal names match YAML strings exactly",
      all(spc %in% cp$coalition))
  }
}

# ── Test 4: Minimal winning coalition rule ────────────────────────────────────
cat("\nTest 4: minimal winning coalition — subset majority zeros out larger coalition\n")
{
  parties <- c("cdu", "spd", "greens")
  # sim 1: cdu+spd have majority (55+30=85) so cdu+spd+greens should NOT get credit
  seats <- make_seats(parties, matrix(c(55, 30, 15), nrow = 1))
  shares <- make_shares(parties, matrix(c(0.55, 0.30, 0.15), nrow = 1))

  res <- run("Test 4", seats, parties, shares)
  if (!is.null(res)) {
    cp <- res$coalProbs

    check("3-party coalition gets 0 (subset already wins)",
      all(cp$coal_prob[cp$coal_size == 3] == 0))

    # cdu+spd is the minimal winner (cdu alone doesn't have majority: 55/100 = 0.55 > 0.5 — actually it does!)
    # cdu has 55 seats out of 100, which IS a majority. So cdu alone wins.
    check("cdu alone wins (55 > 50)",
      abs(cp$coal_prob[cp$coalition == "cdu"] - 1) < 1e-9)

    check("cdu+spd gets 0 (subset cdu already wins)",
      cp$coal_prob[cp$coalition == "cdu|spd"] == 0)
  }
}

# ── Test 5: Minimal winning — two-party needed, not three ────────────────────
cat("\nTest 5: minimal winning — 2-party wins, 3-party gets no credit\n")
{
  parties <- c("cdu", "spd", "greens")
  # cdu=40, spd=35, greens=25 → cdu+spd=75 majority; neither alone has majority
  seats <- make_seats(parties, matrix(c(40, 35, 25), nrow = 1))
  shares <- make_shares(parties, matrix(c(0.40, 0.35, 0.25), nrow = 1))

  res <- run("Test 5", seats, parties, shares)
  if (!is.null(res)) {
    cp <- res$coalProbs

    check("no solo majority", all(cp$coal_prob[cp$coal_size == 1] == 0))
    check("3-party coalition gets 0", all(cp$coal_prob[cp$coal_size == 3] == 0))
    check("cdu|spd wins (prob = 1)",
      abs(cp$coal_prob[cp$coalition == "cdu|spd"] - 1) < 1e-9)
  }
}

cat("\nDone.\n")
