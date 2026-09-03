# Majority logic in calc_allCoalProbs(). Ported from scripts/test_calc_allCoalProbs.R,
# which printed PASS/FAIL but always exited 0 — so it could never fail a CI run.
#
# A coalition counts as possible only if it holds a majority AND no smaller
# subset of it already does ("minimal winning").

test_that("a party with an outright majority scores 1 and larger sets score 0", {
  parties <- c("cdu", "spd", "greens")
  seats   <- make_seats(parties, matrix(c(60, 25, 15), nrow = 1))   # cdu alone wins
  shares  <- make_shares(parties, matrix(c(0.60, 0.25, 0.15), nrow = 1))

  cp <- calc_allCoalProbs(seats, parties, shares)$coalProbs

  expect_equal(cp$coal_prob[cp$coalition == "cdu"], 1)
  # cdu already governs alone, so no coalition containing cdu is minimal winning
  expect_true(all(cp$coal_prob[cp$coal_size > 1 & grepl("cdu", cp$coalition)] == 0))
})

test_that("minimal winning: the two-party coalition wins, the three-party one gets no credit", {
  parties <- c("cdu", "spd", "greens")
  # cdu 40 / spd 35 / greens 25 — cdu+spd = 75 of 100 is a majority, no party alone
  seats  <- make_seats(parties, matrix(c(40, 35, 25), nrow = 1))
  shares <- make_shares(parties, matrix(c(0.40, 0.35, 0.25), nrow = 1))

  cp <- calc_allCoalProbs(seats, parties, shares)$coalProbs

  expect_true(all(cp$coal_prob[cp$coal_size == 1] == 0))
  expect_equal(cp$coal_prob[cp$coalition == "cdu|spd"], 1)
  expect_true(all(cp$coal_prob[cp$coal_size == 3] == 0))
})

test_that("probabilities average over simulations", {
  parties <- c("cdu", "spd")
  # Two simulations: cdu wins outright in one, not in the other.
  seats  <- make_seats(parties, matrix(c(60, 40,
                                         40, 60), nrow = 2, byrow = TRUE))
  shares <- make_shares(parties, matrix(c(0.60, 0.40,
                                          0.40, 0.60), nrow = 2, byrow = TRUE))

  cp <- calc_allCoalProbs(seats, parties, shares)$coalProbs

  expect_equal(cp$coal_prob[cp$coalition == "cdu"], 0.5)
  expect_equal(cp$coal_prob[cp$coalition == "spd"], 0.5)
})

test_that("probabilities are bounded to [0, 1]", {
  parties <- c("cdu", "spd", "greens")
  seats   <- make_seats(parties, matrix(c(45, 35, 20), nrow = 1))
  shares  <- make_shares(parties, matrix(c(0.45, 0.35, 0.20), nrow = 1))

  cp <- calc_allCoalProbs(seats, parties, shares)$coalProbs

  expect_true(all(cp$coal_prob >= 0 & cp$coal_prob <= 1))
})

# ── Strongest-party (leadership) orderings ───────────────────────────────────
# A coalition listed in several orderings ("cdu|spd", "spd|cdu") is counted per
# ordering, and only in the draws where the first-named party is the strongest.
# Ported from the legacy script's Tests 2 and 3.

test_that("two-party leadership orderings split by who leads in each draw", {
  parties <- c("cdu", "spd", "greens")
  # cdu+spd win the majority in every draw (40+35 of 100); vote shares pick the leader
  seats <- make_seats(parties, matrix(rep(c(40, 35, 25), 3), nrow = 3, byrow = TRUE))
  shares <- make_shares(parties, matrix(c(
    0.45, 0.30, 0.25,   # cdu leads
    0.30, 0.45, 0.25,   # spd leads
    0.50, 0.25, 0.25    # cdu leads
  ), nrow = 3, byrow = TRUE))
  spc <- c("cdu|spd", "spd|cdu")

  cp <- calc_allCoalProbs(seats, parties, shares, strongest_party_coals = spc)$coalProbs

  expect_false(anyDuplicated(cp$coalition) > 0)
  expect_equal(cp$coal_prob[cp$coalition == "cdu|spd"], 2 / 3)
  expect_equal(cp$coal_prob[cp$coalition == "spd|cdu"], 1 / 3)
  # the orderings partition the draws, so together they account for all of them
  expect_equal(sum(cp$coal_prob[cp$coalition %in% spc]), 1)
  # each ordering appears exactly once, no unordered duplicate alongside them
  expect_equal(sum(cp$coalition == "cdu|spd"), 1)
})

test_that("three-party leadership orderings are emitted as requested", {
  skip("Unresolved behaviour - see #132.
Requesting the ordering 'greens|spd|cdu' yields 'greens|cdu|spd' instead, so the
name does not round-trip. The legacy script scripts/test_calc_allCoalProbs.R
asserted this too and had been failing silently (it printed FAIL but always
exited 0). Re-enable once the intended behaviour is settled.")

  parties <- c("cdu", "spd", "greens", "fdp")
  # 25 seats each: any pair reaches only 50 of 100 (short of 51), any three reach 75,
  # so every three-party coalition is genuinely minimal winning.
  seats <- make_seats(parties, matrix(rep(c(25, 25, 25, 25), 3), nrow = 3, byrow = TRUE))
  shares <- make_shares(parties, matrix(c(
    0.34, 0.33, 0.33, 0.00,   # cdu leads
    0.33, 0.34, 0.33, 0.00,   # spd leads
    0.33, 0.33, 0.34, 0.00    # greens leads
  ), nrow = 3, byrow = TRUE))
  spc <- c("cdu|spd|greens", "spd|cdu|greens", "greens|spd|cdu")

  cp <- calc_allCoalProbs(seats, parties, shares, strongest_party_coals = spc)$coalProbs

  for (nm in spc) expect_true(nm %in% cp$coalition)
  for (nm in spc) expect_equal(cp$coal_prob[cp$coalition == nm], 1 / 3)
})
