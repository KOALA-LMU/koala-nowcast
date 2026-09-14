# Regression tests for the pending safety net (#96).
#
# result_pairs() scans a result file for (pollster, date) pairs without parsing
# it. It has to cope with BOTH writers used by calc_coalProbs():
#   write_compact() -> jsonlite::write_json(..., digits = 4)   (no whitespace)
#   write_result()  -> jsonlite::write_json(..., pretty = TRUE) (spaces + newlines)
# It once matched only the compact form, so coalProbs_grouping.json — the file
# the dashboard renders — was silently invisible to the safety net.

test_that("result_pairs() reads compact (write_compact) output", {
  x <- data.frame(pollster = c("forsa", "insa"),
                  date     = c("2026-08-01", "2026-08-02"),
                  prob     = c(42, 43))
  p <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(x, p, auto_unbox = TRUE, digits = 4)

  got <- result_pairs(p)
  expect_equal(nrow(got), 2)
  expect_equal(got$pollster, c("forsa", "insa"))
  expect_equal(got$date, as.Date(c("2026-08-01", "2026-08-02")))
})

test_that("result_pairs() reads pretty-printed (write_result) output", {
  # The actual #96 regression: pretty = TRUE inserts spaces after the colons.
  x <- data.frame(pollster = c("forsa", "insa"),
                  date     = c("2026-08-01", "2026-08-02"),
                  prob     = c(42, 43))
  p <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(x, p, auto_unbox = TRUE, pretty = TRUE)

  got <- result_pairs(p)
  expect_equal(nrow(got), 2)
  expect_equal(got$pollster, c("forsa", "insa"))
  expect_equal(got$date, as.Date(c("2026-08-01", "2026-08-02")))
})

test_that("both writers yield identical pairs for identical data", {
  x <- data.frame(pollster = "gms", date = "2026-07-15", prob = 1)
  a <- withr::local_tempfile(fileext = ".json")
  b <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(x, a, auto_unbox = TRUE, digits = 4)
  jsonlite::write_json(x, b, auto_unbox = TRUE, pretty = TRUE)

  expect_equal(result_pairs(a), result_pairs(b))
})

test_that("all four result files are declared for checking", {
  # missing_dates() only inspects what RESULT_FILES lists; dropping one here
  # would silently stop guarding it.
  expect_setequal(
    RESULT_FILES,
    c("coalProbs_grouping", "biggestParty", "passHurdle", "shares")
  )
})

# Migration safety net (#145): a shares.json still in the per-simulation layout
# is unreadable for coalition_density() and gets dropped by calc_coalProbs(), but
# it carries the same (pollster, date) pairs as a current one — so without a
# layout check it looks up to date and is never recomputed.
test_that("missing_dates() reports every date when shares.json predates the quantile layout", {
  tmp <- withr::local_tempdir()
  old <- setwd(tmp)
  withr::defer(setwd(old))

  dir.create("data/surveys/test-election", recursive = TRUE)
  dir.create("data/results/test-election", recursive = TRUE)

  polls <- data.frame(pollster = c("forsa", "insa"),
                      date     = c("2026-08-01", "2026-08-02"))
  jsonlite::write_json(polls, "data/surveys/test-election/polls.json", auto_unbox = TRUE)

  current <- data.frame(pollster = c("forsa", "insa"),
                        date     = c("2026-08-01", "2026-08-02"),
                        prob     = c(42, 43))
  for (f in c("coalProbs_grouping", "biggestParty", "passHurdle")) {
    jsonlite::write_json(current, file.path("data/results/test-election", paste0(f, ".json")),
                         auto_unbox = TRUE, pretty = TRUE)
  }

  shares_old <- data.frame(pollster    = c("forsa", "insa"),
                           date        = c("2026-08-01", "2026-08-02"),
                           coalition   = c("spd", "spd"),
                           coal_share1 = c(0.2, 0.3),
                           coal_share2 = c(0.25, 0.35))
  jsonlite::write_json(shares_old, "data/results/test-election/shares.json",
                       auto_unbox = TRUE, digits = 6)

  expect_equal(missing_dates("test-election"), as.Date(c("2026-08-01", "2026-08-02")))

  # The quantile layout is current, so nothing is pending any more.
  shares_new <- data.frame(pollster              = c("forsa", "insa"),
                           date                  = c("2026-08-01", "2026-08-02"),
                           coalition             = c("spd", "spd"),
                           q1                    = c(0.2, 0.3),
                           q2                    = c(0.25, 0.35),
                           bw                    = c(0.01, 0.01),
                           parliament_presence_n = c(10, 10),
                           simulation_n          = c(10, 10))
  jsonlite::write_json(shares_new, "data/results/test-election/shares.json",
                       auto_unbox = TRUE, digits = 6)

  expect_length(missing_dates("test-election"), 0)
})
