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

test_that("all five result files are declared for checking", {
  # missing_dates() only inspects what RESULT_FILES lists; dropping one here
  # would silently stop guarding it.
  expect_setequal(
    RESULT_FILES,
    c("coalProbs", "coalProbs_grouping", "biggestParty", "passHurdle", "shares")
  )
})
