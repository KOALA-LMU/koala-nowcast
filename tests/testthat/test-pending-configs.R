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

test_that("result_pairs() still reads the pairs with computed_at in the record", {
  # The scanner relies on "pollster" and "date" being ADJACENT fields, so where
  # computed_at sits in the record decides whether the safety net sees the file
  # at all. calc_coalProbs() writes it third, after date, for exactly this
  # reason; putting it between the two would silently blind the net again (#96).
  x <- data.frame(pollster    = c("forsa", "insa"),
                  date        = c("2026-08-01", "2026-08-02"),
                  computed_at = "2026-08-02T06:00:00Z",
                  prob        = c(42, 43))
  for (writer in list(function(p) jsonlite::write_json(x, p, auto_unbox = TRUE, digits = 4),
                      function(p) jsonlite::write_json(x, p, auto_unbox = TRUE, pretty = TRUE))) {
    p <- withr::local_tempfile(fileext = ".json")
    writer(p)
    got <- result_pairs(p)
    expect_equal(nrow(got), 2)
    expect_equal(got$pollster, c("forsa", "insa"))
    expect_equal(got$date, as.Date(c("2026-08-01", "2026-08-02")))
  }
})

test_that("missing_dates() flags the newest dates of a pre-#145 shares.json", {
  # The bucket still holds shares.json with one column per draw. Its pairs look
  # current, so it would never be rewritten, and coalition_density() cannot read
  # it. The dates it covered -- the newest per pollster -- must come back as
  # missing; the other result files, which are complete, add nothing.
  withr::local_dir(withr::local_tempdir())
  dir.create(file.path("data", "surveys", "x"), recursive = TRUE)
  dir.create(file.path("data", "results", "x"), recursive = TRUE)
  polls <- data.frame(pollster = c("forsa", "forsa", "insa"),
                      date     = c("2026-08-01", "2026-08-08", "2026-08-05"))
  jsonlite::write_json(polls, file.path("data", "surveys", "x", "polls.json"))
  for (f in setdiff(RESULT_FILES, "shares"))
    jsonlite::write_json(cbind(polls, prob = 1), file.path("data", "results", "x", paste0(f, ".json")))
  newest <- polls[-1, ]
  shares_file <- file.path("data", "results", "x", "shares.json")

  jsonlite::write_json(cbind(newest, coalition = "cdu", coal_share1 = 0.3, coal_share2 = 0.31),
                       shares_file, digits = 4)
  expect_false(has_quantile_layout(shares_file))
  expect_equal(missing_dates("x"), as.Date(c("2026-08-05", "2026-08-08")))

  jsonlite::write_json(cbind(newest, computed_at = "2026-08-08T06:00:00Z", coalition = "cdu",
                             parliament_presence_n = 2, simulation_n = 2, bw = 0.01, q001 = 0.3),
                       shares_file, digits = 6)
  expect_true(has_quantile_layout(shares_file))
  expect_length(missing_dates("x"), 0)
})
