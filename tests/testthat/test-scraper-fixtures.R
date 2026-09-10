# Fixture-backed parser tests for wahlrecht.de pages. These intentionally use
# local HTML so CI catches parser regressions without relying on the live site.

# testthat::test_path() resolves against tests/testthat/ whatever the working
# directory is, so these run the same under test_dir(), devtools::test() and a
# single test_file() from the project root. A hand-built "../fixtures" path only
# worked from inside tests/testthat/.
fixture_path <- function(...) testthat::test_path("fixtures", ...)

test_that("the installed coalitions still returns what the pipeline expects", {
  # A contract test, not a copy of the upstream parser tests: those live in
  # KOALA-LMU/coalitions. DESCRIPTION currently pulls coalitions from its GitHub
  # master via Remotes:, a moving target, so this pins the few properties
  # scrape_btw() -> scrape_election() depend on. Trim or drop it once the
  # dependency is a fixed CRAN release.
  got <- scrape_wahlrecht(fixture_path("politbarometer.html"))

  # the columns scrape_election() feeds to collapse_parties(), plus the shape it
  # assumes: one wide row per poll
  expect_true(all(c("date", "start", "end", "respondents",
                    "cdu", "spd", "greens", "fdp", "left", "afd", "others")
                  %in% colnames(got)))
  expect_equal(nrow(got), 2)

  # unmodelled parties are folded into others rather than dropped, so the shares
  # still add up -- scrape_election() relies on this via warn_off_total()
  expect_false("fw" %in% colnames(got))
  expect_true(all(rowSums(got[c("cdu", "spd", "greens", "fdp", "left", "afd",
                                "bsw", "others")]) == 100))
})

test_that("scrape_election_results() parses saved HTML and groups unknown parties as others", {
  got <- scrape_election_results(fixture_path("election-results.html"),
                                 election_year = 2025)

  expect_setequal(got$party, c("cdu", "spd", "greens", "others", "bsw"))
  expect_equal(got$percent[got$party == "cdu"], 28.5)
  expect_equal(got$seats[got$party == "cdu"], 208)
  expect_equal(got$percent[got$party == "others"], 7.4)
  expect_equal(got$seats[got$party == "others"], 0)
  expect_equal(got$percent[got$party == "bsw"], 0)
  expect_true(all(c("label", "party", "percent", "seats", "sum_seats") %in% names(got)))
  expect_true(all(got$sum_seats == 413))
})
