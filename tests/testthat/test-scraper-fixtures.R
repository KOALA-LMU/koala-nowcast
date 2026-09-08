# Fixture-backed parser tests for wahlrecht.de pages. These intentionally use
# local HTML so CI catches parser regressions without relying on the live site.

fixture_path <- function(...) {
  normalizePath(file.path("..", "fixtures", ...), mustWork = TRUE)
}

test_that("scrape_wahlrecht() parses the saved Politbarometer HTML and folds FW into others", {
  # The local fork this used to exercise is gone: coalitions 0.6.28 reads the page
  # again (KOALA-LMU/coalitions#146), so the upstream scraper is what to test.
  got <- scrape_wahlrecht(fixture_path("wahlrecht", "politbarometer.html"))

  expect_equal(nrow(got), 2)
  expect_named(got, c("date", "start", "end", "cdu", "spd", "greens", "fdp",
                      "left", "afd", "bsw", "others", "respondents"))
  expect_equal(got$date, as.Date(c("2026-08-01", "2026-08-08")))
  expect_equal(got$start, as.Date(c("2026-07-25", "2026-08-01")))
  expect_equal(got$end, as.Date(c("2026-07-31", "2026-08-07")))
  expect_equal(got$respondents, c(1250, 1300))
  expect_equal(got$others, c(6, 5))
  expect_true(all(rowSums(got[c("cdu", "spd", "greens", "fdp", "left",
                                "afd", "bsw", "others")]) == 100))
})

test_that("scrape_election_results() parses saved HTML and groups unknown parties as others", {
  got <- scrape_election_results(fixture_path("wahlrecht", "election-results.html"),
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
