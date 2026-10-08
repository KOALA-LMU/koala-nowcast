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

test_that("read_wahlrecht_result() parses saved HTML and groups unknown parties as others", {
  labels <- c(cdu = "Union", spd = "SPD", greens = "Gr\u00fcne",
              others = "Sonstige", bsw = "BSW")
  got <- read_wahlrecht_result(
    fixture_path("election-results.html"),
    result_year = 2025,
    party_lookup = default_election_party_lookup(),
    party_labels = labels,
    total_tolerance = Inf
  )
  got$sum_seats <- sum(got$seats)

  expect_setequal(got$party, c("cdu", "spd", "greens", "others", "bsw"))
  expect_equal(got$percent[got$party == "cdu"], 28.5)
  expect_equal(got$seats[got$party == "cdu"], 208)
  expect_equal(got$percent[got$party == "others"], 7.4)
  expect_equal(got$seats[got$party == "others"], 0)
  expect_equal(got$percent[got$party == "bsw"], 0)
  expect_true(all(c("label", "party", "percent", "seats", "sum_seats") %in% names(got)))
  expect_true(all(got$sum_seats == 413))
})

test_that("select_reference_election() switches after the first post-election poll", {
  config <- list(election_results = list(
    list(date = "2021-06-06", year = 2021),
    list(date = "2026-09-06", year = 2026)
  ))

  expect_equal(select_reference_election(config, "2026-09-03")$year, 2021)
  expect_equal(select_reference_election(config, "2026-09-07")$year, 2026)
})

test_that("2026 state-election configs switch only for post-election polls", {
  cases <- list(
    ltw_st = list(before = "2026-09-03", after = "2026-09-07", old = 2021),
    ltw_be = list(before = "2026-09-19", after = "2026-09-21", old = 2023),
    ltw_mv = list(before = "2026-09-19", after = "2026-09-21", old = 2021)
  )

  for (id in names(cases)) {
    config <- yaml::yaml.load_file(file.path("..", "..", "config", "elections",
                                             paste0(id, ".yml")))
    expect_equal(select_reference_election(config, cases[[id]]$before)$year,
                 cases[[id]]$old, info = id)
    expect_equal(select_reference_election(config, cases[[id]]$after)$year,
                 2026, info = id)
  }
})
