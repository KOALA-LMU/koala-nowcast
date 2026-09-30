# Fixture-backed parser tests for wahlrecht.de pages. These intentionally use
# local HTML so CI catches parser regressions without relying on the live site.

fixture_path <- function(...) {
  normalizePath(file.path("..", "fixtures", ...), mustWork = TRUE)
}

test_that("scrape_politbarometer() parses saved HTML and folds FW into others", {
  got <- scrape_politbarometer(fixture_path("wahlrecht", "politbarometer.html"))

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

test_that("read_wahlrecht_result() parses saved HTML and groups unknown parties as others", {
  labels <- c(cdu = "Union", spd = "SPD", greens = "Gr\u00fcne",
              others = "Sonstige", bsw = "BSW")
  got <- read_wahlrecht_result(
    fixture_path("wahlrecht", "election-results.html"),
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
