testthat::test_that("blog snapshots treat an unreported party as zero", {
  polls_file <- tempfile(fileext = ".json")
  on.exit(unlink(polls_file), add = TRUE)

  polls <- data.frame(
    pollster = c("institute", "institute", "pooled", "pooled", "pooled"),
    date = rep("2026-09-18", 5),
    start = rep("2026-09-15", 5),
    end = rep("2026-09-17", 5),
    respondents = rep(1000, 5),
    party = c("a", "b", "a", "b", "other"),
    percent = c(40, 60, 40, 60, 0),
    votes = c(400, 600, 400, 600, 0),
    election = rep("test", 5)
  )
  jsonlite::write_json(polls, polls_file, auto_unbox = TRUE)

  snapshots <- blog_select_poll_snapshots(
    polls_file = polls_file,
    election_date = "2026-09-20",
    election_config = list(
      pooling = list(period_extended = 100),
      pollsters = list("institute")
    ),
    party_order = c("a", "b", "other"),
    expected_pooled_date = "2026-09-18",
    expected_poll_count = 1
  )

  completed <- snapshots$latest_polls |>
    dplyr::filter(.data$party == "other")

  testthat::expect_equal(nrow(snapshots$latest_polls), 3)
  testthat::expect_equal(completed$percent, 0)
  testthat::expect_equal(completed$votes, 0)
  testthat::expect_equal(sum(snapshots$latest_polls$percent), 100)
})
