# ISO-8601 in UTC, to the second — the format computed_at is written in.
stamp_re <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"

testthat::test_that("calc_coalProbs() writes all expected JSON outputs", {
  config_path <- local_calc_project(make_polls_json("2026-08-01"))

  calc_coalProbs(config_path, nsim = 100, cores = 1)

  result_dir <- "data/results/test-election"

  expected_files <- c(
    "coalProbs_grouping.json",
    "biggestParty.json",
    "passHurdle.json",
    "shares.json"
  )

  testthat::expect_true(
    all(file.exists(file.path(result_dir, expected_files)))
  )

  # coalProbs.json is no longer written (#129): the dashboard reads
  # coalProbs_grouping.json instead.
  testthat::expect_false(file.exists(file.path(result_dir, "coalProbs.json")))

  ## Test the json files
  # coalProbs_grouping
  grouping <- jsonlite::fromJSON(file.path(result_dir, expected_files[[1]]))
  testthat::expect_true(
    all(c("pollster", "date", "coal_type", "prob") %in% colnames(grouping))
  )
  character_cols <- c("pollster", "coal_type")
  for (col in character_cols) {
    testthat::expect_type(grouping[[col]], "character")
  }
  testthat::expect_true(is.numeric(grouping$prob))
  testthat::expect_true(
    all(grouping$prob >= 0 & grouping$prob <= 100)
  )

  # biggestParty
  biggest <- jsonlite::fromJSON(file.path(result_dir, expected_files[[2]]))
  testthat::expect_true(
    all(c("pollster", "date", "index", "party", "prob") %in% colnames(biggest))
  )
  character_cols <- c("pollster", "index", "party")
  for (col in character_cols) {
    testthat::expect_type(biggest[[col]], "character")
  }
  testthat::expect_true(is.numeric(biggest$prob))
  testthat::expect_true(
    all(biggest$prob >= 0 & biggest$prob <= 100)
  )

  # passHurdle
  hurdle <- jsonlite::fromJSON(file.path(result_dir, expected_files[[3]]))
  testthat::expect_true(
    all(c("pollster", "date", "party", "prob") %in% colnames(hurdle))
  )
  testthat::expect_type(hurdle$party, "character")
  testthat::expect_true(is.numeric(hurdle$prob))
  testthat::expect_true(all(hurdle$prob >= 0 & hurdle$prob <= 100))

  # shares
  shares <- jsonlite::fromJSON(file.path(result_dir, expected_files[[4]]))
  testthat::expect_true(
    all(c("pollster", "date", "coalition") %in% colnames(shares))
  )
  testthat::expect_true(
    all(c("parliament_presence_n", "simulation_n", "bw") %in% colnames(shares))
  )
  # The per-simulation columns are gone (#145); a quantile grid stands in.
  testthat::expect_false(any(grepl("^coal_share", colnames(shares))))
  character_cols <- c("pollster", "coalition")
  for (col in character_cols) {
    testthat::expect_type(shares[[col]], "character")
  }
  q_cols <- colnames(shares)[grep("^q[0-9]+$", colnames(shares))]
  testthat::expect_gt(length(q_cols), 1)
  for (col in q_cols) {
    testthat::expect_type(shares[[col]], "double")
    testthat::expect_true(all(shares[[col]] >= 0 & shares[[col]] <= 1))
  }
  # Quantiles, so non-decreasing along the grid.
  testthat::expect_true(all(apply(as.matrix(shares[, q_cols]), 1, function(x) !is.unsorted(x))))
  testthat::expect_true(
    all(shares$parliament_presence_n >= 0 & shares$parliament_presence_n <= shares$simulation_n)
  )

  # Every result carries the time of its computation, in one and the same format,
  # and a single run stamps all of its rows alike.
  for (res in list(grouping, biggest, hurdle, shares)) {
    testthat::expect_true("computed_at" %in% colnames(res))
    testthat::expect_type(res$computed_at, "character")
    testthat::expect_true(all(grepl(stamp_re, res$computed_at)))
    testthat::expect_length(unique(res$computed_at), 1)
  }
  testthat::expect_length(
    unique(c(grouping$computed_at, biggest$computed_at,
             hurdle$computed_at, shares$computed_at)),
    1
  )
})

testthat::test_that("a second run keeps the earlier dates and their computed_at", {
  config_path <- local_calc_project(make_polls_json("2026-08-01"))

  calc_coalProbs(config_path, nsim = 100, cores = 1)

  result_dir <- "data/results/test-election"
  read_res   <- function(name) jsonlite::fromJSON(file.path(result_dir, paste0(name, ".json")))
  names_all  <- c("shares", "coalProbs_grouping", "biggestParty", "passHurdle")

  first <- lapply(setNames(names_all, names_all), read_res)
  stamp_first <- unique(first$shares$computed_at)
  testthat::expect_length(stamp_first, 1)

  # computed_at has second resolution, so the second run needs a later second to
  # be distinguishable from the first at all.
  Sys.sleep(1.1)

  # A new poll date arrives; the old one stays in polls.json.
  write_polls(make_polls_json(c("2026-08-01", "2026-09-01")))
  calc_coalProbs(config_path, nsim = 100, cores = 1)

  second <- lapply(setNames(names_all, names_all), read_res)

  for (name in names_all) {
    res <- second[[name]]
    # Both dates are in the file. shares.json used to be cut to the latest date
    # per pollster, which left the distributions without any history.
    testthat::expect_setequal(unique(res$date), c("2026-08-01", "2026-09-01"))
    # Only the new date was recomputed; the carried-over rows keep the stamp they
    # were written with, which is the whole point of storing it per row.
    testthat::expect_equal(unique(res$computed_at[res$date == "2026-08-01"]), stamp_first)
    stamp_second <- unique(res$computed_at[res$date == "2026-09-01"])
    testthat::expect_length(stamp_second, 1)
    testthat::expect_true(stamp_second > stamp_first)
    # The older rows survive unchanged, column for column.
    testthat::expect_equal(
      res[res$date == "2026-08-01", ] %>% dplyr::arrange(pollster),
      first[[name]] %>% dplyr::arrange(pollster),
      ignore_attr = TRUE
    )
  }

  # Both pollsters are present on both dates, not just the one that was new.
  testthat::expect_setequal(unique(second$shares$pollster), c("insa", "pooled"))
})

testthat::test_that("force_newCalculation restamps every row it recomputes", {
  # The path the one-off backfill takes: nothing is carried over, so every row
  # gets the stamp of that run, and the history is rewritten in full.
  config_path <- local_calc_project(make_polls_json(c("2026-08-01", "2026-09-01")))

  calc_coalProbs(config_path, nsim = 100, cores = 1)

  result_dir <- "data/results/test-election"
  read_res   <- function(name) jsonlite::fromJSON(file.path(result_dir, paste0(name, ".json")))

  before <- read_res("shares")
  stamp_before <- unique(before$computed_at)
  testthat::expect_length(stamp_before, 1)

  Sys.sleep(1.1)
  calc_coalProbs(config_path, nsim = 100, cores = 1, force_newCalculation = TRUE)

  for (name in c("shares", "coalProbs_grouping", "biggestParty", "passHurdle")) {
    res <- read_res(name)
    testthat::expect_setequal(unique(res$date), c("2026-08-01", "2026-09-01"))
    stamp_after <- unique(res$computed_at)
    testthat::expect_length(stamp_after, 1)
    testthat::expect_true(stamp_after > stamp_before)
  }

  # Same rows as before, only recomputed: the draws are random, so the numbers
  # move, but the shape of the file does not.
  after <- read_res("shares")
  testthat::expect_equal(dim(after), dim(before))
  testthat::expect_equal(colnames(after), colnames(before))
})
