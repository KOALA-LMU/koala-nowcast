testthat::test_that("calc_coalProbs() writes all expected JSON outputs", {
  repo <- normalizePath(file.path("..", ".."))
  tmp <- withr::local_tempdir()
  old <- setwd(tmp)
  withr::defer(setwd(old))

  dir.create("scripts", recursive = TRUE)
  dir.create("config/elections", recursive = TRUE)
  dir.create("data/surveys/test-election", recursive = TRUE)

  testthat::expect_true(file.copy(
    file.path(repo, "scripts", "calc_coalProbs.R"),
    "scripts/calc_coalProbs.R"
  ))
  testthat::expect_true(file.copy(
    file.path(repo, "scripts", "calc_coalProbs_helpers.R"),
    "scripts/calc_coalProbs_helpers.R"
  ))

  source("scripts/calc_coalProbs_helpers.R")
  source("scripts/calc_coalProbs.R")

  config_path <- "config/elections/test.yml"

  writeLines(
    c(
       "id: test-election",
      "name: Test Election",
      "parliament:",
      "  seats: 20",
      "  hurdle: 5",
      "  seat_allocation: sls",
      "parties:",
      "  - id: cdu",
      "    label: CDU",
      "    color: '#111111'",
      "    required: true",
      "  - id: spd",
      "    label: SPD",
      "    color: '#cc0000'",
      "    required: true",
      "  - id: greens",
      "    label: Greens",
      "    color: '#00aa00'",
      "    required: true",
      "  - id: others",
      "    label: Others",
      "    color: '#999999'",
      "    required: false",
      "coalitions:",
      "  - parties: [cdu]",
      "    label: CDU",
      "    color: '#111111'",
      "  - parties: [spd]",
      "    label: SPD",
      "    color: '#cc0000'",
      "  - parties: [cdu, spd]",
      "    label: CDU-SPD",
      "    color: '#111111'",
      "analyses:",
      "  biggest_party:",
      "    - parties: [cdu, spd, greens]"
    ),
    config_path
  )

  polls <- data.frame(
    pollster = rep(c("insa", "pooled"), each = 4),
    date = as.Date(rep("2026-08-01", 8)),
    party = rep(c("cdu", "spd", "greens", "others"), times = 2),
    percent = c(34, 28, 18, 20, 34, 28, 18, 20),
    votes = c(340, 280, 180, 200, 340, 280, 180, 200),
    election = "test-election"
  )

  jsonlite::write_json(
    polls,
    "data/surveys/test-election/polls.json",
    auto_unbox = TRUE,
    pretty = TRUE
  )

  calc_coalProbs(config_path, nsim = 100, cores = 1)

  result_dir <- "data/results/test-election"

  expected_files <- c(
    "coalProbs.json",
    "coalProbs_grouping.json",
    "biggestParty.json",
    "passHurdle.json",
    "shares.json"
  )

  testthat::expect_true(
    all(file.exists(file.path(result_dir, expected_files)))
  )

  ## Test the json files
  # coalProbs
  coal_probs <- jsonlite::fromJSON(file.path(result_dir, expected_files[[1]]))
  testthat::expect_true(
    all(c("pollster", "date", "coalition", "size", "prob") %in% colnames(coal_probs))
  )
  character_cols <- c("pollster", "coalition")
  for (col in character_cols) {
    testthat::expect_type(coal_probs[[col]], "character")
  }
  testthat::expect_type(coal_probs$size, "integer")
  testthat::expect_true(is.numeric(coal_probs$prob))
  testthat::expect_true(
    all(coal_probs$prob >= 0 & coal_probs$prob <= 100)
  )

  # coalProbs_grouping
  grouping <- jsonlite::fromJSON(file.path(result_dir, expected_files[[2]]))
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
  biggest <- jsonlite::fromJSON(file.path(result_dir, expected_files[[3]]))
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
  hurdle <- jsonlite::fromJSON(file.path(result_dir, expected_files[[4]]))
  testthat::expect_true(
    all(c("pollster", "date", "party", "prob") %in% colnames(hurdle))
  )
  testthat::expect_type(hurdle$party, "character")
  testthat::expect_true(is.numeric(hurdle$prob))
  testthat::expect_true(all(hurdle$prob >= 0 & hurdle$prob <= 100))

  # shares
  shares <- jsonlite::fromJSON(file.path(result_dir, expected_files[[5]]))
  testthat::expect_true(
    all(c("pollster", "date", "coalition") %in% colnames(shares))
  )
  testthat::expect_true(any(grepl("^coal_share", colnames(shares))))
  character_cols <- c("pollster", "coalition")
  for (col in character_cols) {
    testthat::expect_type(shares[[col]], "character")
  }
  coal_share_cols <- colnames(shares)[grep("^coal_share", colnames(shares))]
  for (col in coal_share_cols) {
    testthat::expect_type(shares[[col]], "double")
  }
})
