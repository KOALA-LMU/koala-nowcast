### Tests for derive_dynamic_coalitions
testthat::test_that("derive_dynamic_coalitions excludes others", { 
  parties_cfg <- list(
  list(id = "cdu",    label = "CDU",    color = "#111111"),
  list(id = "spd",    label = "SPD",    color = "#cc0000"),
  list(id = "greens", label = "Greens", color = "#00aa00"),
  list(id = "fdp",    label = "FDP",    color = "#ffcc00"),
  list(id = "others", label = "Others", color = "#999999")
  )
  pooled_shares <- c(
  cdu = 30,
  spd = 20,
  greens = 12,
  fdp = 4,
  others = 34
  )
  coals <- derive_dynamic_coalitions(parties_cfg, pooled_shares, max_size = 1)
  coal_parties <- unlist(lapply(coals, function(coal) coal$parties))
  testthat::expect_false("others" %in% coal_parties)
 })


testthat::test_that("derive_dynamic_coalitions drops coalitions below threshold", {
  parties_cfg <- list(
    list(id = "cdu",    label = "CDU",    color = "#111111"),
    list(id = "spd",    label = "SPD",    color = "#cc0000"),
    list(id = "greens", label = "Greens", color = "#00aa00"),
    list(id = "fdp",    label = "FDP",    color = "#ffcc00"),
    list(id = "others", label = "Others", color = "#999999")
  )

  pooled_shares <- c(
    cdu = 30,
    spd = 24,
    greens = 18,
    fdp = 6,
    others = 22
  )

  coals <- derive_dynamic_coalitions(
    parties_cfg = parties_cfg,
    pooled_shares = pooled_shares,
    max_size = 3,
    min_combined_pct = 25
  )

  coal_keys <- vapply(coals, function(coal) {
    paste(unlist(coal$parties), collapse = "|")
  }, character(1))

  # Expected TRUE
  testthat::expect_true("cdu" %in% coal_keys)
  testthat::expect_true("spd|greens" %in% coal_keys)

  # Expected FALSE
  testthat::expect_false("spd" %in% coal_keys)
  testthat::expect_false("greens|fdp" %in% coal_keys)
})


testthat::test_that("derive_dynamic_coalitions emits leader variants only above ratio", {
    parties_cfg <- list(
    list(id = "cdu", label = "CDU", color = "#111111"),
    list(id = "spd", label = "SPD", color = "#cc0000"),
    list(id = "fdp", label = "FDP", color = "#ffcc00"),
    list(id = "others", label = "Others", color = "#999999")
  )

  pooled_shares <- c(
    cdu = 30,
    spd = 20,
    fdp = 4,
    others = 46
  )

  coals <- derive_dynamic_coalitions(
    parties_cfg = parties_cfg,
    pooled_shares = pooled_shares,
    max_size = 2,
    min_combined_pct = 25,
    leader_flip_ratio = 0.5
  )

  coal_keys <- vapply(coals, function(coal) {
    paste(unlist(coal$parties), collapse = "|")
  }, character(1))

  testthat::expect_true("cdu|spd" %in% coal_keys)
  testthat::expect_true("spd|cdu" %in% coal_keys)

  testthat::expect_true("cdu|fdp" %in% coal_keys)
  testthat::expect_false("fdp|cdu" %in% coal_keys)
})


testthat::test_that("derive_dynamic_coalitions treats missing shares as zero", {
  testthat::skip(
    "Known bug: missing entries in pooled_shares currently error instead of
    being treated as zero. See Issue #138"
  )
  parties_cfg <- list(
    list(id = "cdu", label = "CDU", color = "#111111"),
    list(id = "spd", label = "SPD", color = "#cc0000"),
    list(id = "greens", label = "Greens", color = "#00aa00"),
    list(id = "others", label = "Others", color = "#999999")
  )

  pooled_shares <- c(
    cdu = 20,
    spd = 20,
    others = 60
  )

  coals <- derive_dynamic_coalitions(
    parties_cfg = parties_cfg,
    pooled_shares = pooled_shares,
    max_size = 2,
    min_combined_pct = 25,
    leader_flip_ratio = 0.5
  )

  coal_keys <- vapply(coals, function(coal) {
    paste(unlist(coal$parties), collapse = "|")
  }, character(1))

  testthat::expect_true("cdu|spd" %in% coal_keys)
  testthat::expect_true("spd|cdu" %in% coal_keys)

  testthat::expect_false("greens" %in% coal_keys)
  testthat::expect_false("cdu|greens" %in% coal_keys)
  testthat::expect_false("greens|cdu" %in% coal_keys)
  testthat::expect_false("spd|greens" %in% coal_keys)
  testthat::expect_false("greens|spd" %in% coal_keys)
})

### Tests for impute_polls_for_pooling()
testthat::test_that("impute_polls_for_pooling leaves complete polls unchanged", {
   cfg <- list(
    parties = list(
      list(id = "cdu"),
      list(id = "spd"),
      list(id = "greens"),
      list(id = "others")
    )
  )

  raw <- tibble::tibble(
    pollster = "insa",
    date = as.Date("2026-08-01"),
    party = c("cdu", "spd", "greens", "others"),
    percent = c(30, 20, 10, 40),
    votes = c(300, 200, 100, 400),
    election = "test-election"
  )

  out <- impute_polls_for_pooling(
    raw = raw,
    cfg = cfg
  )

  testthat::expect_equal(raw, out)
})

testthat::test_that("impute_polls_for_pooling adds missing party from previous poll", {
  cfg <- list(
    parties = list(
      list(id = "cdu"),
      list(id = "spd"),
      list(id = "greens"),
      list(id = "others")
    )
  )
  raw <- rbind(
    tibble::tibble(
      pollster = "insa",
      date = as.Date("2026-8-01"),
      party = c("cdu", "spd", "greens", "others"),
      percent = c(30, 20, 10, 40),
      votes = c(300, 200, 100, 400),
      election = "test-election"
    ),
    tibble::tibble(
      pollster = "insa",
      date = as.Date("2026-08-08"),
      party = c("cdu", "spd", "others"),
      percent = c(32, 21, 47),
      votes = c(320, 210, 470),
      election = "test-election"
    )
  )

  out <- impute_polls_for_pooling(raw, cfg)

  imputed <- out[out$pollster == "insa" &
    out$date == as.Date("2026-08-08") &
    out$party == "greens", ]
  others <- out[out$pollster == "insa" &
    out$date == as.Date("2026-08-08") &
    out$party == "others", ]

  testthat::expect_equal(nrow(imputed), 1)
  testthat::expect_equal(imputed$percent, 10)
  testthat::expect_equal(imputed$votes, 100)

  testthat::expect_equal(others$percent, 37)
  testthat::expect_equal(others$votes, 370)
})

testthat::test_that("impute_polls_for_pooling prefers same pollster over other pollsters", {
  cfg <- list(
    parties = list(
      list(id = "cdu"),
      list(id = "spd"),
      list(id = "greens"),
      list(id = "others")
    )
  )

  raw <- rbind(
    data.frame(
      pollster = "forsa",
      date = as.Date("2026-08-01"),
      party = c("cdu", "spd", "greens", "others"),
      percent = c(31, 19, 14, 36),
      votes = c(310, 190, 140, 360),
      election = "test-election"
    ),
    data.frame(
      pollster = "insa",
      date = as.Date("2026-08-02"),
      party = c("cdu", "spd", "greens", "others"),
      percent = c(30, 20, 10, 40),
      votes = c(300, 200, 100, 400),
      election = "test-election"
    ),
    data.frame(
      pollster = "insa",
      date = as.Date("2026-08-08"),
      party = c("cdu", "spd", "others"),
      percent = c(32, 21, 47),
      votes = c(320, 210, 470),
      election = "test-election"
    )
  )

  out <- impute_polls_for_pooling(raw, cfg)

  imputed <- out[out$pollster == "insa" &
                   out$date == as.Date("2026-08-08") &
                   out$party == "greens", ]

  testthat::expect_equal(imputed$percent, 10)
})

### Tests for compute_pooled
testthat::test_that("compute_pooled returns pooled rows for raw poll dates", {
  cfg <- list(
    id = "test-election",
    pollsters = list("insa", "forsa"),
    pooling = list(
      period = 14,
      period_extended = NULL
    ),
    parties = list(
      list(id = "cdu"),
      list(id = "spd"),
      list(id = "greens"),
      list(id = "others")
    )
  )

  raw <- rbind(
    tibble::tibble(
      pollster = "insa",
      date = as.Date("2026-08-01"),
      start = as.Date("2026-07-30"),
      end = as.Date("2026-08-01"),
      respondents = 1000,
      party = c("cdu", "spd", "greens", "others"),
      percent = c(30, 20, 10, 40),
      votes = c(300, 200, 100, 400),
      election = "test-election"
    ),
    tibble::tibble(
      pollster = "forsa",
      date = as.Date("2026-08-08"),
      start = as.Date("2026-08-06"),
      end = as.Date("2026-08-08"),
      respondents = 1000,
      party = c("cdu", "spd", "greens", "others"),
      percent = c(32, 21, 11, 36),
      votes = c(320, 210, 110, 360),
      election = "test-election"
    )
  )

  out <- compute_pooled(raw, cfg)

  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(out$pollster == "pooled"))
  testthat::expect_setequal(
    unique(out$date), as.Date(c("2026-08-01", "2026-08-08"))
  )
  testthat::expect_true(all(out$election == "test-election"))
  testthat::expect_true(is.numeric(out$percent))
})

testthat::test_that("compute_pooled respects from_date", {
  cfg <- list(
    id = "test-election",
    pollsters = list("insa", "forsa"),
    pooling = list(
      period = 14,
      period_extended = NULL
    ),
    parties = list(
      list(id = "cdu"),
      list(id = "spd"),
      list(id = "greens"),
      list(id = "others")
    )
  )

  raw <- rbind(
    data.frame(
      pollster = "insa",
      date = as.Date("2026-08-01"),
      start = as.Date("2026-07-30"),
      end = as.Date("2026-08-01"),
      respondents = 1000,
      party = c("cdu", "spd", "greens", "others"),
      percent = c(30, 20, 10, 40),
      votes = c(300, 200, 100, 400),
      election = "test-election"
    ),
    data.frame(
      pollster = "forsa",
      date = as.Date("2026-08-08"),
      start = as.Date("2026-08-06"),
      end = as.Date("2026-08-08"),
      respondents = 1000,
      party = c("cdu", "spd", "greens", "others"),
      percent = c(32, 21, 11, 36),
      votes = c(320, 210, 110, 360),
      election = "test-election"
    )
  )

  out <- compute_pooled(
    raw,
    cfg,
    from_date = as.Date("2026-08-08")
  )

  testthat::expect_setequal(unique(out$date), as.Date("2026-08-08"))
  testthat::expect_true(all(out$pollster == "pooled"))
})

testthat::test_that("compute_pooled works with a single pollster", {
  cfg <- list(
    id = "test-election",
    pollsters = list("insa"),
    pooling = list(
      period = 14,
      period_extended = NULL
    ),
    parties = list(
      list(id = "cdu"),
      list(id = "spd"),
      list(id = "greens"),
      list(id = "others")
    )
  )

  raw <- data.frame(
    pollster = "insa",
    date = as.Date(rep("2026-08-01", 4)),
    start = as.Date(rep("2026-07-30", 4)),
    end = as.Date(rep("2026-08-01", 4)),
    respondents = 1000,
    party = c("cdu", "spd", "greens", "others"),
    percent = c(30, 20, 10, 40),
    votes = c(300, 200, 100, 400),
    election = "test-election"
  )

  out <- compute_pooled(raw, cfg)

  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(out$pollster == "pooled"))
  testthat::expect_setequal(unique(out$date), as.Date("2026-08-01"))
  testthat::expect_true(all(out$election == "test-election"))
})
### Tests for summarise_shareDraws
testthat::test_that("summarise_shareDraws counts joint presence of all members", {
  shares <- data.frame(
    coalition = c("a", "b", "c", "a|b", "a|d"),
    s1 = c(0.5, 0.3, 0.2, 0.8, 0.5),
    s2 = c(0.5, 0.0, 0.5, 0.5, 0.5),
    s3 = c(0.5, 0.3, 0.2, 0.8, 0.5),
    s4 = c(0.5, 0.3, 0.2, 0.8, 0.5),
    stringsAsFactors = FALSE
  )
  party_shares <- shares[shares$coalition %in% c("a", "b", "c"),
                         colnames(shares) != "coalition"]
  rownames(party_shares) <- c("a", "b", "c")

  res <- summarise_shareDraws(shares, party_shares, probs = c(0.25, 0.75))

  testthat::expect_equal(res$simulation_n, rep(4L, 5))
  # b is out of parliament in s2, so a|b is fully represented in 3 of 4 draws.
  testthat::expect_equal(res$parliament_presence_n[res$coalition == "a|b"], 3L)
  testthat::expect_equal(res$parliament_presence_n[res$coalition == "b"], 3L)
  # d was not polled and so has no row: the coalition counts as never complete
  # rather than being scored from a alone.
  testthat::expect_equal(res$parliament_presence_n[res$coalition == "a|d"], 0L)
})

testthat::test_that("summarise_shareDraws stores the quantile grid and a bandwidth", {
  set.seed(1)
  draws  <- matrix(runif(2 * 500), nrow = 2)
  shares <- data.frame(coalition = c("a", "b"), draws, stringsAsFactors = FALSE)
  party_shares <- shares[, colnames(shares) != "coalition"]
  rownames(party_shares) <- c("a", "b")

  res <- summarise_shareDraws(shares, party_shares)

  q_cols <- grep("^q[0-9]+$", colnames(res), value = TRUE)
  testthat::expect_equal(length(q_cols), 100)
  testthat::expect_equal(
    as.numeric(res[1, q_cols]),
    unname(quantile(draws[1, ], probs = (seq_len(100) - 0.5) / 100))
  )
  # The 95% interval the dashboard shows is read off the grid, not interpolated.
  testthat::expect_equal(res[[1, "q003"]], unname(quantile(draws[1, ], 0.025)))
  testthat::expect_equal(res[[1, "q098"]], unname(quantile(draws[1, ], 0.975)))
  testthat::expect_true(all(res$bw > 0))
})

testthat::test_that("summarise_shareDraws marks a degenerate row with bw NA", {
  shares <- data.frame(coalition = "a", s1 = 0.4, s2 = 0.4, s3 = 0.4,
                       stringsAsFactors = FALSE)
  party_shares <- shares[, colnames(shares) != "coalition", drop = FALSE]
  rownames(party_shares) <- "a"

  res <- summarise_shareDraws(shares, party_shares, probs = c(0.25, 0.75))
  testthat::expect_true(is.na(res$bw))
})
