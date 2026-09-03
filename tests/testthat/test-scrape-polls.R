# Regression tests for the poll-cleaning steps in scrape_polls.R.

# ── drop_incomplete_polls (#93) ──────────────────────────────────────────────
# A poll missing a required party must be dropped on its own. It once took down
# every poll sharing its date, discarding valid polls from other institutes.

test_that("an incomplete poll does not drop a complete poll from the same date", {
  fresh <- rbind(
    make_long_poll("forsa", "2026-08-01", c("cdu", "spd", "greens"), c(30, 20, 15)),
    make_long_poll("insa",  "2026-08-01", c("cdu", "spd"),           c(31, 19))
  )
  kept <- drop_incomplete_polls(fresh, c("cdu", "spd", "greens"), "test")

  expect_setequal(unique(kept$pollster), "forsa")
  expect_equal(nrow(kept), 3)
})

test_that("complete polls on a date are all kept", {
  fresh <- rbind(
    make_long_poll("forsa", "2026-08-01", c("cdu", "spd"), c(30, 20)),
    make_long_poll("insa",  "2026-08-01", c("cdu", "spd"), c(31, 19))
  )
  kept <- drop_incomplete_polls(fresh, c("cdu", "spd"), "test")

  expect_setequal(unique(kept$pollster), c("forsa", "insa"))
  expect_equal(nrow(kept), 4)
})

test_that("a poll missing a required party is dropped entirely", {
  fresh <- make_long_poll("insa", "2026-08-01", c("cdu", "spd"), c(60, 40))
  kept  <- drop_incomplete_polls(fresh, c("cdu", "spd", "greens"), "test")

  expect_equal(nrow(kept), 0)
})

# ── fold_unmodelled_parties (#94) ────────────────────────────────────────────
# collapse_parties() keeps only the configured party columns and drops the rest
# without adding them to Sonstige, so an unmodelled party (e.g. Freie Wähler in
# the state configs) silently vanished and the poll no longer summed to 100.

test_that("a scraped party the config does not model is folded into others", {
  wide <- make_wide_poll(cdu = 30, spd = 18, greens = 8, afd = 20,
                         left = 6, fw = 9, others = 9)   # sums to 100
  cfg_parties <- c("cdu", "spd", "greens", "afd", "left", "others")

  out <- suppressMessages(fold_unmodelled_parties(wide, cfg_parties, "test"))

  expect_equal(out$others, 18)              # 9 others + 9 fw
  expect_false("fw" %in% names(out))
  expect_equal(sum(out[cfg_parties]), 100)  # nothing lost
})

test_that("polls without unmodelled parties are left untouched", {
  wide <- make_wide_poll(cdu = 40, spd = 30, others = 30)
  cfg_parties <- c("cdu", "spd", "others")

  out <- suppressMessages(fold_unmodelled_parties(wide, cfg_parties, "test"))

  expect_equal(out$others, 30)
  expect_equal(sum(out[cfg_parties]), 100)
})

test_that("folding warns rather than silently losing a share when others is absent", {
  wide <- make_wide_poll(cdu = 55, spd = 36, fw = 9)
  expect_warning(fold_unmodelled_parties(wide, c("cdu", "spd"), "test"),
                 "others")
})

# ── warn_off_total ───────────────────────────────────────────────────────────
# Backstop for a table shape neither the fold nor the config anticipated.

test_that("a poll that does not sum to 100 raises a warning", {
  fresh <- make_long_poll("insa", "2026-08-01", c("cdu", "spd"), c(30, 20))
  expect_warning(warn_off_total(fresh, "test"), "do not sum to 100")
})

test_that("a poll summing to 100 passes quietly and is returned unchanged", {
  fresh <- make_long_poll("insa", "2026-08-01", c("cdu", "spd"), c(60, 40))
  expect_silent(out <- warn_off_total(fresh, "test"))
  expect_equal(out, fresh)
})
