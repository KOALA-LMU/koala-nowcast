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

# ── values_differ (#99) ──────────────────────────────────────────────────────
# Decides whether wahlrecht has revised a value we already stored. Keying on
# (pollster, date, party) alone made a correction look like a duplicate, so the
# stale value survived every later scrape.

test_that("a revised percentage counts as different", {
  expect_true(values_differ(31, 29))
  expect_true(values_differ(29.4, 29.5))
})

test_that("an unchanged value does not", {
  expect_false(values_differ(29, 29))
  expect_false(values_differ(0, 0))
})

test_that("differences below the JSON rounding are ignored", {
  # write_json keeps 4 decimals, so the stored side comes back slightly changed;
  # without a tolerance every run would report a revision, rewrite the file and
  # recompute pooled forever.
  expect_false(values_differ(29.00001, 29))
  # ... while the tolerance still sits well below what a real revision moves by
  expect_true(values_differ(29.1, 29))
})

test_that("NA is treated as a value, not as unknown", {
  # A party the pollster does not report. `!=` would answer NA here, which drops
  # the row from the filter instead of deciding.
  expect_true(values_differ(NA, 5))
  expect_true(values_differ(5, NA))
  expect_false(values_differ(NA, NA))
})

test_that("values_differ is vectorised over columns", {
  expect_equal(values_differ(c(29, 30, NA), c(29, 31, NA)),
               c(FALSE, TRUE, FALSE))
})

# ── upsert_polls (#99) ───────────────────────────────────────────────────────

test_that("a re-delivered key takes the freshly scraped value", {
  key      <- c("pollster", "date", "party")
  existing <- make_long_poll("forsa", "2026-08-01", c("cdu", "spd"), c(29, 20))
  fresh    <- make_long_poll("forsa", "2026-08-01", c("cdu", "spd"), c(31, 20))

  got <- upsert_polls(existing, fresh, key)

  expect_equal(nrow(got), 2)                                  # replaced, not appended
  expect_equal(got$percent[got$party == "cdu"], 31)           # the correction won
})

test_that("polls outside the re-scraped window are kept", {
  key      <- c("pollster", "date", "party")
  existing <- rbind(
    make_long_poll("forsa", "2026-01-15", c("cdu", "spd"), c(25, 22)),  # before the window
    make_long_poll("forsa", "2026-08-01", c("cdu", "spd"), c(29, 20))
  )
  fresh <- make_long_poll("forsa", "2026-08-01", c("cdu", "spd"), c(31, 20))

  got <- upsert_polls(existing, fresh, key)

  expect_equal(nrow(got), 4)
  expect_equal(got$percent[got$date == as.Date("2026-01-15") & got$party == "cdu"], 25)
})

test_that("a stored poll the scrape no longer lists is retained", {
  # Deliberate: a poll that disappears upstream stays in our history rather than
  # vanishing from the series. Pinned here so the choice is visible if it changes.
  key      <- c("pollster", "date", "party")
  existing <- rbind(
    make_long_poll("forsa", "2026-08-01", c("cdu", "spd"), c(29, 20)),
    make_long_poll("insa",  "2026-08-01", c("cdu", "spd"), c(30, 19))   # gone upstream
  )
  fresh <- make_long_poll("forsa", "2026-08-01", c("cdu", "spd"), c(29, 20))

  got <- upsert_polls(existing, fresh, key)

  expect_true("insa" %in% got$pollster)
  expect_equal(nrow(got), 4)
})

test_that("with no stored history the scrape is taken as-is", {
  key   <- c("pollster", "date", "party")
  fresh <- make_long_poll("forsa", "2026-08-01", c("cdu", "spd"), c(29, 20))

  expect_equal(nrow(upsert_polls(NULL, fresh, key)), 2)
})

test_that("results come back newest first", {
  key   <- c("pollster", "date", "party")
  fresh <- rbind(
    make_long_poll("forsa", "2026-07-01", "cdu", 28),
    make_long_poll("forsa", "2026-08-01", "cdu", 29)
  )

  got <- upsert_polls(NULL, fresh, key)

  expect_equal(got$date, sort(got$date, decreasing = TRUE))
})
