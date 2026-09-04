suppressPackageStartupMessages({
  library(coalitions)
  library(yaml)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})
source("scripts/scrape_btw.R")

scrape_election <- function(config_path, oldest_date = as.Date("2024-12-01")) {
  cfg <- read_yaml(config_path)
  message(sprintf("\n[%s] Scraping polls for %s...", cfg$id, cfg$name))

  # How far back a from-scratch scrape reaches. Per election, because the useful
  # start depends on the pooling window (cfg$pooling$period_extended): the first
  # window's worth of dates pool over a stretch that is only partly scraped, so
  # the run-up has to be long enough that the first date one actually wants to
  # show has a complete window behind it.
  if (!is.null(cfg$scraper$oldest_date))
    oldest_date <- as.Date(as.character(cfg$scraper$oldest_date))

  parties <- sapply(cfg$parties, `[[`, "id")
  parties_required <- sapply(cfg$parties, function(p) if (isTRUE(p$required)) p$id else NULL) |>
    Filter(Negate(is.null), x = _)

  # Define path and name of output file
  out_dir  <- file.path("data", "surveys", cfg$id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, "polls.json")

  # Load existing data if available
  if (file.exists(out_file) && file.size(out_file) > 0) {
    existing <- tryCatch(
      as_tibble(fromJSON(out_file)) %>%
        mutate(date = as.Date(date), start = as.Date(start), end = as.Date(end)),
      error = function(e) {
        message(sprintf("[%s] Could not parse existing JSON, starting fresh", cfg$id))
        NULL
      }
    )
    if (!is.null(existing)) {
      scrape_from <- max(existing$date) - 30
    } else {
      scrape_from <- oldest_date
    }
  } else {
    existing   <- NULL
    scrape_from <- oldest_date
  }

  message(sprintf("[%s] existing data up to: %s — scraping from: %s",
              cfg$id,
              if (!is.null(existing)) as.character(max(existing$date)) else "none",
              scrape_from))

  # assign scraper function to fn
  fn <- match.fun(cfg$scraper[["function"]])
  # define fn arguments and define them as needed
  fn_args <- list()
  if ("address" %in% names(formals(fn)) && !is.null(cfg$scraper$url))
    fn_args$address <- cfg$scraper$url
  if ("ind_row_remove" %in% names(formals(fn)) && !is.null(cfg$scraper$ind_row_remove))
    fn_args$ind_row_remove <- -cfg$scraper$ind_row_remove
  # call function and replace/edit field values based on conventions.
  # Date filter runs before folding/pivoting so fold_unmodelled_parties() only
  # reports polls that reach the output.
  fresh <- do.call(fn, fn_args) %>%
    mutate(
      pollster = replace(pollster, pollster == "infratestdimap", "infratest"),
      pollster = gsub("forschungsgruppewahlen", "fgw", pollster)
    ) %>%
    filter(date >= scrape_from) %>%
    fold_unmodelled_parties(parties, cfg$id) %>%
    collapse_parties(parties = parties) %>%
    unnest(survey) %>%
    mutate(election = cfg$id) %>%
    warn_off_total(cfg$id)

  fresh <- drop_incomplete_polls(fresh, parties_required, cfg$id)

  # New rows plus rows wahlrecht has revised. On the key alone a corrected
  # percentage looks like a duplicate, so the values decide as well.
  poll_key     <- c("pollster", "date", "party")
  existing_raw <- if (!is.null(existing)) filter(existing, pollster != "pooled") else NULL

  if (!is.null(existing_raw)) {
    new_rows     <- anti_join(fresh, existing_raw, by = poll_key)
    revised_rows <- fresh %>%
      inner_join(
        existing_raw %>%
          select(all_of(poll_key), stored_percent = percent,
                 stored_respondents = respondents),
        by = poll_key
      ) %>%
      filter(values_differ(percent, stored_percent) |
               values_differ(respondents, stored_respondents)) %>%
      select(-stored_percent, -stored_respondents)
  } else {
    new_rows     <- fresh
    revised_rows <- fresh[0, ]
  }
  changed_rows <- bind_rows(new_rows, revised_rows)

  has_new_raw    <- nrow(changed_rows) > 0
  has_no_pooled  <- is.null(existing) || !any(existing$pollster == "pooled")

  # changed_rows has one row per (pollster, date, party); count/report distinct
  # polls (pollster+date pairs) rather than raw row count.
  changed_polls      <- distinct(changed_rows, pollster, date)
  changed_poll_dates <- sort(unique(changed_polls$date))

  message(sprintf("[%s] new_raw=%s (%d poll(s)), no_pooled=%s",
              cfg$id, has_new_raw, nrow(changed_polls), has_no_pooled))

  if (!has_new_raw && !has_no_pooled) {
    message(sprintf("[%s] No new polls.", cfg$id))
    return(invisible(FALSE))
  }

  if (has_new_raw)
    message(sprintf("[%s] %d new/revised poll(s) found: %s", cfg$id, nrow(changed_polls),
                    paste(changed_poll_dates, collapse = ", ")))
  # Revisions change no key, so nothing else in the log would show them.
  if (nrow(revised_rows) > 0)
    message(sprintf("[%s] %d revised value(s) replacing stored ones: %s", cfg$id,
                    nrow(revised_rows),
                    paste(sprintf("%s (%s, %s)", revised_rows$pollster,
                                  revised_rows$date, revised_rows$party),
                          collapse = ", ")))
  if (has_no_pooled)
    message(sprintf("[%s] No pooled data found, computing pooled estimates", cfg$id))

  # Upsert, not append: the fresh row wins for every key it re-delivers, so a
  # correction replaces the stored value. Polls outside the window are kept.
  raw_updated <- bind_rows(
    if (!is.null(existing_raw)) anti_join(existing_raw, fresh, by = poll_key) else NULL,
    fresh
  ) %>% arrange(desc(date))

  # Only recompute pooled from the earliest new poll date forward; recompute all when
  # there is no existing pooled data yet.
  from_date <- if (has_new_raw && !has_no_pooled) min(unique(changed_rows$date)) else NULL
  pooled_updated <- compute_pooled(raw_updated, cfg, from_date = from_date)

  # Write affected dates so calc_coalProbs knows exactly which dates to recompute.
  pending_raw <- raw_updated %>% filter(pollster != "pooled")
  if (!is.null(from_date))
    pending_raw <- pending_raw %>% filter(date >= from_date)
  pending_dates <- as.character(sort(unique(pending_raw$date)))
  write_json(pending_dates, file.path(out_dir, "pending_dates.json"), auto_unbox = TRUE)

  # Drop old pooled rows for recomputed dates, replace with fresh ones
  recompute_dates <- unique(pooled_updated$date)
  existing_pooled <- if (!is.null(existing)) {
    existing %>% filter(pollster == "pooled", !date %in% recompute_dates)
  } else {
    NULL
  }

  updated <- bind_rows(raw_updated, existing_pooled, pooled_updated) %>%
    arrange(desc(date), pollster)

  write_json(updated, out_file, pretty = TRUE, auto_unbox = TRUE)
  message(sprintf("[%s] Saved to %s", cfg$id, out_file))

  invisible(TRUE)
}

# TRUE wherever a freshly scraped value disagrees with the stored one.
#
# Not `!=`: the stored side is rounded to 4 decimals by write_json, so exact
# comparison could flag a revision on every run; the tolerance sits above that
# rounding and well below the 0.1 a real revision moves by. NA is a value here
# (a party a pollster does not report), and `!=` returns NA rather than a verdict.
values_differ <- function(new, old, tol = 1e-3) {
  (is.na(new) != is.na(old)) |
    (!is.na(new) & !is.na(old) & abs(new - old) > tol)
}

# Fold scraped parties the config does not model into others (Sonstige).
#
# collapse_parties() pivots only the columns named in `parties`; every other
# party column is left behind and its share silently disappears, so the poll no
# longer sums to 100 and Sonstige is understated. wahlrecht breaks small parties
# out separately in the run-up to an election, so a column that is empty today
# can start carrying a share mid-campaign. The set is derived, not listed, so a
# party broken out later needs no code change.
fold_unmodelled_parties <- function(wide, parties, id = "") {
  meta       <- c("pollster", "date", "start", "end", "respondents")
  unmodelled <- setdiff(names(select(wide, where(is.numeric))), c(meta, parties, "others"))
  if (length(unmodelled) == 0) return(wide)

  if (!"others" %in% names(wide)) {
    warning(sprintf("[%s] No 'others' column to fold %s into — their share is lost",
                    id, paste(unmodelled, collapse = ", ")))
    return(select(wide, -all_of(unmodelled)))
  }

  extra <- rowSums(wide[unmodelled], na.rm = TRUE)
  if (any(extra > 0))
    message(sprintf("[%s] Folded %s into others for %d poll(s): %s", id,
                    paste(unmodelled, collapse = ", "), sum(extra > 0),
                    paste(sprintf("%s (%s, +%g)", wide$pollster, wide$date,
                                  extra)[extra > 0], collapse = ", ")))

  # if_else, not +: a poll reporting no Sonstige keeps its NA instead of a spurious 0%
  wide %>%
    mutate(others = if_else(extra > 0, coalesce(others, 0) + extra, others)) %>%
    select(-all_of(unmodelled))
}

# Warn about polls whose shares do not add up to ~100.
#
# Both scrapers only emit rows summing to exactly 100, so after folding every
# poll must still hit 100; a deviation means a table shape neither the fold nor
# the config anticipated. Warn rather than stop: the pipeline runs unattended
# over all elections, and one odd poll should not block the others' updates.
warn_off_total <- function(fresh, id = "", tol = 0.5) {
  off <- fresh %>%
    group_by(date, pollster) %>%
    summarise(total = sum(percent, na.rm = TRUE), .groups = "drop") %>%
    filter(abs(total - 100) > tol)

  if (nrow(off) > 0)
    warning(sprintf("[%s] %d poll(s) do not sum to 100: %s", id, nrow(off),
                    paste(sprintf("%s (%s): %g", off$pollster, off$date, off$total),
                          collapse = ", ")))
  invisible(fresh)
}

# Drop individual polls where any required party is missing.
#
# Scoped to the single poll (pollster + date), NOT the whole date. Two institutes
# often publish on the same day; if the check were reduced to the date, one
# incomplete poll would take down every other institute's complete poll that day,
# and those valid polls would then be missing from every pooling window covering
# it. A date disappears entirely only when all of its polls are incomplete.
drop_incomplete_polls <- function(fresh, parties_required, id = "") {
  poll_completeness <- fresh %>%
    group_by(date, pollster) %>%
    summarise(has_all = all(parties_required %in% party), .groups = "drop")

  dropped <- filter(poll_completeness, !has_all)
  if (nrow(dropped) > 0)
    message(sprintf("[%s] Dropped %d incomplete poll(s): %s", id, nrow(dropped),
                    paste(sprintf("%s (%s)", dropped$pollster, dropped$date),
                          collapse = ", ")))

  semi_join(fresh, filter(poll_completeness, has_all), by = c("date", "pollster"))
}

impute_polls_for_pooling <- function(raw, cfg) {
  parties_all      <- sapply(cfg$parties, `[[`, "id")
  parties_to_check <- parties_all[parties_all != "others"]

  raw %>%
    filter(pollster != "pooled") %>%
    group_by(pollster, date) %>%
    group_modify(function(poll, key) {
      missing_pts <- parties_to_check[!parties_to_check %in% poll$party[!is.na(poll$percent)]]
      if (length(missing_pts) == 0) return(poll)

      ref <- raw %>%
        filter(pollster != "pooled", date < key$date, !is.na(percent), percent > 0) %>%
        mutate(same_pollster = pollster == key$pollster)

      total_votes       <- sum(poll$votes, na.rm = TRUE)
      total_imputed_pct <- 0
      template_row      <- poll[1, ]

      for (mp in missing_pts) {
        mp_ref <- ref %>%
          filter(party == mp) %>%
          arrange(desc(date), desc(same_pollster))

        if (nrow(mp_ref) > 0) {
          imp_pct          <- mp_ref$percent[1]
          new_row          <- template_row
          new_row$party    <- mp
          new_row$percent  <- imp_pct
          new_row$votes    <- as.integer(round((imp_pct / 100) * total_votes))
          poll             <- bind_rows(poll, new_row)
          total_imputed_pct <- total_imputed_pct + imp_pct
        }
      }

      if ("others" %in% poll$party && total_imputed_pct > 0) {
        idx              <- which(poll$party == "others")
        poll$percent[idx] <- max(0, poll$percent[idx] - total_imputed_pct)
        poll$votes[idx]   <- as.integer(round((poll$percent[idx] / 100) * total_votes))
      }

      poll
    }) %>%
    ungroup()
}

compute_pooled <- function(raw, cfg, from_date = NULL) {
  pollsters <- sapply(cfg$pollsters, identity)
  period          <- cfg$pooling$period
  period_extended <- cfg$pooling$period_extended

  # Impute missing parties in individual polls before pooling (imputed values
  # are used only for the pooling calculation — raw rows in polls.json stay clean)
  raw_imputed <- impute_polls_for_pooling(raw, cfg)

  # Reconstruct nested format expected by pool_surveys()
  surveys_nested <- raw_imputed %>%
    nest(survey = c(party, percent, votes)) %>%
    nest(surveys = c(date, start, end, respondents, survey))

  # pool_surveys()/get_eligible() hard-require >= 2 pollster rows in `surveys`
  # (assert_data_frame(min.rows = 2)), even though the actual per-date pooling
  # window only ever needs the eligible polls for that date. When an election
  # currently has data from a single pollster only, pad with an inert
  # placeholder pollster (empty surveys) so the assertion passes; since it has
  # no surveys it never contributes to any date's window or estimate — the
  # result is identical to that pollster's own numbers, same as the existing
  # single-poll-window fallback below.
  if (nrow(surveys_nested) < 2) {
    placeholder <- surveys_nested[1, ]
    placeholder$pollster <- "__placeholder__"
    placeholder$surveys  <- list(surveys_nested$surveys[[1]][0, ])
    surveys_nested <- bind_rows(surveys_nested, placeholder)
  }

  # Pool for each date a raw poll was published; skip dates before from_date if
  # provided (earlier pooled estimates are unaffected by polls published later).
  dates <- raw %>%
    filter(pollster != "pooled") %>%
    pull(date) %>%
    unique() %>%
    sort()

  if (!is.null(from_date))
    dates <- dates[dates >= from_date]

  # pool_surveys() emits a message() (not a warning) once per party when a date's
  # pooling window holds only a single poll. Catch those per date so we log which
  # dates were sparse (one line per date) instead of one identical message per party.
  sparse_dates <- character()
  pooled <- lapply(dates, function(d) {
    withCallingHandlers(
      pool_surveys(
        surveys        = surveys_nested,
        last_date      = d,
        pollsters      = pollsters,
        period         = period,
        period_extended = if (is.null(period_extended)) NA else period_extended
      ),
      message = function(m) {
        if (grepl("Only one survey/pollster", conditionMessage(m))) {
          sparse_dates <<- union(sparse_dates, as.character(d))
          invokeRestart("muffleMessage")
        }
      }
    )
  }) %>%
    bind_rows() %>%
    mutate(election = cfg$id)

  if (length(sparse_dates) > 0)
    message(sprintf("[%s] single-poll window (pooled = that poll) for: %s",
                    cfg$id, paste(sort(sparse_dates), collapse = ", ")))

  window_desc <- if (is.null(period_extended)) sprintf("%d days", period)
                 else sprintf("%d days (extended: %d)", period, period_extended)
  message(sprintf("[%s] Computed pooled estimates for %d date(s) (pooling window: %s)",
                  cfg$id, length(dates), window_desc))
  pooled
}
