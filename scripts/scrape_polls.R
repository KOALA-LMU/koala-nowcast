library(coalitions)
library(yaml)
library(dplyr)
library(tidyr)
library(jsonlite)

scrape_election <- function(config_path, oldest_date = as.Date("2025-01-01")) {
  cfg <- read_yaml(config_path)
  message(sprintf("Scraping polls for %s...\n", cfg$name))

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
        message("  Could not parse existing JSON, starting fresh\n")
        NULL
      }
    )
    if (!is.null(existing)) {
      scrape_from <- max(existing$date) - 30
      message(sprintf("  Existing data up to %s, re-scraping from %s\n", max(existing$date), scrape_from))
    } else {
      scrape_from <- oldest_date
    }
  } else {
    existing   <- NULL
    scrape_from <- oldest_date
    message(sprintf("  No existing data, scraping from %s\n", scrape_from))
  }

  # assign scraper function to fn
  fn <- match.fun(cfg$scraper[["function"]])
  # define fn arguments and define them as needed
  fn_args <- list()
  if ("address" %in% names(formals(fn)) && !is.null(cfg$scraper$url))
    fn_args$address <- cfg$scraper$url
  if ("ind_row_remove" %in% names(formals(fn)) && !is.null(cfg$scraper$ind_row_remove))
    fn_args$ind_row_remove <- -cfg$scraper$ind_row_remove
  # call function and replace/edit field values based on conventions
  fresh <- do.call(fn, fn_args) %>%
    mutate(
      pollster = replace(pollster, pollster == "infratestdimap", "infratest"),
      pollster = gsub("forschungsgruppewahlen", "fgw", pollster)
    ) %>%
    collapse_parties(parties = parties) %>%
    unnest(survey) %>%
    filter(date >= scrape_from) %>%
    mutate(election = cfg$id)

  # Drop poll dates where any required party is missing
  complete_dates <- fresh %>%
    group_by(date, pollster) %>%
    summarise(has_all = all(parties_required %in% party), .groups = "drop") %>%
    group_by(date) %>%
    summarise(all_complete = all(has_all), .groups = "drop") %>%
    filter(all_complete) %>%
    pull(date)

  n_dropped <- length(unique(fresh$date)) - length(complete_dates)
  if (n_dropped > 0)
    message(sprintf("  Dropped %d incomplete poll date(s)\n", n_dropped))

  fresh <- filter(fresh, date %in% complete_dates)

  # Find rows not already in existing data
  if (!is.null(existing)) {
    new_rows <- anti_join(fresh, existing, by = c("pollster", "date", "party"))
  } else {
    new_rows <- fresh
  }

  has_new_raw    <- nrow(new_rows) > 0
  has_no_pooled  <- is.null(existing) || !any(existing$pollster == "pooled")

  if (!has_new_raw && !has_no_pooled) {
    message("  No new polls.\n")
    return(invisible(FALSE))
  }

  if (has_new_raw)
    message(sprintf("  %d new poll(s) found\n", nrow(new_rows)))
  if (has_no_pooled)
    message("  No pooled data found, computing pooled estimates\n")

  # Merge existing raw polls with new rows
  raw_updated <- bind_rows(
    if (!is.null(existing)) existing %>% filter(pollster != "pooled") else NULL,
    new_rows
  ) %>% arrange(desc(date))

  # Recompute pooled estimates for all affected dates
  pooled_updated <- compute_pooled(raw_updated, cfg)

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
  message(sprintf("  Saved to %s\n", out_file))

  invisible(TRUE)
}

compute_pooled <- function(raw, cfg) {
  pollsters <- sapply(cfg$pollsters, identity)
  period          <- cfg$pooling$period
  period_extended <- cfg$pooling$period_extended

  # Reconstruct nested format expected by pool_surveys()
  surveys_nested <- raw %>%
    filter(pollster != "pooled") %>%
    nest(survey = c(party, percent, votes)) %>%
    nest(surveys = c(date, start, end, respondents, survey))

  # Pool for each date a raw poll was published
  dates <- raw %>%
    filter(pollster != "pooled") %>%
    pull(date) %>%
    unique() %>%
    sort()

  pooled <- lapply(dates, function(d) {
    pool_surveys(
      surveys        = surveys_nested,
      last_date      = d,
      pollsters      = pollsters,
      period         = period,
      period_extended = if (is.null(period_extended)) NA else period_extended
    )
  }) %>%
    bind_rows() %>%
    mutate(election = cfg$id)

  message(sprintf("  Computed pooled estimates for %d date(s)\n", length(dates)))
  pooled
}
