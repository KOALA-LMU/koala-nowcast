library(coalitions)
library(yaml)
library(dplyr)
library(tidyr)
library(jsonlite)

scrape_election <- function(config_path, oldest_date = as.Date("2025-01-01")) {
  cfg <- read_yaml(config_path)
  cat(sprintf("Scraping polls for %s...\n", cfg$name))

  parties <- sapply(cfg$parties, `[[`, "id")
  parties_required <- sapply(cfg$parties, function(p) if (isTRUE(p$required)) p$id else NULL) |>
    Filter(Negate(is.null), x = _)

  out_dir  <- file.path("data", "surveys", cfg$id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, "polls.json")

  # Load existing data if available
  if (file.exists(out_file)) {
    existing <- as_tibble(fromJSON(out_file)) %>%
      mutate(date = as.Date(date), start = as.Date(start), end = as.Date(end))
    scrape_from <- max(existing$date) - 30  # re-scrape last 30 days to catch late additions
    cat(sprintf("  Existing data up to %s, re-scraping from %s\n", max(existing$date), scrape_from))
  } else {
    existing   <- NULL
    scrape_from <- oldest_date
    cat(sprintf("  No existing data, scraping from %s\n", scrape_from))
  }

  fn <- match.fun(cfg$scraper[["function"]])
  fn_args <- list(address = cfg$scraper$url)
  if ("ind_row_remove" %in% names(formals(fn)) && !is.null(cfg$scraper$ind_row_remove))
    fn_args$ind_row_remove <- -cfg$scraper$ind_row_remove

  fresh <- do.call(fn, fn_args) %>%
    mutate(
      pollster = replace(pollster, pollster == "infratestdimap", "infratest"),
      pollster = gsub("forschungsgruppewahlen", "fgw", pollster)
    ) %>%
    collapse_parties(parties = parties) %>%
    filter(date >= scrape_from) %>%
    mutate(election = cfg$id)

  # Drop poll dates where any required party is missing
  complete_dates <- fresh %>%
    unnest(survey) %>%
    group_by(date) %>%
    summarise(has_all = all(parties_required %in% party), .groups = "drop") %>%
    filter(has_all) %>%
    pull(date)

  n_dropped <- length(unique(fresh$date)) - length(complete_dates)
  if (n_dropped > 0)
    cat(sprintf("  Dropped %d incomplete poll date(s)\n", n_dropped))

  fresh <- filter(fresh, date %in% complete_dates)

  # Find rows not already in existing data
  if (!is.null(existing)) {
    new_rows <- anti_join(fresh, existing, by = c("pollster", "date"))
  } else {
    new_rows <- fresh
  }

  if (nrow(new_rows) == 0) {
    cat("  No new polls.\n")
    return(invisible(FALSE))
  }

  cat(sprintf("  %d new poll(s) found\n", nrow(new_rows)))

  # Append new rows and sort by date descending
  updated <- bind_rows(existing, new_rows) %>%
    arrange(desc(date))

  write_json(updated, out_file, pretty = TRUE, auto_unbox = TRUE)
  cat(sprintf("  Saved to %s\n", out_file))

  invisible(TRUE)
}
