library(jsonlite)
library(dplyr)
library(yaml)
source("scripts/coalition_density.R")
source("scripts/scrape_election_results.R")

# First date whose pooled estimate rests on a fully scraped pooling window.
# pool_surveys() averages over the period_extended days before each date, so the
# estimates for the first window's worth of dates pool over a stretch that
# reaches back past the oldest poll we hold. Those are computed and stored, but
# not shown.
reliable_from <- function(cfg, surveys_dir) {
  window <- cfg$pooling$period_extended
  if (is.null(window)) window <- cfg$pooling$period

  # Measured from where the scrape starts, not from the first poll we happen to
  # hold: a stretch with no polls in it is not a gap in the data. The first poll
  # of a sparse state election can be months after the scrape start, and its
  # window is complete all the same — there was simply nothing to pool.
  if (!is.null(cfg$scraper$oldest_date))
    return(as.Date(as.character(cfg$scraper$oldest_date)) + window)

  raw <- fromJSON(file.path(surveys_dir, "polls.json")) %>%
    filter(pollster != "pooled") %>%
    mutate(date = as.Date(date))
  min(raw$date) + window
}

# Drop the pooling run-up from a series, unless that would leave nothing to plot
# (an election whose whole history is still shorter than one pooling window).
drop_warmup <- function(dat, cutoff, what, election_id) {
  kept <- dat %>% filter(as.Date(date) >= cutoff)
  if (nrow(kept) == 0) {
    message(election_id, ": all ", what, " predates ", cutoff,
            " — showing it anyway, pooling window not yet complete")
    return(dat)
  }
  n_dropped <- length(unique(dat$date)) - length(unique(kept$date))
  if (n_dropped > 0)
    message(election_id, ": hiding ", n_dropped, " ", what, " date(s) before ", cutoff)
  kept
}

prepare_election <- function(election_id) {
  cfg         <- yaml::yaml.load_file(paste0("config/elections/", election_id, ".yml"))
  results_dir <- paste0("data/results/", election_id)
  surveys_dir <- paste0("data/surveys/", election_id)
  out_dir     <- paste0("dashboard/data/", sub("ltw_", "", election_id))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # "Aktualisiert am" on the site: the date of the most recent poll behind this
  # election's numbers. Not a build timestamp — deploy runs after every compute
  # run, including the many where no institute published anything, so that would
  # advance several times a day while the nowcast stands still. The pooled series
  # is excluded because it carries a row per computed date rather than per
  # published poll; the newest raw poll is what actually moved the estimate.
  all_polls  <- fromJSON(file.path(surveys_dir, "polls.json")) %>% mutate(date = as.Date(date))
  raw_polls  <- all_polls %>% filter(pollster != "pooled")
  updated    <- as.character(max(if (nrow(raw_polls) > 0) raw_polls$date else all_polls$date))

  coal <- fromJSON(file.path(results_dir, "coalProbs_grouping.json")) %>%
    filter(pollster == "pooled", date == max(date)) %>%
    mutate(label = coal_type, probability = prob / 100) %>%
    select(label, probability)
  write_json(list(coalitions = coal, updated = updated),
             file.path(out_dir, "coalition_probabilities.json"), auto_unbox = TRUE)

  shares <- fromJSON(file.path(surveys_dir, "polls.json")) %>%
    filter(pollster == "pooled", date == max(date)) %>%
    select(party, percent, date)
  write_json(list(party_shares = shares, updated = updated),
             file.path(out_dir, "party_shares.json"), auto_unbox = TRUE)

  # Both time series start where the pooling window is first fully covered —
  # the raw polls too, so the scatter never runs on past the end of the line.
  cutoff <- reliable_from(cfg, surveys_dir)

  history <- fromJSON(file.path(surveys_dir, "polls.json")) %>%
    select(pollster, date, party, percent) %>%
    drop_warmup(cutoff, "poll", election_id)
  write_json(list(history = history, updated = updated),
             file.path(out_dir, "poll_history.json"), auto_unbox = TRUE)

  coal_history <- fromJSON(file.path(results_dir, "coalProbs_grouping.json")) %>%
    mutate(date = as.Date(date), probability = prob / 100) %>%
    select(pollster, date, label = coal_type, probability) %>%
    drop_warmup(cutoff, "coalition probability", election_id)
  write_json(list(coalitions_history = coal_history, updated = updated),
             file.path(out_dir, "coalition_history.json"), auto_unbox = TRUE)

  hurdle <- fromJSON(file.path(results_dir, "passHurdle.json")) %>%
    filter(pollster == "pooled", date == max(date)) %>%
    group_by(party) %>%
    summarise(prob_above_hurdle = mean(prob / 100), .groups = "drop") %>%
    select(party, prob_above_hurdle)
  write_json(list(hurdle = hurdle, updated = updated),
             file.path(out_dir, "hurdle_probabilities.json"), auto_unbox = TRUE)

  per_pollster <- fromJSON(file.path(surveys_dir, "polls.json")) %>%
    filter(pollster != "pooled") %>%
    group_by(pollster) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    select(pollster, party, percent, date)

  per_pollster_coalitions <- fromJSON(file.path(results_dir, "coalProbs_grouping.json")) %>%
    mutate(date = as.Date(date)) %>%
    filter(pollster != "pooled") %>%
    group_by(pollster) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(probability = prob / 100) %>%
    select(pollster, label = coal_type, probability)

  per_pollster_hurdle <- fromJSON(file.path(results_dir, "passHurdle.json")) %>%
    mutate(date = as.Date(date)) %>%
    filter(pollster != "pooled") %>%
    group_by(pollster) %>%
    filter(date == max(date)) %>%
    group_by(pollster, party) %>%
    summarise(prob_above_hurdle = mean(prob / 100), .groups = "drop") %>%
    select(pollster, party, prob_above_hurdle)

  write_json(
    list(
      per_pollster            = per_pollster,
      per_pollster_coalitions = per_pollster_coalitions,
      per_pollster_hurdle     = per_pollster_hurdle,
      updated                 = updated
    ),
    file.path(out_dir, "per_pollster.json"), auto_unbox = TRUE
  )

  # One record per (pollster, date, coalition), holding the curve as two arrays,
  # rather than 512 rows that each repeat the group's twelve constant fields.
  # This file is by far the largest thing the dashboard loads and the browser has
  # to have all of it before the Koalitionswahrscheinlichkeiten page can draw, so
  # the redundancy is expensive: for the Bundestagswahl, nesting plus rounding the
  # curve to four significant digits takes it from 49 MB to 3 MB.
  #
  # Grouping by "every column except the curve" rather than by a hard-coded key
  # list keeps this correct if coalition_density() gains or drops a column.
  dens <- coalition_density(cfg, results_dir) %>%
    group_by(across(!c(seat_share, density))) %>%
    summarise(
      seat_share = list(signif(seat_share, 4)),
      density    = list(signif(density, 4)),
      .groups    = "drop"
    )
  write_json(
    list(densities = dens, updated = updated),
    file.path(out_dir, "coalition_densities.json"),
    # was auto.unbox, which is not an argument jsonlite knows: it went into `...`
    # and was silently ignored, so this file alone was written boxed
    auto_unbox = TRUE
  )

  if (!is.null(cfg$last_result)) {
    last_result <- scrape_election_results(
      url = cfg$last_result$url,
      election_year = cfg$last_result$year
    )
  }
  write_json(
    list(last_result = last_result, updated = updated),
    file.path(out_dir, "last_results.json"),
    auto_unbox = TRUE
  )

  message(election_id, " dashboard data written to ", out_dir)
}

for (id in c("ltw_st", "ltw_mv", "ltw_be", "btw")) {
  tryCatch(
    prepare_election(id),
    error = function(e) message("Skipping ", id, ": ", conditionMessage(e))
  )
}
