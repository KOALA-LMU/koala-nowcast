library(jsonlite)
library(dplyr)
library(yaml)
source("scripts/calc_coalProbs_helpers.R")

coalition_density <- function(election_id, cfg, results_dir) {
  parl_seats     <- cfg$parliament$seats

  coal_labels <- setNames(
    vapply(cfg$coalitions, `[[`, character(1), "label"),
    vapply(cfg$coalitions, function(c) {
      paste(c$parties, collapse = "|")
    }, character(1))
  )

  shares <- jsonlite::fromJSON(file.path(results_dir, "shares.json")) %>%
    mutate(date = as.Date(date))

  latest <- shares %>%
    group_by(pollster) %>%
    filter(date == max(date)) %>%
    ungroup()

  party_ids <- vapply(cfg$parties, `[[`, character(1), "id")
  party_presence <- latest %>%
    filter(coalition %in% party_ids) %>%
    tidyr::pivot_longer(
      starts_with("coal_share"),
      names_to = "sim",
      values_to = "party_seat_share"
    ) %>%
    transmute(
      pollster,
      date,
      sim,
      party = coalition,
      party_present = party_seat_share > 0
    )

  latest_long <- latest %>%
    tidyr::pivot_longer(
      starts_with("coal_share"),
      names_to = "sim",
      values_to = "seat_share"
    )

  wanted_coals <- names(coal_labels)
  coal_parties <- strsplit(wanted_coals, "|", fixed = TRUE)
  non_afd_coals <- !vapply(coal_parties, function(x) {
    "afd" %in% x && length(x) > 1
  }, logical(1))
  wanted_coals <- wanted_coals[non_afd_coals]

  latest_long <- latest_long %>%
    filter(coalition %in% wanted_coals)

  latest_long %>%
    group_by(pollster, date, coalition) %>%
    group_modify(function(dat, key) {
      members <- strsplit(key$coalition, "|", fixed = TRUE)[[1]]
      presence <- party_presence %>%
        filter(
          pollster == key$pollster,
          date == key$date,
          party %in% members
        ) %>%
        group_by(sim) %>%
        summarise(
          all_members_present = all(members %in% party) && all(party_present),
          .groups = "drop"
        )

      dat <- dat %>%
        left_join(presence, by = "sim") %>%
        mutate(all_members_present = tidyr::replace_na(all_members_present, FALSE))

      parliament_presence_n <- sum(dat$all_members_present)
      simulation_n <- nrow(dat)
      parliament_presence <- parliament_presence_n / simulation_n
      density_values <- dat$seat_share[is.finite(dat$seat_share)]
      has_density <- length(density_values) > 1 && diff(range(density_values)) > 0

      if (has_density) {
        d <- suppressWarnings(density(density_values, from = 0, to = 1, n = 512, bw = "bcv"))
        q <- quantile(density_values, probs = c(0.025, 0.975), na.rm = TRUE)
      } else {
        d <- list(x = seq(0, 1, length.out = 512), y = rep(0, 512))
        q <- c(`2.5%` = NA_real_, `97.5%` = NA_real_)
      }

      subset_coalitions <- latest %>%
        filter(
          pollster == key$pollster,
          date == key$date
        ) %>%
        pull(coalition) %>%
        unique()
      subset_coalitions <- subset_coalitions[vapply(subset_coalitions, function(coal) {
        parties <- strsplit(coal, "|", fixed = TRUE)[[1]]
        length(parties) < length(members) && all(parties %in% members)
      }, logical(1))]

      if (length(subset_coalitions) > 0) {
        subset_majorities <- shares %>%
          filter(
            pollster == key$pollster,
            date == key$date,
            coalition %in% subset_coalitions
          ) %>%
          tidyr::pivot_longer(
            starts_with("coal_share"),
            names_to = "sim",
            values_to = "seat_share"
          ) %>%
          group_by(sim) %>%
          summarise(
            subset_has_majority = any(seat_share > 0.5, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        subset_majorities <- tibble::tibble(
          sim = unique(dat$sim),
          subset_has_majority = FALSE
        )
      }

      dat <- dat %>%
        left_join(subset_majorities, by = "sim") %>%
        mutate(subset_has_majority = tidyr::replace_na(subset_has_majority, FALSE))

      probability <- mean(
        dat$seat_share > 0.5 &
          dat$all_members_present &
          !dat$subset_has_majority
      )

      tibble::tibble(
        seat_share = d$x,
        density = d$y,
        prob_majority = probability,
        parliament_presence = parliament_presence,
        parliament_presence_n = parliament_presence_n,
        simulation_n = simulation_n,
        ci_lower = q[[1]],
        ci_upper = q[[2]],
        ci_lower_seats = ceiling(q[[1]] * parl_seats),
        ci_upper_seats = floor(q[[2]] * parl_seats)
      )
    }) %>%
    ungroup() %>%
    mutate(label = coal_labels[coalition]) %>%
    dplyr::select(
      pollster, date, coalition, label, seat_share, density, prob_majority,
      parliament_presence, parliament_presence_n, simulation_n,
      ci_lower, ci_upper, ci_lower_seats, ci_upper_seats
    )
}

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

  # If the YAML has no coalitions: list at all, derive it dynamically from the
  # current pooled vote shares (mirrors calc_coalProbs.R, so the labels this
  # script expects to find in coalProbs_grouping.json match what was computed).
  if (is.null(cfg$coalitions) || length(cfg$coalitions) == 0) {
    polls          <- fromJSON(file.path(surveys_dir, "polls.json")) %>% mutate(date = as.Date(date))
    pooled_latest  <- polls %>% filter(pollster == "pooled", date == max(date))
    pooled_shares  <- setNames(pooled_latest$percent, pooled_latest$party)
    cfg$coalitions <- derive_dynamic_coalitions(cfg$parties, pooled_shares)
  }

  coal_labels <- setNames(
    sapply(cfg$coalitions, `[[`, "label"),
    sapply(cfg$coalitions, function(c) paste(c$parties, collapse = "|"))
  )
  updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

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
  dens <- coalition_density(cfg$id, cfg, results_dir) %>%
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

  message(election_id, " dashboard data written to ", out_dir)
}

for (id in c("ltw_st", "ltw_mv", "ltw_be", "btw")) {
  tryCatch(
    prepare_election(id),
    error = function(e) message("Skipping ", id, ": ", conditionMessage(e))
  )
}
