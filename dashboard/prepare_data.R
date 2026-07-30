library(jsonlite)
library(dplyr)
library(yaml)

coalition_density <- function(election_id, cfg, results_dir) {
  parl_seats <- cfg$parliament$seats
  majority_seats <- cfg$parliament$majority

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

  wanted_coals <- names(coal_labels)
  coal_parties <- strsplit(wanted_coals, "|", fixed = TRUE)
  non_afd_coals <- !vapply(coal_parties, function(x) {
    "afd" %in% x && length(x) > 1
  }, logical(1))
  non_bsw_coals <- !vapply(coal_parties, function(x) {
    "bsw" %in% x && length(x) > 1
  }, logical(1))
  wanted_coals <- wanted_coals[non_afd_coals & non_bsw_coals]

  latest <- latest %>%
    filter(coalition %in% wanted_coals)

  latest <- tidyr::pivot_longer(
    latest,
    starts_with("coal_share"),
    names_to = "sim",
    values_to = "seat_share"
  )

  latest %>%
    group_by(pollster, date, coalition) %>%
    group_modify(function(dat, key) {
      d <- suppressWarnings(density(dat$seat_share, from = 0, to = 1, n = 512, bw = "bcv"))
      q <- quantile(dat$seat_share, probs = c(0.025, 0.975), na.rm = TRUE)
      seats <- dat$seat_share * parl_seats

      tibble::tibble(
        seat_share = d$x,
        density = d$y,
        prob_majority = mean(seats >= majority_seats),
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
      ci_lower, ci_upper, ci_lower_seats, ci_upper_seats
    )
}

prepare_election <- function(election_id) {
  cfg         <- yaml::yaml.load_file(paste0("config/elections/", election_id, ".yml"))
  results_dir <- paste0("data/results/", election_id)
  surveys_dir <- paste0("data/surveys/", election_id)
  out_dir     <- paste0("dashboard/data/", sub("ltw_", "", election_id))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

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
    select(party, percent)
  write_json(list(party_shares = shares, updated = updated),
             file.path(out_dir, "party_shares.json"), auto_unbox = TRUE)

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
    select(pollster, party, percent)

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

  dens <- coalition_density(cfg$id, cfg, results_dir)
  write_json(
    list(densities = dens, updated = updated),
    file.path(out_dir, "coalition_densities.json"),
    auto.unbox = TRUE
  )

  message(election_id, " dashboard data written to ", out_dir)
}

for (id in c("ltw_st", "ltw_mv", "ltw_be", "btw")) {
  tryCatch(
    prepare_election(id),
    error = function(e) message("Skipping ", id, ": ", conditionMessage(e))
  )
}
