library(jsonlite)
library(dplyr)
library(yaml)

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
    mutate(prob_above_hurdle = prob / 100) %>%
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
    ungroup() %>%
    mutate(prob_above_hurdle = prob / 100) %>%
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

  message(election_id, " dashboard data written to ", out_dir)
}

for (id in c("ltw_st", "ltw_mv", "ltw_be", "btw")) {
  tryCatch(
    prepare_election(id),
    error = function(e) message("Skipping ", id, ": ", conditionMessage(e))
  )
}
