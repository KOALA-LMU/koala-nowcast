library(jsonlite)
library(dplyr)
library(yaml)

cfg         <- yaml::yaml.load_file("config/elections/ltw_st.yml")
results_dir <- "data/results/ltw_st"
surveys_dir <- "data/surveys/ltw_st"
out_dir     <- "st-dashboard/data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

coal_labels <- setNames(
  sapply(cfg$coalitions, `[[`, "label"),
  sapply(cfg$coalitions, function(c) paste(c$parties, collapse = "|"))
)
updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

# coalition_probabilities.json
coal <- fromJSON(file.path(results_dir, "coalProbs.json")) %>%
  filter(pollster == "pooled", date == max(date),
         coalition %in% names(coal_labels)) %>%
  mutate(label = coal_labels[coalition], probability = prob / 100) %>%
  select(label, probability)
write_json(list(coalitions = coal, updated = updated),
           file.path(out_dir, "coalition_probabilities.json"), auto_unbox = TRUE)

# party_shares.json
shares <- fromJSON(file.path(surveys_dir, "polls.json")) %>%
  filter(pollster == "pooled", date == max(date)) %>%
  select(party, percent)
write_json(list(party_shares = shares, updated = updated),
           file.path(out_dir, "party_shares.json"), auto_unbox = TRUE)

# hurdle_probabilities.json
hurdle <- fromJSON(file.path(results_dir, "passHurdle.json")) %>%
  filter(pollster == "pooled", date == max(date)) %>%
  mutate(prob_above_hurdle = prob / 100) %>%
  select(party, prob_above_hurdle)
write_json(list(hurdle = hurdle, updated = updated),
           file.path(out_dir, "hurdle_probabilities.json"), auto_unbox = TRUE)

# per_pollster.json
per_pollster <- fromJSON(file.path(surveys_dir, "polls.json")) %>%
  filter(pollster != "pooled") %>%
  group_by(pollster) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(pollster, party, percent)
write_json(list(per_pollster = per_pollster, updated = updated),
           file.path(out_dir, "per_pollster.json"), auto_unbox = TRUE)

message("ST dashboard data written to ", out_dir)
