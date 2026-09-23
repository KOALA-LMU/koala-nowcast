repo_root <- normalizePath(".")
blog_dir <- file.path(repo_root, "dashboard", "blog", "202609_ltwbe")

if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run this script from the project root.")
}

source(file.path(repo_root, "scripts", "blog_functions.R"))

party_labels <- c(
  bsw = "BSW",
  left = "Linke",
  spd = "SPD",
  greens = "Gr\u00fcne",
  fdp = "FDP",
  cdu = "CDU",
  afd = "AfD",
  others = "Sonstige"
)

party_colors <- c(
  bsw = "#6d1f99",
  left = "#be3075",
  spd = "#E3000F",
  greens = "#46962b",
  fdp = "#ffed00",
  cdu = "#1c1c1b",
  afd = "#82caff",
  others = "#bdbdbd"
)

pollster_labels <- c(
  fgw = "Forschgr. Wahlen",
  infratest = "Infratest dimap",
  insa = "INSA",
  forsa = "Forsa",
  pooled = "KOALA"
)

expected_poll_dates <- data.frame(
  pollster = c("insa", "fgw", "infratest", "forsa"),
  date = c("2026-09-18", "2026-09-17", "2026-09-10", "2026-08-22")
)

coalition_specs <- list(
  left_spd_greens = list(
    label = "Linke-SPD-Gr\u00fcne",
    parties = c("left", "spd", "greens"),
    include_minimal = TRUE
  ),
  cdu_spd_greens = list(
    label = "CDU-SPD-Gr\u00fcne",
    parties = c("cdu", "spd", "greens"),
    include_minimal = TRUE
  )
)

plot_titles <- list(
  election_result = paste(
    "Vorl\u00e4ufiges Ergebnis der Abgeordnetenhauswahl Berlin 2026",
    "Zweitstimmenanteile der Parteien",
    sep = "\n"
  ),
  differences = paste(
    "Letzte Einzelumfragen und gepoolte KOALA-Sch\u00e4tzung",
    "Abweichung vom vorl\u00e4ufigen Wahlergebnis",
    sep = "\n"
  ),
  mae = paste(
    "Letzte Einzelumfragen und gepoolte KOALA-Sch\u00e4tzung",
    "Mittlerer absoluter Fehler im Vergleich zum vorl\u00e4ufigen Wahlergebnis",
    sep = "\n"
  )
)

prepared <- prepare_election_blog(
  election_id = "ltw_be",
  election_date = "2026-09-20",
  config_path = file.path(repo_root, "config", "elections", "ltw_be.yml"),
  polls_file = file.path(repo_root, "data", "surveys", "ltw_be", "polls.json"),
  evaluation_dir = file.path(repo_root, "data", "evaluations", "ltw_be"),
  result_url = "https://www.wahlrecht.de/ergebnisse/berlin.htm",
  result_year = 2026,
  party_lookup = c(
    CDU = "cdu",
    SPD = "spd",
    FDP = "fdp",
    GRUNE1 = "greens",
    PDSDIELINKE = "left",
    AFD = "afd",
    BSW = "bsw"
  ),
  party_labels = party_labels,
  party_colors = party_colors,
  pollster_labels = pollster_labels,
  blog_dir = blog_dir,
  result_status = "vorl\u00e4ufig",
  # The Wahlrecht overview currently has a stale AfD value and an empty
  # "Sonstige" cell; the detailed provisional-result page reports these values.
  result_percent_overrides = c(afd = 16.3, others = 5.7),
  expected_result_seats = 158,
  expected_pooled_date = "2026-09-18",
  expected_poll_dates = expected_poll_dates,
  expected_poll_count = 4,
  coalition_specs = coalition_specs,
  simulation_draws = 10000L,
  simulation_seed = 20260918L,
  simulation_correction = 0.005,
  plot_titles = plot_titles
)

print(prepared$metrics)
print(prepared$simulation$coalition_probabilities)
print(prepared$simulation$hurdle_probabilities)
