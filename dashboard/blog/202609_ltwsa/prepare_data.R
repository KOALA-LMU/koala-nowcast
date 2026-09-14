repo_root <- normalizePath(".")
blog_dir <- file.path(repo_root, "dashboard", "blog", "202609_ltwsa")

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
  pollytix = "Pollytix",
  pooled = "KOALA"
)

expected_poll_dates <- data.frame(
  pollster = c("fgw", "insa", "infratest", "pollytix"),
  date = c("2026-09-03", "2026-09-02", "2026-08-26", "2026-08-12")
)

coalition_specs <- list(
  cdu_left_spd_greens = list(
    label = "CDU-Linke-SPD-Gr\u00fcne",
    parties = c("cdu", "left", "spd", "greens"),
    include_minimal = TRUE
  ),
  afd = list(
    label = "AfD",
    parties = "afd"
  )
)

plot_titles <- list(
  latest_polls = "Letzte Wahlumfragen und KOALA-Sch\u00e4tzung vor der Wahl",
  election_result = paste(
    "Vorl\u00e4ufiges Ergebnis der Landtagswahl Sachsen-Anhalt 2026",
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
  election_id = "ltw_st",
  election_date = "2026-09-06",
  config_path = file.path(repo_root, "config", "elections", "ltw_st.yml"),
  polls_file = file.path(repo_root, "data", "surveys", "ltw_st", "polls.json"),
  evaluation_dir = file.path(repo_root, "data", "evaluations", "ltw_st"),
  result_url = "https://www.wahlrecht.de/ergebnisse/sachsen-anhalt.htm",
  result_year = 2026,
  party_lookup = c(
    CDU = "cdu",
    SPD = "spd",
    DIELINKE1 = "left",
    FDP = "fdp",
    B90GRUNE2 = "greens",
    AFD = "afd",
    BSW = "bsw"
  ),
  party_labels = party_labels,
  party_colors = party_colors,
  pollster_labels = pollster_labels,
  blog_dir = blog_dir,
  result_status = "vorl\u00e4ufig",
  expected_result_seats = 83,
  expected_pooled_date = "2026-09-03",
  expected_poll_dates = expected_poll_dates,
  expected_poll_count = 4,
  coalition_specs = coalition_specs,
  simulation_draws = 10000L,
  simulation_seed = 20260903L,
  simulation_correction = 0.005,
  plot_titles = plot_titles
)

print(prepared$metrics)
print(prepared$simulation$coalition_probabilities)
print(prepared$simulation$hurdle_probabilities)
