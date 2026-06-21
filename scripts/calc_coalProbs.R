
#' Calculate coalition probabilities for all surveys and dates
#'
#' Calculate the coalition probabilities for all surveys and dates using a YAML
#' election config file. Survey data is read from the JSON file produced by
#' \code{\link{scrape_election}}. Results are saved as RDS files under
#' \code{data/results/<election_id>/}.
#'
#' @param config_path path to a YAML election config file (e.g. \code{"config/elections/ltw_be.yml"})
#' @param nsim number of draws from the posterior
#' @param correction see argument \code{correction} from \code{coalitions::draw_from_posterior()}
#' @param cores number of cores to use for parallel processing. Possible for both Linux-based systems and Windows.
#' @param force_newCalculation If TRUE, recalculate even for dates that were already computed.
#' @import coalitions dplyr tidyr parallel yaml jsonlite
#' @export
calc_coalProbs <- function(config_path, nsim = 10000, correction = 0.005, cores = 1, force_newCalculation = FALSE) {
  if (missing(config_path))
    stop("Please specify a config_path!")
  cfg <- yaml::yaml.load(paste(readLines(config_path, encoding = "UTF-8", warn = FALSE), collapse = "\n"))

  # ── Build config from YAML ──────────────────────────────────────────────────
  parties_all  <- sapply(cfg$parties, `[[`, "id")
  parties      <- parties_all[parties_all != "others"]
  party_labels <- setNames(sapply(cfg$parties, `[[`, "label"), parties_all)

  coals       <- sapply(cfg$coalitions, function(c) paste(c$parties, collapse = "|"))
  coal_labels <- setNames(sapply(cfg$coalitions, `[[`, "label"), coals)

  parl_seats  <- cfg$parliament$seats
  hurdle      <- cfg$parliament$hurdle
  distrib_fun <- get(cfg$parliament$seat_allocation, envir = asNamespace("coalitions"))

  # Normalise a coalition name by sorting its parties alphabetically
  norm_coal <- function(c) paste(sort(strsplit(c, "\\|")[[1]]), collapse = "|")

  # Coalitions that appear with multiple party orderings trigger the strongest-party logic
  coals_norm <- sapply(coals, norm_coal)
  if (length(coals_norm) > 0 && any(table(coals_norm) > 1)) {
    dupe_sets             <- names(table(coals_norm))[table(coals_norm) > 1]
    strongest_party_coals <- coals[coals_norm %in% dupe_sets]
  } else {
    strongest_party_coals <- NULL
  }

  # ── Paths ────────────────────────────────────────────────────────────────────
  surveys_file <- file.path("data", "surveys", cfg$id, "polls.json")
  results_dir  <- file.path("data", "results", cfg$id)
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

  # ── Load surveys (flat JSON produced by scrape_election) ────────────────────
  surveys_byTime <- jsonlite::fromJSON(surveys_file) %>% mutate(date = as.Date(date))
  pollsters      <- sort(unique(surveys_byTime$pollster))
  dates_todo     <- sort(unique(surveys_byTime$date))

  # ── Determine which dates need (re-)computation ──────────────────────────────
  log_file <- file.path(results_dir, "info.log")
  if (!file.exists(log_file) || force_newCalculation) {
    dates <- dates_todo
  } else {
    info        <- readLines(log_file)
    range_done  <- strsplit(strsplit(info, ": ")[[1]][2], ",")[[1]]
    range_done[2] <- substring(range_done[2], 1, nchar(range_done[2]) - 1)
    range_done  <- as.Date(range_done)
    d           <- dates_todo[seq_len(length(dates_todo) - 1)]
    dates       <- c(d[d < range_done[1] | d > range_done[2]], tail(dates_todo, 1))
  }

  # ── Per-pollster computation ─────────────────────────────────────────────────
  results <- lapply(pollsters, function(p) {
    survey_byTime <- surveys_byTime %>% filter(pollster == p)
    dates_ins     <- unique(survey_byTime$date[survey_byTime$date %in% dates])
    if (length(dates_ins) == 0) {
      return(list("coalProbs" = NULL, "sharesSim" = NULL, "shares" = NULL,
                  "coalProbs_grouping" = NULL, "biggestParty" = NULL,
                  "passHurdle" = NULL))
    }
    print(paste0("Perform new calculations for ", p, "..."))

    calc_oneDate <- function(date_ins) {
      survey <- survey_byTime %>%
        filter(date == date_ins) %>%
        distinct(party, .keep_all = TRUE) %>%   # take first record per party when two surveys land on the same day
        select(party, percent, votes) %>%
        right_join(data.frame(party = parties, stringsAsFactors = FALSE), by = "party") %>%
        mutate(percent = ifelse(is.na(percent), 0, percent),
               votes   = ifelse(is.na(votes),   0, votes))

      dirichlet.draws    <- coalitions::draw_from_posterior(survey = survey, nsim = nsim, correction = correction)
      seat.distributions <- coalitions::get_seats(dirichlet.draws, survey = survey,
                                                  distrib.fun = distrib_fun, n_seats = parl_seats)

      res_all   <- calc_allCoalProbs(seat.distributions, parties, dirichlet.draws,
                                     strongest_party_coals = strongest_party_coals, cores = cores)
      coalProbs <- res_all$coalProbs
      allShares <- res_all$shares_perSimulation

      # ── Filter to realistic coalitions ──────────────────────────────────────
      realistic_norms <- c(sapply(parties, norm_coal), sapply(coals, norm_coal))
      is_realistic    <- sapply(allShares$coalition, function(x) norm_coal(x) %in% realistic_norms)
      shares          <- allShares[is_realistic, ]

      # ── Grouped coalition probabilities ─────────────────────────────────────
      res_grouping <- res_all$coalProbs
      # For each YAML coalition find its matching row in res_grouping.
      # Strongest-party coalitions are matched by exact string; all others by sorted party set.
      ids_norm        <- sapply(coals, function(x)
        if (!is.null(strongest_party_coals) && x %in% strongest_party_coals) x else norm_coal(x))
      coals_norm_rows <- sapply(res_grouping$coalition, function(x)
        if (!is.null(strongest_party_coals) && x %in% strongest_party_coals) x else norm_coal(x))
      indices <- sapply(ids_norm, function(id) {
        idx <- which(coals_norm_rows == id)
        if (length(idx)) idx[1] else NA_integer_
      })
      valid <- !is.na(indices)
      res_grouping$coal_type <- NA_character_
      res_grouping$coal_type[indices[valid]] <- coal_labels[coals[valid]]
      res_grouping <- res_grouping[!is.na(res_grouping$coal_type),
                                   !(colnames(res_grouping) %in% c("coalition", "coal_size", "coal_prob"))] %>%
        group_by(coal_type) %>% mutate(across(where(is.numeric), max)) %>% slice(1) %>% ungroup()
      res_grouping$prob <- rowMeans(res_grouping[seq_len(ncol(res_grouping) - 1)])
      res_grouping      <- res_grouping[, c("coal_type", "prob")]

      # ── Biggest-party analyses ───────────────────────────────────────────────
      res_biggestParty <- if (!is.null(cfg$analyses$biggest_party)) {
        bind_rows(lapply(seq_along(cfg$analyses$biggest_party), function(i) {
          p_vec        <- intersect(cfg$analyses$biggest_party[[i]]$parties, colnames(dirichlet.draws))
          biggestParty <- p_vec[apply(dirichlet.draws[, p_vec, drop = FALSE], 1, which.max)]
          data.frame("index" = paste0("biggestParty", i),
                     "party" = p_vec,
                     "prob"  = sapply(p_vec, function(x) sum(biggestParty == x) / nsim, USE.NAMES = FALSE),
                     stringsAsFactors = FALSE)
        }))
      } else {
        data.frame("index" = character(), "party" = character(), "prob" = numeric())
      }

      # ── Hurdle probabilities ─────────────────────────────────────────────────
      partyShares    <- allShares[allShares$coalition %in% parties, colnames(allShares) != "coalition"]
      res_passHurdle <- data.frame("party" = parties,
                                   "prob"  = rowMeans(partyShares > hurdle))

      # ── Attach pollster/date, subsample simulations, return ─────────────────
      coalProbs        <- coalProbs        %>% mutate(pollster = p, date = date_ins) %>% select(pollster, date, everything())
      dirichlet.draws  <- as.data.frame(dirichlet.draws) %>% mutate(pollster = p, date = date_ins) %>% select(pollster, date, everything())
      shares           <- shares           %>% mutate(pollster = p, date = date_ins) %>% select(pollster, date, everything())
      res_grouping     <- res_grouping     %>% mutate(pollster = p, date = date_ins) %>% select(pollster, date, everything())
      res_biggestParty <- res_biggestParty %>% mutate(pollster = p, date = date_ins) %>% select(pollster, date, everything())
      res_passHurdle   <- res_passHurdle   %>% mutate(pollster = p, date = date_ins) %>% select(pollster, date, everything())

      n <- 1000
      if (nrow(dirichlet.draws) > n) {
        dirichlet.draws    <- dirichlet.draws[sample(seq_len(nrow(dirichlet.draws)), n), ]
        coal_share_columns <- grepl("coal_share", colnames(shares))
        shares             <- shares[, c(which(!coal_share_columns), sample(which(coal_share_columns), n))]
        colnames(shares)[which(coal_share_columns)[seq_len(n)]] <- paste0("coal_share", seq_len(n))
      }

      list("coalProbs" = coalProbs, "sharesSim" = dirichlet.draws, "shares" = shares,
           "coalProbs_grouping" = res_grouping, "biggestParty" = res_biggestParty,
           "passHurdle" = res_passHurdle)
    }

    results  <- lapply(dates_ins, calc_oneDate)
    results[sapply(results, is.null)] <- NULL

    list(
      "coalProbs"          = bind_rows(lapply(results, `[[`, "coalProbs")),
      "sharesSim"          = bind_rows(lapply(results, `[[`, "sharesSim")),
      "shares"             = bind_rows(lapply(results, `[[`, "shares")),
      "coalProbs_grouping" = bind_rows(lapply(results, `[[`, "coalProbs_grouping")),
      "biggestParty"       = bind_rows(lapply(results, `[[`, "biggestParty")),
      "passHurdle"         = bind_rows(lapply(results, `[[`, "passHurdle"))
    )
  })

  # ── Bind all pollsters ───────────────────────────────────────────────────────
  coalProbs          <- bind_rows(lapply(results, `[[`, "coalProbs"))
  sharesSim          <- bind_rows(lapply(results, `[[`, "sharesSim"))
  shares             <- bind_rows(lapply(results, `[[`, "shares"))
  coalProbs_grouping <- bind_rows(lapply(results, `[[`, "coalProbs_grouping"))
  biggestParty       <- bind_rows(lapply(results, `[[`, "biggestParty"))
  passHurdle         <- bind_rows(lapply(results, `[[`, "passHurdle"))

  # ── Post-processing of new results (must happen before merging with saved results
  # which are already in post-processed format) ──────────────────────────────────
  coalProbs <- coalProbs %>%
    select(-starts_with("coal_maj")) %>%
    mutate(coal_prob = coal_prob * 100, log.odds = log(coal_prob / (100 - coal_prob))) %>%
    rename(size = coal_size, prob = coal_prob)
  coalProbs_grouping <- coalProbs_grouping %>%
    mutate(prob = prob * 100, log.odds = log(prob / (100 - prob)))
  biggestParty <- biggestParty %>% mutate(prob = prob * 100)
  passHurdle   <- passHurdle   %>% mutate(prob = prob * 100)

  # ── Merge with pre-existing results ─────────────────────────────────────────
  read_result <- function(name) {
    jsonlite::fromJSON(file.path(results_dir, paste0(name, ".json"))) %>% dplyr::mutate(date = as.Date(date))
  }
  if (!identical(dates, dates_todo)) {
    coalProbs          <- bind_rows(coalProbs,          read_result("coalProbs"))
    sharesSim          <- bind_rows(sharesSim,          read_result("sharesSim"))
    shares             <- bind_rows(shares,             read_result("shares"))
    coalProbs_grouping <- bind_rows(coalProbs_grouping, read_result("coalProbs_grouping"))
    biggestParty       <- bind_rows(biggestParty,       read_result("biggestParty"))
    passHurdle         <- bind_rows(passHurdle,         read_result("passHurdle"))
  }

  # ── Save results ─────────────────────────────────────────────────────────────
  write_result <- function(x, name) jsonlite::write_json(x, file.path(results_dir, paste0(name, ".json")), auto_unbox = TRUE, pretty = TRUE)
  write_result(coalProbs,          "coalProbs")
  write_result(sharesSim,          "sharesSim")
  write_result(shares,             "shares")
  write_result(coalProbs_grouping, "coalProbs_grouping")
  write_result(biggestParty,       "biggestParty")
  write_result(passHurdle,         "passHurdle")

  range_todo <- range(dates_todo)
  writeLines(paste0("Results are already calculated for the time: ", range_todo[1], ",", range_todo[2], ")"),
             log_file)
}
