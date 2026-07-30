
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

  parl_seats  <- cfg$parliament$seats
  hurdle      <- cfg$parliament$hurdle
  distrib_fun <- get(cfg$parliament$seat_allocation, envir = asNamespace("coalitions"))

  # ── Paths ────────────────────────────────────────────────────────────────────
  surveys_file <- file.path("data", "surveys", cfg$id, "polls.json")
  results_dir  <- file.path("data", "results", cfg$id)
  results_file <- file.path(results_dir, "coalProbs.json")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

  # ── Load surveys (flat JSON produced by scrape_election) ────────────────────
  surveys_byTime <- jsonlite::fromJSON(surveys_file) %>% mutate(date = as.Date(date))
  pollsters      <- sort(unique(surveys_byTime$pollster))
  dates_todo     <- sort(unique(surveys_byTime$date))

  # If the YAML has no coalitions: list at all, derive it dynamically from the
  # current pooled vote shares instead of relying on a fixed, hand-authored set
  # (see derive_dynamic_coalitions() in calc_coalProbs_helpers.R).
  if (is.null(cfg$coalitions) || length(cfg$coalitions) == 0) {
    pooled_latest <- surveys_byTime %>% filter(pollster == "pooled") %>% filter(date == max(date))
    pooled_shares <- setNames(pooled_latest$percent, pooled_latest$party)
    cfg$coalitions <- derive_dynamic_coalitions(cfg$parties, pooled_shares)
  }

  coals       <- sapply(cfg$coalitions, function(c) paste(c$parties, collapse = "|"))
  coal_labels <- setNames(sapply(cfg$coalitions, `[[`, "label"), coals)

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

  # ── Determine which dates need (re-)computation ──────────────────────────────
  pending_file <- file.path("data", "surveys", cfg$id, "pending_dates.json")

  if (force_newCalculation) {
    dates <- dates_todo
  } else if (file.exists(pending_file)) {
    # scrape_election wrote exactly which dates have new/updated pooled estimates
    pending <- as.Date(jsonlite::fromJSON(pending_file))
    dates   <- dates_todo[dates_todo %in% pending]
    file.remove(pending_file)
  } else if (!file.exists(results_file)) {
    dates <- dates_todo
  } else {
    # Recompute any survey date missing from the existing results, wherever it falls
    computed_dates <- unique(as.Date(jsonlite::fromJSON(results_file)$date))
    dates          <- dates_todo[!dates_todo %in% computed_dates]
  }

  # ── Parallelisation strategy ─────────────────────────────────────────────────
  # `cores` is spent on the date loop rather than on calc_allCoalProbs(): the
  # per-date cost is dominated by draw_from_posterior() + get_seats(), both
  # single-threaded, so the coalition-level split can only shave off part of the
  # work while the date-level split scales the whole thing. calc_allCoalProbs()
  # therefore runs sequentially inside each worker — nested forking would
  # oversubscribe the machine. mclapply() has no parallel path on Windows, so fall
  # back to the coalition-level split there.
  dates_in_parallel <- cores > 1 && Sys.info()[["sysname"]] != "Windows"
  cores_coals       <- if (dates_in_parallel) 1 else cores

  # ── Per-pollster computation ─────────────────────────────────────────────────
  results <- lapply(pollsters, function(p) {
    survey_byTime <- surveys_byTime %>% filter(pollster == p)
    dates_ins     <- unique(survey_byTime$date[survey_byTime$date %in% dates])
    if (length(dates_ins) == 0) {
      return(list("coalProbs" = NULL, "sharesSim" = NULL, "shares" = NULL,
                  "coalProbs_grouping" = NULL, "biggestParty" = NULL,
                  "passHurdle" = NULL))
    }
    print(paste0("Perform new calculations for ", cfg$name, " (", p, ")..."))

    calc_oneDate <- function(date_ins) {
      survey_raw <- survey_byTime %>%
        filter(date == date_ins) %>%
        distinct(party, .keep_all = TRUE) %>%   # take first record per party when two surveys land on the same day
        select(party, percent, votes)

      survey <- survey_raw %>%
        filter(party != "others") %>%
        right_join(data.frame(party = parties, stringsAsFactors = FALSE), by = "party")

      others_row <- survey_raw %>% filter(party == "others")
      if (nrow(others_row) > 0)
        survey <- bind_rows(survey, others_row %>% select(party, percent, votes))

      survey <- survey %>%
        mutate(percent = ifelse(is.na(percent), 0, percent),
               votes   = ifelse(is.na(votes),   0, votes)) %>%
        # coalitions::sls() re-sorts parties alphabetically internally and returns
        # seat counts with no names attached; coalitions::get_seats() then reattaches
        # them positionally to survey's original row order. Pre-sorting here so that
        # order already matches what sls() assumes avoids that mislabeling.
        arrange(party)

      # Parties that did not exist yet at this date reach this point at percent = 0
      # (filled in by the right_join above). Handing a 0% party to
      # draw_from_posterior(correction = ...) is harmful: the correction jitters every
      # share by up to +-correction, so a party at 0 draws a negative share in roughly
      # half the simulations and the coalitions package silently discards those whole
      # draws — halving the effective sample. Such parties are dropped from this date
      # only: they carry no row at all in this date's results (rather than a
      # misleading 0) and reappear on every date where they are polled.
      parties_ins    <- parties[parties %in% survey$party[survey$percent > 0]]
      parties_absent <- setdiff(parties, parties_ins)
      if (length(parties_absent) > 0) {
        survey <- survey %>% filter(percent > 0)
        message(sprintf("[%s] %s: no support for %s — excluded for this date",
                        cfg$id, date_ins, paste(parties_absent, collapse = ", ")))
      }

      # Seed explicitly per (pollster, date): draw_from_posterior() defaults to
      # seed = as.numeric(now()), which set.seed() truncates to whole seconds, so
      # workers starting in the same second would draw from the same stream. Deriving
      # it from the pollster and date instead keeps each date independent (and makes
      # a re-run reproducible).
      seed_ins <- sum(utf8ToInt(p)) * 100003L + as.integer(as.Date(date_ins))

      dirichlet.draws    <- coalitions::draw_from_posterior(survey = survey, nsim = nsim,
                                                            correction = correction, seed = seed_ins)
      # Drop "others" before seat allocation so majorities are computed over the
      # explicitly modelled parties only (matching the parties vector).
      dirichlet.draws    <- dirichlet.draws[, colnames(dirichlet.draws) != "others", drop = FALSE]
      seat.distributions <- coalitions::get_seats(dirichlet.draws,
                                                  survey = survey %>% filter(party != "others"),
                                                  distrib.fun = distrib_fun, n_seats = parl_seats)

      # Strongest-party coalitions naming a party that is absent at this date have no
      # simulated shares to look up, so restrict them to the parties in the simulation.
      spc_ins <- if (is.null(strongest_party_coals)) NULL else {
        keep <- sapply(strongest_party_coals,
                       function(x) all(strsplit(x, "\\|")[[1]] %in% parties_ins))
        if (any(keep)) strongest_party_coals[keep] else NULL
      }

      res_all   <- calc_allCoalProbs(seat.distributions, parties_ins, dirichlet.draws,
                                     strongest_party_coals = spc_ins, cores = cores_coals)
      coalProbs <- res_all$coalProbs
      allShares <- res_all$shares_perSimulation

      # ── Filter to realistic coalitions ──────────────────────────────────────
      realistic_norms <- c(sapply(parties_ins, norm_coal), sapply(coals, norm_coal))
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
          # Normalise by the draws actually retained, not by nsim: draw_from_posterior()
          # drops draws that came out negative, so nrow() can be lower. Parties absent
          # at this date are not in p_vec and get no row for this date.
          data.frame("index" = paste0("biggestParty", i),
                     "party" = p_vec,
                     "prob"  = sapply(p_vec, function(x) sum(biggestParty == x) / nrow(dirichlet.draws),
                                      USE.NAMES = FALSE),
                     stringsAsFactors = FALSE)
        }))
      } else {
        data.frame("index" = character(), "party" = character(), "prob" = numeric())
      }

      # ── Hurdle probabilities ─────────────────────────────────────────────────
      # Parties excluded above simply have no row for this date
      partyShares    <- allShares[allShares$coalition %in% parties_ins, ]
      res_passHurdle <- data.frame(
        "party" = partyShares$coalition,
        "prob"  = rowMeans(partyShares[, colnames(partyShares) != "coalition"] > hurdle)
      )

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

    results <- if (dates_in_parallel) {
      # mclapply() returns a "try-error" per failed element instead of aborting, so
      # surface the first failure rather than writing silently incomplete results.
      out    <- parallel::mclapply(dates_ins, calc_oneDate, mc.cores = cores)
      failed <- vapply(out, inherits, logical(1), "try-error")
      if (any(failed))
        stop(sprintf("[%s] %d of %d date(s) failed, first error: %s", cfg$id,
                     sum(failed), length(out), conditionMessage(attr(out[[which(failed)[1]]], "condition"))))
      out
    } else {
      lapply(dates_ins, calc_oneDate)
    }
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
    coalProbs          <- bind_rows(coalProbs,          read_result("coalProbs")          %>% filter(!date %in% dates))
    sharesSim          <- bind_rows(sharesSim,          read_result("sharesSim")          %>% filter(!date %in% dates))
    shares             <- bind_rows(shares,             read_result("shares")             %>% filter(!date %in% dates))
    coalProbs_grouping <- bind_rows(coalProbs_grouping, read_result("coalProbs_grouping") %>% filter(!date %in% dates))
    biggestParty       <- bind_rows(biggestParty,       read_result("biggestParty")       %>% filter(!date %in% dates))
    passHurdle         <- bind_rows(passHurdle,         read_result("passHurdle")         %>% filter(!date %in% dates))
  }

  # ── Sort final output by date, then pollster ─────────────────────────────────
  coalProbs          <- coalProbs          %>% dplyr::arrange(date, pollster)
  sharesSim          <- sharesSim          %>% dplyr::arrange(date, pollster)
  shares             <- shares             %>% dplyr::arrange(date, pollster)
  coalProbs_grouping <- coalProbs_grouping %>% dplyr::arrange(date, pollster)
  biggestParty       <- biggestParty       %>% dplyr::arrange(date, pollster)
  passHurdle         <- passHurdle         %>% dplyr::arrange(date, pollster)

  # ── Save results ─────────────────────────────────────────────────────────────
  write_result <- function(x, name) jsonlite::write_json(x, file.path(results_dir, paste0(name, ".json")), auto_unbox = TRUE, pretty = TRUE)
  write_result(coalProbs,          "coalProbs")
  write_result(sharesSim,          "sharesSim")
  write_result(shares,             "shares")
  write_result(coalProbs_grouping, "coalProbs_grouping")
  write_result(biggestParty,       "biggestParty")
  write_result(passHurdle,         "passHurdle")
}
