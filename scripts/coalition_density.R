suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

#' Normalise a coalition key by sorting its party ids
#'
#' Converts an ordered coalition key such as \code{"spd|greens|left"} into a
#' canonical, order-independent key such as \code{"greens|left|spd"}. This is
#' used for seat-share densities, where only the summed seats of the member
#' parties matter and leadership order is irrelevant.
#'
#' @param coal Character scalar coalition key with party ids separated by \code{"|"}.
#' @return Character scalar with alphabetically sorted party ids.
norm_coal <- function(coal) {
  stopifnot(is.character(coal) && length(coal) == 1)
  sorted <- sort(strsplit(coal, "|", fixed = TRUE)[[1]])
  paste(sorted, collapse = "|")
}

#' Compute current seat-share density curves for displayed coalitions
#'
#' Builds the data used by the dashboard's seat-distribution view from
#' \code{shares.json}. For each pollster's latest date, coalition orderings that
#' contain the same parties are collapsed into one canonical coalition key before
#' the simulation draws are pivoted. The function then estimates the distribution
#' of each party/coalition's simulated seat share, records the 95% simulation
#' interval, and counts how often all member parties are represented in
#' parliament. It deliberately does not compute coalition probabilities; those
#' come from \code{coalProbs_grouping.json}/\code{coalition_history.json}, where
#' leadership variants and subset-majority rules are handled.
#'
#' @param cfg Election configuration as read from \code{config/elections/*.yml}.
#' Must contain \code{parliament$seats} and \code{parties}.
#' @param results_dir Directory containing the election's result files, including
#' \code{shares.json}.
#' @param until Optional \code{Date}. Rows after it are dropped before each
#' pollster's latest date is picked, so an election already held keeps the curves
#' it had on election day rather than following polls published since. \code{NULL}
#' (the default) uses everything.
#' @return Tibble with one density curve per \code{pollster}, latest \code{date},
#' and canonical \code{coalition}. The output contains \code{seat_share} and
#' \code{density} curve points plus parliament-presence and interval metadata.
coalition_density <- function(cfg, results_dir, until = NULL) {

  parl_seats <- cfg$parliament$seats
  party_labels <- setNames(
    vapply(cfg$parties, `[[`, character(1), "label"),
    vapply(cfg$parties, `[[`, character(1), "id")
  )

  # shares.json holds only each pollster's newest date, not a history, so after
  # an election it no longer carries the draws the result rested on.
  # archive_shares() copies those aside on the first run after the election;
  # prefer that snapshot whenever one exists.
  shares_file <- file.path(results_dir, "shares.json")
  if (!is.null(until)) {
    archived <- file.path(results_dir, paste0("shares_", until, ".json"))
    if (file.exists(archived)) shares_file <- archived
  }

  shares <- jsonlite::fromJSON(shares_file)
  shares$date <- as.Date(shares$date)

  if (!is.null(until)) {
    keep <- shares$date <= until
    if (any(keep)) {
      shares <- shares[keep, , drop = FALSE]
    } else {
      # No snapshot and the live file has moved on — the draws are gone. Show
      # what is there rather than write an empty file the seat view cannot render.
      warning(basename(shares_file), " holds no draws on or before ", until,
              " — the election-day draws were not archived in time; showing the ",
              "newest draws instead")
    }
  }

  latest <- shares %>%
    group_by(pollster) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(coalition = vapply(coalition, norm_coal, character(1))) %>%
    distinct(pollster, date, coalition, .keep_all = TRUE)

  party_ids <- vapply(cfg$parties, `[[`, character(1), "id")
  party_presence <- latest[latest$coalition %in% party_ids, ] %>%
    tidyr::pivot_longer(
      starts_with("coal_share"),
      names_to = "sim",
      names_prefix = "coal_share",
      values_to = "party_seat_share",
      names_transform = list(sim = as.integer)
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
      names_prefix = "coal_share",
      values_to = "seat_share",
      names_transform = list(sim = as.integer)
    )
  latest_long %>%
    group_by(pollster, date, coalition) %>%
    group_modify(function(dat, key) {
      members <- strsplit(key$coalition, "|", fixed = TRUE)[[1]]
      presence <- party_presence %>%
        filter(
          date == key$date,
          pollster == key$pollster,
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

      parliament_presence_n <- sum(dat$all_members_present, na.rm = TRUE)
      simulation_n <- nrow(dat)
      parliament_presence <- parliament_presence_n / simulation_n
      density_values <- dat$seat_share[is.finite(dat$seat_share)]
      has_density <- length(density_values) > 1 && diff(range(density_values)) > 0

      if (has_density) {
        d <- suppressWarnings(density(density_values, from = 0, to = 1, n = 512, bw = "bcv"))
        q <- quantile(density_values, probs = c(0.025, 0.975), na.rm = TRUE)
      } else {
        d <- list(x = seq(0, 1, length.out = 512), y = rep(0, length.out = 512))
        q <- c(`2.5%` = NA_real_, `97.5%` = NA_real_)
      }

      tibble::tibble(
        seat_share = d$x,
        density = d$y,
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
    mutate(label = vapply(strsplit(coalition, "|", fixed = TRUE), function(x) {
      paste(party_labels[x], collapse = "-")
    }, character(1))) %>%
      dplyr::select(
          pollster, date, coalition, label, seat_share, density,
          parliament_presence, parliament_presence_n, simulation_n,
          ci_lower, ci_upper, ci_lower_seats, ci_upper_seats
    )
}
