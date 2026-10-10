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
#' contain the same parties are collapsed into one canonical coalition key. The
#' function then reconstructs the distribution of each party/coalition's
#' simulated seat share from the stored quantile grid and reads the 95%
#' simulation interval off it; how often all member parties are represented in
#' parliament is taken as computed, since that count cannot be recovered from
#' per-coalition quantiles (see \code{summarise_shareDraws()}). It deliberately
#' does not compute coalition probabilities; those come from
#' \code{coalProbs_grouping.json}/\code{coalition_history.json}, where
#' leadership variants and subset-majority rules are handled.
#'
#' @param cfg Election configuration as read from \code{config/elections/*.yml}.
#' Must contain \code{parliament$seats} and \code{parties}.
#' @param results_dir Directory containing the election's result files, including
#' \code{shares.json}.
#' @return Tibble with one density curve per \code{pollster}, latest \code{date},
#' and canonical \code{coalition}. The output contains \code{seat_share} and
#' \code{density} curve points plus parliament-presence and interval metadata.
coalition_density <- function(cfg, results_dir) {

  parl_seats <- cfg$parliament$seats
  party_labels <- setNames(
    vapply(cfg$parties, `[[`, character(1), "label"),
    vapply(cfg$parties, `[[`, character(1), "id")
  )

  shares <- jsonlite::fromJSON(file.path(results_dir, "shares.json"))
  shares$date <- as.Date(shares$date)

  latest <- shares %>%
    group_by(pollster) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(coalition = vapply(coalition, norm_coal, character(1))) %>%
    distinct(pollster, date, coalition, .keep_all = TRUE)

  # The grid is not stored alongside the values: it is the midpoint grid
  # summarise_shareDraws() wrote them on, so the column count determines it.
  q_cols <- grep("^q[0-9]+$", colnames(latest), value = TRUE)
  probs  <- (seq_along(q_cols) - 0.5) / length(q_cols)

  dplyr::bind_rows(lapply(seq_len(nrow(latest)), function(i) {
    row <- latest[i, ]
    q   <- as.numeric(row[, q_cols])

    # The quantiles are a thinned stand-in for the draws, so the bandwidth is the
    # one measured on the draws themselves rather than one re-estimated here.
    has_density <- is.finite(row$bw) && diff(range(q)) > 0
    if (has_density) {
      d  <- suppressWarnings(density(q, bw = row$bw, from = 0, to = 1, n = 512))
      ci <- stats::approx(probs, q, xout = c(0.025, 0.975))$y
    } else {
      d  <- list(x = seq(0, 1, length.out = 512), y = rep(0, length.out = 512))
      ci <- c(NA_real_, NA_real_)
    }

    tibble::tibble(
      pollster              = row$pollster,
      date                  = row$date,
      coalition             = row$coalition,
      seat_share            = d$x,
      density               = d$y,
      parliament_presence   = row$parliament_presence_n / row$simulation_n,
      parliament_presence_n = row$parliament_presence_n,
      simulation_n          = row$simulation_n,
      ci_lower              = ci[[1]],
      ci_upper              = ci[[2]],
      ci_lower_seats        = ceiling(ci[[1]] * parl_seats),
      ci_upper_seats        = floor(ci[[2]] * parl_seats)
    )
  })) %>%
    mutate(label = vapply(strsplit(coalition, "|", fixed = TRUE), function(x) {
      paste(party_labels[x], collapse = "-")
    }, character(1))) %>%
    dplyr::select(
      pollster, date, coalition, label, seat_share, density,
      parliament_presence, parliament_presence_n, simulation_n,
      ci_lower, ci_upper, ci_lower_seats, ci_upper_seats
    )
}
