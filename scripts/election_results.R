election_result_party_key <- function(x) {
  x <- gsub("&shy;", "", as.character(x), fixed = TRUE)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("\\^[0-9]+|\\[[0-9]+\\]|\\([0-9]+\\)", "", x)
  superscript_digits <- vapply(
    c(0x00B9, 0x00B2, 0x00B3, 0x2070, 0x2074:0x2079),
    intToUtf8,
    character(1)
  )
  digit_replacements <- c("1", "2", "3", "0", as.character(4:9))
  for (i in seq_along(superscript_digits)) {
    x <- gsub(superscript_digits[[i]], digit_replacements[[i]], x, fixed = TRUE)
  }
  umlauts <- c(
    intToUtf8(0x00C4), intToUtf8(0x00D6), intToUtf8(0x00DC),
    intToUtf8(0x00E4), intToUtf8(0x00F6), intToUtf8(0x00FC),
    intToUtf8(0x00DF)
  )
  replacements <- c("A", "O", "U", "a", "o", "u", "ss")
  for (i in seq_along(umlauts)) {
    x <- gsub(umlauts[[i]], replacements[[i]], x, fixed = TRUE)
  }
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  gsub("[^A-Z0-9]+", "", x)
}

parse_election_result_number <- function(x) {
  suppressWarnings(as.numeric(sub(",", ".", trimws(as.character(x)), fixed = TRUE)))
}

default_election_party_lookup <- function() {
  c(
    "CDU/CSU" = "cdu",
    "CDU" = "cdu",
    "SPD" = "spd",
    "GRUNE" = "greens",
    "GRUENE" = "greens",
    "B'90/GRUNE" = "greens",
    "B'90/GRUENE" = "greens",
    "BUNDNIS 90/DIE GRUNEN" = "greens",
    "BUENDNIS 90/DIE GRUENEN" = "greens",
    "FDP" = "fdp",
    "DPS/FDP" = "fdp",
    "DIE LINKE" = "left",
    "PDS/DIE LINKE" = "left",
    "AFD" = "afd",
    "BSW" = "bsw",
    "SSW" = "ssw",
    "SSV/SSW" = "ssw",
    "SONSTIGE" = "others"
  )
}

validate_election_result <- function(result, party_order, expected_seats = NULL,
                                     total_tolerance = 0.2) {
  required_columns <- c("party", "percent", "seats")
  missing_columns <- setdiff(required_columns, names(result))
  if (length(missing_columns) > 0) {
    stop("Election result is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  if (!is.numeric(result$percent) || !is.numeric(result$seats) ||
      any(!is.finite(result$percent)) || any(!is.finite(result$seats))) {
    stop("Election-result percentages and seats must be finite numeric values.")
  }
  if (any(result$percent < 0 | result$percent > 100) ||
      any(result$seats < 0 | result$seats != round(result$seats))) {
    stop("Election-result percentages or seats are outside their valid range.")
  }
  if (anyDuplicated(result$party)) {
    stop("Election result contains duplicate party categories.")
  }
  if (!setequal(result$party, party_order)) {
    stop(
      "Election-result categories do not match the requested categories. Missing: ",
      paste(setdiff(party_order, result$party), collapse = ", "), "; extra: ",
      paste(setdiff(result$party, party_order), collapse = ", "), "."
    )
  }
  if (abs(sum(result$percent) - 100) > total_tolerance) {
    stop("Election-result percentages do not sum to approximately 100 percent.")
  }
  if (!is.null(expected_seats) && sum(result$seats) != expected_seats) {
    stop(
      "Election-result seats sum to ", sum(result$seats),
      ", expected ", expected_seats, "."
    )
  }

  invisible(result)
}

read_wahlrecht_result <- function(result_url, result_year, party_lookup,
                                  party_labels, party_order = names(party_labels),
                                  others_id = "others",
                                  excluded_source_rows = "Wahlbeteiligung",
                                  expected_seats = NULL,
                                  total_tolerance = 0.2,
                                  overrides = NULL) {
  tables <- rvest::read_html(result_url) |>
    rvest::html_table(fill = TRUE)

  year_columns <- function(table) {
    which(grepl(paste0("^", result_year, "([^0-9]|$)"), names(table)))
  }
  table_indices <- which(vapply(tables, function(table) {
    length(year_columns(table)) == 2
  }, logical(1)))

  if (length(table_indices) != 1) {
    stop(
      "Expected exactly one Wahlrecht result table with two columns for ",
      result_year, "."
    )
  }

  result_table <- tables[[table_indices]]
  result_raw <- result_table[, c(1, year_columns(result_table))]
  names(result_raw) <- c("party_label", "percent", "seats")

  lookup <- stats::setNames(
    unname(party_lookup),
    election_result_party_key(names(party_lookup))
  )
  excluded_keys <- election_result_party_key(excluded_source_rows)

  result <- result_raw |>
    dplyr::transmute(
      source_party = election_result_party_key(.data$party_label),
      party = unname(lookup[.data$source_party]),
      fallback_party = unname(lookup[gsub("[0-9]+$", "", .data$source_party)]),
      percent = parse_election_result_number(.data$percent),
      seats = parse_election_result_number(.data$seats)
    ) |>
    dplyr::filter(!(.data$source_party %in% excluded_keys), !is.na(.data$percent)) |>
    dplyr::mutate(
      party = dplyr::coalesce(.data$party, .data$fallback_party),
      party = dplyr::if_else(
        is.na(.data$party) | !(.data$party %in% party_order),
        others_id,
        .data$party
      )
    ) |>
    dplyr::group_by(.data$party) |>
    dplyr::summarise(
      percent = sum(.data$percent),
      seats = sum(dplyr::coalesce(.data$seats, 0)),
      .groups = "drop"
    )

  missing_parties <- setdiff(party_order, result$party)
  if (length(missing_parties) > 0) {
    result <- dplyr::bind_rows(
      result,
      tibble::tibble(party = missing_parties, percent = 0, seats = 0)
    )
  }

  if (!is.null(overrides)) {
    for (party in names(overrides)) {
      if (!(party %in% result$party)) {
        stop("Election-result override refers to unknown party: ", party)
      }
      for (field in names(overrides[[party]])) {
        if (!(field %in% c("percent", "seats"))) {
          stop("Unsupported election-result override field: ", field)
        }
        result[[field]][result$party == party] <- overrides[[party]][[field]]
      }
    }
  }

  result <- result |>
    dplyr::mutate(label = unname(party_labels[.data$party])) |>
    dplyr::select(dplyr::all_of(c("label", "party", "percent", "seats"))) |>
    dplyr::arrange(match(.data$party, party_order))

  validate_election_result(
    result,
    party_order = party_order,
    expected_seats = expected_seats,
    total_tolerance = total_tolerance
  )
  result
}

select_reference_election <- function(config, latest_poll_date) {
  candidates <- config$election_results
  if (is.null(candidates) && !is.null(config$last_result)) {
    return(config$last_result)
  }
  if (is.null(candidates) || length(candidates) == 0) {
    return(NULL)
  }

  latest_poll_date <- as.Date(latest_poll_date)
  election_dates <- as.Date(vapply(candidates, function(x) {
    as.character(x$date)
  }, character(1)))
  eligible <- which(election_dates < latest_poll_date)
  if (length(eligible) == 0) {
    stop("No configured election result predates the latest poll on ", latest_poll_date, ".")
  }

  candidates[[eligible[which.max(election_dates[eligible])]]]
}

read_configured_election_result <- function(config, latest_poll_date) {
  selected <- select_reference_election(config, latest_poll_date)
  if (is.null(selected)) {
    return(NULL)
  }

  party_order <- vapply(config$parties, `[[`, character(1), "id")
  party_labels <- stats::setNames(
    vapply(config$parties, `[[`, character(1), "label"),
    party_order
  )
  lookup <- default_election_party_lookup()
  if (!is.null(selected$party_lookup)) {
    lookup <- c(unlist(selected$party_lookup), lookup)
  }

  result <- read_wahlrecht_result(
    result_url = selected$url,
    result_year = selected$year,
    party_lookup = lookup,
    party_labels = party_labels,
    party_order = party_order,
    expected_seats = selected$expected_seats,
    overrides = selected$overrides
  )
  result$sum_seats <- sum(result$seats)
  result
}
