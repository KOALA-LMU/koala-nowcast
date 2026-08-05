clean_party_label <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("\\*+", "", x)

  x <- gsub("\\^[0-9]+", "", x)
  x <- gsub("[¹²³⁴⁵⁶⁷⁸⁹⁰]+", "", x)
  x <- gsub("\\[[0-9]+\\]", "", x)
  x <- gsub("\\([0-9]+\\)", "", x)

  x <- gsub("’", "'", x, fixed = TRUE)
  x <- gsub("`", "'", x, fixed = TRUE)

  x <- gsub("\\.+$", "", x)

  x <- gsub("\\s+", " ", x)

  trimws(x)
}

party_lookup <- function() {
  c(
    "CDU/CSU" = "cdu",
    "CDU" = "cdu",

    "SPD" = "spd",

    "GRÜNE" = "greens",
    "B'90/GRÜNE" = "greens",
    "BÜNDNIS 90/DIE GRÜNEN" = "greens",


    "FDP" = "fdp",
    "DIE LINKE" = "left",
    "Die Linke" = "left",
    "PDS/DIE LINKE" = "left",
    "PDS/Die Linke" = "left",

    "AfD" = "afd",

    "BSW" = "bsw",

    "Sonstige" = "others"
  )
}

scrape_election_results <- function(
  url = "https://www.wahlrecht.de/ergebnisse/bundestag.htm",
  election_year = 2025
) {
  page <- rvest::read_html(url)
  table <- rvest::html_table(page, fill = TRUE)[[2]]
  
  year_cols <- grep(paste0("^", election_year), colnames(table))
  percent_col <- year_cols[[1]]
  seats_col <- year_cols[[2]]

  table <- table[, c(1, percent_col, seats_col)]
  colnames(table) <- c("label", "percent", "seats")
  table <- table[-c(1, 2), ]

  table$label <- clean_party_label(table$label)
  lookup <- party_lookup()
  table$party <- ifelse(table$label %in% names(lookup),
    unname(lookup[table$label]),
    "others"
  )
  
  table$percent <- gsub(",", ".", table$percent, fixed = TRUE)
  table$percent <- as.numeric(ifelse(table$percent == "–", "0", table$percent))
  table$seats <- as.numeric(ifelse(table$seats == "–", "0", table$seats))
  table$sum_seats <- sum(table$seats)

  res <- table |>
    dplyr::group_by(party) |>
    summarise(
      label = dplyr::first(ifelse(party == "others", "Sonstige", label)),
      percent = sum(percent, na.rm = TRUE),
      seats = sum(seats, na.rm = TRUE),
      sum_seats = dplyr::first(sum_seats),
      .groups = "drop"
    ) |>
    dplyr::select(label, party, percent, seats, sum_seats)


  if (!("bsw" %in% res$party)) {
    res <- rbind(
      res,
      tibble::tibble(label = "BSW", party = "bsw", percent = 0.0,
      seats = 0, sum_seats = res$sum_seats[[1]])
    )
  }

  return(res)
}

# urls <- c(
#   "https://www.wahlrecht.de/ergebnisse/bundestag.htm",
#   "https://www.wahlrecht.de/ergebnisse/berlin.htm",
#   "https://www.wahlrecht.de/ergebnisse/mecklenburg.htm",
#   "https://www.wahlrecht.de/ergebnisse/sachsen-anhalt.htm"
# )

# election_years <- c(2025, 2023, 2021, 2021)

# results <- list()

# for (i in seq_along(urls)) {
#   data <- tryCatch(scrape_election_results(urls[[i]], election_years[[i]]),
#   error = function(e) e)
#   results[[length(results) + 1]] <- data
# }
# results
