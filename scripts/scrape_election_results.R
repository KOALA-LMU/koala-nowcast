clean_party_label <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub(intToUtf8(0x00A0), " ", x, fixed = TRUE)
  x <- gsub("\\*+", "", x)

  x <- gsub("\\^[0-9]+", "", x)
  superscript_digits <- vapply(c(0x00B9, 0x00B2, 0x00B3, 0x2070, 0x2074:0x2079),
                               intToUtf8, character(1))
  for (digit in superscript_digits) {
    x <- gsub(digit, "", x, fixed = TRUE)
  }
  x <- gsub("\\[[0-9]+\\]", "", x)
  x <- gsub("\\([0-9]+\\)", "", x)
  x <- gsub("[0-9]+$", "", x)

  x <- gsub(intToUtf8(0x2019), "'", x, fixed = TRUE)
  x <- gsub("`", "'", x, fixed = TRUE)

  umlauts <- c(
    intToUtf8(0x00C4), intToUtf8(0x00D6), intToUtf8(0x00DC),
    intToUtf8(0x00E4), intToUtf8(0x00F6), intToUtf8(0x00FC),
    intToUtf8(0x00DF)
  )
  replacements <- c("AE", "OE", "UE", "ae", "oe", "ue", "ss")
  for (i in seq_along(umlauts)) {
    x <- gsub(umlauts[[i]], replacements[[i]], x, fixed = TRUE)
  }

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
    "GRUENE" = "greens",
    "B'90/GRÜNE" = "greens",
    "B'90/GRUENE" = "greens",
    "BÜNDNIS 90/DIE GRÜNEN" = "greens",
    "BUENDNIS 90/DIE GRUENEN" = "greens",


    "FDP" = "fdp",
    "DPS/FDP" = "fdp",
    "DIE LINKE" = "left",
    "Die Linke" = "left",
    "PDS/DIE LINKE" = "left",
    "PDS/Die Linke" = "left",

    "AfD" = "afd",

    "BSW" = "bsw",
    "SSW" = "ssw",
    "SSV/SSW" = "ssw",

    "Sonstige" = "others"
  )
}

scrape_election_results <- function(
  url = "https://www.wahlrecht.de/ergebnisse/bundestag.htm",
  election_year = 2025
) {

  # Grep the election
  splitted_url <- strsplit(url, "/", fixed = TRUE)[[1]]
  rel_part <- splitted_url[[length(splitted_url)]]
  election <- strsplit(rel_part, ".", fixed = TRUE)[[1]][[1]]


  page <- rvest::read_html(url)
  table <- rvest::html_table(page, fill = TRUE)[[2]]

  year_cols <- grep(paste0("^", election_year), colnames(table))
  if (length(year_cols) < 2) {
    first_row <- trimws(as.character(unlist(table[1, ], use.names = FALSE)))
    year_cols <- grep(paste0("^", election_year), first_row)
  }
  if (length(year_cols) < 2) {
    stop(sprintf("Could not find percent and seat columns for election year %s", election_year))
  }
  percent_col <- year_cols[[1]]
  seats_col <- year_cols[[2]]

  table <- table[, c(1, percent_col, seats_col)]
  colnames(table) <- c("label", "percent", "seats")

  table$label <- enc2utf8(clean_party_label(table$label))
  table <- table[nzchar(table$label) & !grepl("^Wahlbe", table$label), ]
  lookup <- party_lookup()
  names(lookup) <- enc2utf8(names(lookup))
  table$party <- ifelse(table$label %in% names(lookup),
    unname(lookup[table$label]),
    "others"
  )
  
  parse_number <- function(x) {
    x <- gsub(",", ".", trimws(as.character(x)), fixed = TRUE)
    x[!grepl("^[0-9]+(?:\\.[0-9]+)?$", x)] <- "0"
    as.numeric(x)
  }
  table$percent <- parse_number(table$percent)
  table$seats <- parse_number(table$seats)
  table$sum_seats <- sum(table$seats)

  res <- table |>
    dplyr::group_by(party) |>
    dplyr::summarise(
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

  if (election == "berlin") {
    res$percent[res$label == "Sonstige"] <- 9.0
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
