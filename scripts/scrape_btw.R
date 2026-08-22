suppressPackageStartupMessages({
  library("dplyr")
  library("rvest")
  library("stringr")
  library("lubridate")
  library("coalitions")
})

# The one page coalitions::scrape_wahlrecht() can no longer read; see below.
POLITBAROMETER_URL <- "https://www.wahlrecht.de/umfragen/politbarometer.htm"

scrape_btw <- function() {
  lookup <- coalitions:::.pollster_df
  scraped_list <- lapply(seq_len(nrow(lookup)), function(i) {
    address <- lookup$address[[i]]
    scrape <- if (identical(address, POLITBAROMETER_URL)) scrape_politbarometer else scrape_wahlrecht
    scrape(address) %>%
      mutate(
        pollster = lookup$pollster[[i]],
        start = if_else(start > end, as.Date(start) - 365, start),
        .before = 1
      )
  })
  bind_rows(scraped_list)
}

# Local replacement for coalitions::scrape_wahlrecht() on the Politbarometer page.
# Upstream reads that page's column names out of body row 2 and then drops rows
# 1-3, which was right while a stray <td> spacer in the header row stopped rvest
# from promoting it to column names. wahlrecht has since made the spacer a <th>,
# so html_table() takes the header and every body row moves up one: upstream now
# reads the party legend (one string repeated across all 14 cells) as the column
# names — dplyr aborts on the duplicates — and cuts one row too many, taking the
# most recent poll with it. The rest follows upstream, so it stays easy to diff;
# drop this function once coalitions handles the current page.
scrape_politbarometer <- function(
  address = POLITBAROMETER_URL,
  parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE", "PIRATEN", "AFD", "BSW", "SONSTIGE")
) {
  extract_num <- coalitions:::extract_num
  atab <- coalitions:::try_readHTML(address) %>%
    html_nodes("table") %>% .[[2]] %>% html_table(fill = TRUE)

  # <tfoot> precedes <tbody> in the markup, so html_table() puts the footer rows
  # (here a repeated header and the party legend) above the data. How many there
  # are differs per page and over time, so cut by content rather than by a fixed
  # offset: everything above the first row whose date column holds a date.
  first_poll <- which(grepl("^\\d{2}[.]\\d{2}[.]\\d{4}$", trimws(atab[[1]])))[1]
  if (is.na(first_poll))
    stop(sprintf("No poll rows found at %s — the table layout changed again", address))
  atab <- atab[seq(first_poll, nrow(atab) - 1L), ]  # last row is the Bundestagswahl result
  colnames(atab) <- c("Datum", colnames(atab)[-1])
  atab <- atab[, !(sapply(atab, function(z) all(z == "") | all(is.na(z))) |
                     colnames(atab) %in% c("", NA))]
  atab <- coalitions:::sanitize_colnames(atab)

  parties <- colnames(atab)[colnames(atab) %in% tolower(parties)]
  atab <- atab %>%
    mutate(
      across(all_of(parties), extract_num),
      befragte = extract_num(.data$befragte, decimal = FALSE),
      datum    = dmy(.data$datum),
      zeitraum = if_else(nchar(zeitraum) == 6, paste0(zeitraum, "-", zeitraum), zeitraum)
    ) %>%
    filter(.data$zeitraum != "Bundestagswahl", !grepl("\\?", .data$zeitraum)) %>%
    mutate(
      start = dmy(paste0(str_sub(.data$zeitraum, 1, 6), str_sub(.data$datum, 1, 4))),
      end   = dmy(paste0(str_sub(.data$zeitraum, 8, 13), str_sub(.data$datum, 1, 4))),
      end   = if_else(is.na(end), start, end)
    )

  # FW is not in `parties`, so fold its share into Sonstige or every poll
  # reporting it would fail the == 100 check below.
  if ("fw" %in% colnames(atab))
    atab <- mutate(atab, sonstige = .data$sonstige + tidyr::replace_na(extract_num(.data$fw), 0))

  atab %>%
    filter(
      rowSums(pick(all_of(parties)), na.rm = TRUE) == 100,
      !is.na(.data$befragte), !is.na(.data$datum)
    ) %>%
    distinct(datum, .keep_all = TRUE) %>%
    arrange(datum) %>%
    select(any_of(c("datum", "start", "end", parties, "befragte"))) %>%
    rename_with(~ coalitions:::prettify_strings(
      .x, current = coalitions:::.trans_df$german, new = coalitions:::.trans_df$english))
}
