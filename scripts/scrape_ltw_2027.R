scrape_ltw_2027 <- function(
  address,
  parties = c("CDU", "SPD", "GRUENE", "FDP", "LINKE",
  "PIRATEN", "FW", "SSW", "BIW/BD", "AFD", "BSW", "SONSTIGE"
  ),
  ind_row_remove = -c(1:2)) {
  
  extract_num <- coalitions:::extract_num
  sanitize_befragte <- coalitions:::sanitize_befragte
  sanitize_sonstige <- coalitions:::sanitize_sonstige
  sanitize_colnames <- coalitions:::sanitize_colnames

  page <- coalitions:::try_readHTML(address)
  tables <- rvest::html_elements(page, "table")

  if (length(tables) < 2) {
    stop("No polling found at", address)
  }

  atab <- rvest::html_table(tables[[2]], fill = TRUE)
  atab <- atab[, !duplicated(names(atab)), drop = FALSE]
  
  atab <- atab[ind_row_remove, , drop = FALSE]
  atab <- atab[-nrow(atab), , drop = FALSE]
  atab <- atab[, -2, drop = FALSE]

  atab$Befragte <- sanitize_befragte(atab$Befragte)

  empty_columns <- vapply(
    atab,
    function(x) all(is.na(x) | x == ""),
    logical(1)
  )
  empty_names <- is.na(names(atab)) | names(atab) == ""
  atab <- atab[, !(empty_columns | empty_names), drop = FALSE]
  atab <- sanitize_colnames(atab)

  raw_sonstige <- atab$sonstige
  embedded_bsw <- extract_embedded_bsw(raw_sonstige)

   if ("bsw" %in% names(atab)) {
    standalone_bsw <- extract_num(atab$bsw)
    atab$bsw <- dplyr::coalesce(standalone_bsw, embedded_bsw)
  } else {
    atab$bsw <- embedded_bsw
  }

  atab$sonstige <- raw_sonstige %>%
    remove_embedded_bsw() %>%
    sanitize_sonstige() %>%
    unlist(use.names = FALSE)

  party_columns <- intersect(tolower(parties), names(atab))
  regular_parties <- setdiff(party_columns, c("bsw", "sonstige"))

  atab <- atab %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(regular_parties),
        extract_num
      ),
      befragte = extract_num(.data$befragte, decimal = FALSE),
      datum = lubridate::dmy(.data$datum),
      start = .data$datum,
      end = .data$datum
    ) %>%
    dplyr::mutate(
      total = rowSums(
        dplyr::pick(dplyr::all_of(party_columns)),
        na.rm = TRUE
      )
    ) %>%
    filter(
      .data$total == 100,
      !is.na(.data$befragte),
      !is.na(.data$datum)
    ) %>%
    dplyr::select(
      dplyr::any_of(c(
        "institut", "datum", "start", "end", party_columns, "befragte"
      ))
    ) %>%
    dplyr::rename(pollster = "institut") %>%
    dplyr::mutate(
      pollster = tolower(.data$pollster),
      pollster = dplyr::case_when(
        .data$pollster == "forschungs-gruppe wahlen" ~ "fgw",
        TRUE ~ .data$pollster
      )
    )
  
  names(atab) <- coalitions::prettify_strings(
    names(atab),
    current = coalitions:::.trans_df$german,
    new = coalitions:::.trans_df$english
  )

  atab

}

extract_embedded_bsw <- function(x) {
  match <- stringr::str_match(
    x,
    stringr::regex(
      "BSW\\s*([0-9]+(?:[,.][0-9]+)?)\\s*%?",
      ignore_case = TRUE
    )
  )[, 2]

  as.numeric(sub(",", ".", match, fixed = TRUE))
}

remove_embedded_bsw <- function(x) {
  stringr::str_remove_all(
    x,
    stringr::regex(
      "BSW\\s*[0-9]+(?:[,.][0-9]+)?\\s*%?",
      ignore_case = TRUE
    )
  )
}
