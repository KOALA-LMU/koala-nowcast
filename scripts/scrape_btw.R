suppressPackageStartupMessages({
  library("dplyr")
  library("coalitions")
})

# Bundestag polls for every institute wahlrecht publishes.
#
# coalitions::get_surveys() covers the same pages but returns them nested and
# already collapsed onto its own party set, while scrape_election() needs the
# wide table so it can collapse onto the party set from the election config.
scrape_btw <- function() {
  lookup <- coalitions:::.pollster_df
  scraped_list <- lapply(seq_len(nrow(lookup)), function(i) {
    scrape_wahlrecht(lookup$address[[i]]) %>%
      mutate(
        pollster = lookup$pollster[[i]],
        # A field period that starts after it ends spans New Year, so the start
        # date picked up the wrong year from the publication date.
        start = if_else(start > end, as.Date(start) - 365, start),
        .before = 1
      )
  })
  bind_rows(scraped_list)
}
