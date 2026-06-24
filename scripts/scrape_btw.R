library("dplyr")
library("coalitions")

scrape_btw <- function() {
  lookup <- coalitions:::.pollster_df
  scraped_list <- lapply(seq_len(nrow(lookup)), function(i) {
    scrape_wahlrecht(lookup$address[[i]]) %>%
      mutate(
      pollster = lookup$pollster[[i]],
      start = if_else(start > end, as.Date(start)- 365, start),
      .before = 1
    )
  })
  bind_rows(scraped_list)
}