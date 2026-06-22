library("dplyr")
library("coalitions")

scrape_btw <- function() {
  pollster_df <- getFromNamespace(".pollster_df", "coalitions")
  bind_rows(lapply(seq_len(nrow(pollster_df)), function(i) {
    coalitions::scrape_wahlrecht(address = pollster_df$address[i]) %>%
      mutate(
        pollster = pollster_df$pollster[i],
        start = if_else(start > end, as.Date(start) - 365, start),
        .before = 1
      )
  }))
}