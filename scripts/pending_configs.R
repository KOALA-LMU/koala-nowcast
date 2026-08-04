#' Check whether an election still needs (re-)computation
#'
#' An election is pending if it has an explicit \code{pending_dates.json}
#' (written by \code{\link{scrape_election}}), if it has surveys but no
#' results yet, or if any survey date is missing from the saved results.
#'
#' @param cfg_path path to a YAML election config file
#' @import yaml jsonlite
#' @export
has_pending <- function(cfg_path) {
  # Same UTF-8-safe read as calc_coalProbs(): the configs carry umlauts and no
  # trailing newline, which read_yaml() reports as an invalid input connection.
  id <- yaml::yaml.load(paste(readLines(cfg_path, encoding = "UTF-8", warn = FALSE), collapse = "\n"))$id
  pending_file <- file.path("data", "surveys", id, "pending_dates.json")
  survey_file  <- file.path("data", "surveys", id, "polls.json")
  results_file <- file.path("data", "results", id, "coalProbs.json")

  if (file.exists(pending_file)) {
    message(sprintf("[%s] scraper flagged dates for recomputation", id))
    return(TRUE)
  }
  if (!file.exists(survey_file)) return(FALSE)  # nothing scraped yet, nothing to compute
  if (!file.exists(results_file)) {
    message(sprintf("[%s] no results yet", id))
    return(TRUE)  # never computed, or crashed before finishing
  }

  # Flag any survey date missing from the results, wherever it falls
  survey_dates <- unique(as.Date(jsonlite::fromJSON(survey_file)$date))
  result_dates <- unique(as.Date(jsonlite::fromJSON(results_file)$date))
  missing      <- sort(survey_dates[!survey_dates %in% result_dates])

  # Named, not just counted: when this fires it means an earlier run scraped
  # polls whose results never landed, so the dates themselves are the lead.
  if (length(missing) > 0)
    message(sprintf("[%s] %d scraped date(s) without results: %s", id, length(missing),
                    paste(c(format(head(missing, 10)),
                            if (length(missing) > 10) sprintf("(+%d more)", length(missing) - 10)),
                          collapse = ", ")))

  length(missing) > 0
}

#' Get the election configs that need (re-)computation
#'
#' @param configs character vector of paths to election YAML config files
#' @param force_all if TRUE, return all configs regardless of pending state
#' @export
configs_todo <- function(configs, force_all = FALSE) {
  if (force_all) configs else Filter(has_pending, configs)
}
