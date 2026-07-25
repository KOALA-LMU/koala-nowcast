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
  id <- yaml::read_yaml(cfg_path)$id
  pending_file <- file.path("data", "surveys", id, "pending_dates.json")
  survey_file  <- file.path("data", "surveys", id, "polls.json")
  results_file <- file.path("data", "results", id, "coalProbs.json")

  if (file.exists(pending_file)) return(TRUE)
  if (!file.exists(survey_file)) return(FALSE)  # nothing scraped yet, nothing to compute
  if (!file.exists(results_file)) return(TRUE)  # never computed, or crashed before finishing

  # Flag any survey date missing from the results, wherever it falls
  survey_dates <- unique(as.Date(jsonlite::fromJSON(survey_file)$date))
  result_dates <- unique(as.Date(jsonlite::fromJSON(results_file)$date))

  any(!survey_dates %in% result_dates)
}

#' Get the election configs that need (re-)computation
#'
#' @param configs character vector of paths to election YAML config files
#' @param force_all if TRUE, return all configs regardless of pending state
#' @export
configs_todo <- function(configs, force_all = FALSE) {
  if (force_all) configs else Filter(has_pending, configs)
}
