RESULT_FILES <- c("coalProbs", "coalProbs_grouping", "biggestParty", "passHurdle", "shares")

#' Newest date per pollster, as a named Date vector
#' @noRd
newest_per_pollster <- function(dates, pollsters) {
  x <- tapply(as.numeric(as.Date(dates)), pollsters, max)  # tapply drops the Date class
  stats::setNames(as.Date(x, origin = "1970-01-01"), names(x))
}

#' Pollster/date pairs of a result file, read without parsing it
#'
#' shares.json carries one column per simulation draw, so fromJSON() needs
#' minutes on it; every record starts with these two fields, so scanning for
#' that prefix answers the same question in about a second.
#' @noRd
result_pairs <- function(path) {
  txt <- readChar(path, file.size(path), useBytes = TRUE)
  m   <- regmatches(txt, gregexpr('"pollster":"[^"]*","date":"[0-9]{4}-[0-9]{2}-[0-9]{2}"',
                                  txt, useBytes = TRUE))[[1]]
  data.frame(pollster = sub('^"pollster":"([^"]*).*', "\\1", m),
             date     = as.Date(substr(m, nchar(m) - 10, nchar(m) - 1)))
}

#' Check whether an election still needs (re-)computation
#'
#' An election is pending if it has an explicit \code{pending_dates.json}
#' (written by \code{\link{scrape_election}}), if it has surveys but no results
#' yet, or if any result file falls short of what was scraped.
#'
#' All result files are checked, not only \code{coalProbs.json}: the upload
#' syncs them one at a time, so a run that dies partway — or that loses a single
#' file, as a failed multipart upload of the large \code{shares.json} does —
#' leaves some current and the rest stale.
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

  if (file.exists(pending_file)) return(TRUE)
  if (!file.exists(survey_file)) return(FALSE)  # nothing scraped yet, nothing to compute

  surveys <- jsonlite::fromJSON(survey_file)
  dates   <- unique(as.Date(surveys$date))
  newest  <- newest_per_pollster(surveys$date, surveys$pollster)

  paths  <- file.path("data", "results", id, paste0(RESULT_FILES, ".json"))
  behind <- vapply(paths, function(p) {
    if (!file.exists(p)) return(TRUE)
    got <- result_pairs(p)
    if (nrow(got) == 0) return(FALSE)  # empty analysis, or a layout we cannot read
    have <- newest_per_pollster(got$date, got$pollster)[names(newest)]
    if (any(is.na(have) | have < newest)) return(TRUE)
    # shares.json keeps only each pollster's newest date, the rest a full history
    !grepl("shares", p) && any(!dates %in% got$date)
  }, logical(1))

  if (any(behind))
    message(sprintf("[%s] behind the scraped polls: %s", id,
                    paste(basename(paths[behind]), collapse = ", ")))
  any(behind)
}

#' Get the election configs that need (re-)computation
#'
#' @param configs character vector of paths to election YAML config files
#' @param force_all if TRUE, return all configs regardless of pending state
#' @export
configs_todo <- function(configs, force_all = FALSE) {
  if (force_all) configs else Filter(has_pending, configs)
}
