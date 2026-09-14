
blog_read_config <- function(config_path) {
  if (!file.exists(config_path)) {
    stop("Election config not found: ", config_path)
  }

  yaml::yaml.load(paste(
    readLines(config_path, encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  ))
}

blog_clean_label <- function(x) {
  x <- gsub("&shy;", "", as.character(x), fixed = TRUE)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

blog_party_key <- function(x) {
  x <- iconv(blog_clean_label(x), from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  gsub("[^A-Z0-9]+", "", x)
}

blog_parse_decimal <- function(x) {
  suppressWarnings(as.numeric(sub(",", ".", trimws(x), fixed = TRUE)))
}

# Result and poll preparation ----------------------------------------------

blog_validate_result <- function(result, party_order, expected_seats = NULL,
                                 total_tolerance = 0.2) {
  required_columns <- c("party", "percent", "seats")
  missing_columns <- setdiff(required_columns, names(result))
  if (length(missing_columns) > 0) {
    stop("Election result is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  if (!is.numeric(result$percent) || !is.numeric(result$seats) ||
    any(!is.finite(result$percent)) || any(!is.finite(result$seats))) {
    stop("Election-result percentages and seats must be finite numeric values.")
  }
  if (any(result$percent < 0 | result$percent > 100) ||
    any(result$seats < 0 | result$seats != round(result$seats))) {
    stop("Election-result percentages or seats are outside their valid range.")
  }
  if (anyDuplicated(result$party)) {
    stop("Election result contains duplicate party categories.")
  }
  if (!setequal(result$party, party_order)) {
    missing <- setdiff(party_order, result$party)
    extra <- setdiff(result$party, party_order)
    stop(
      "Election-result categories do not match the requested categories. ",
      "Missing: ", paste(missing, collapse = ", "), "; extra: ",
      paste(extra, collapse = ", "), "."
    )
  }
  if (abs(sum(result$percent) - 100) > total_tolerance) {
    stop("Election-result percentages do not sum to approximately 100 percent.")
  }
  if (!is.null(expected_seats) && sum(result$seats) != expected_seats) {
    stop(
      "Election-result seats sum to ", sum(result$seats),
      ", expected ", expected_seats, "."
    )
  }

  invisible(result)
}

blog_read_wahlrecht_result <- function(result_url, result_year, party_lookup,
                                       party_labels, party_order = names(party_labels),
                                       others_id = "others",
                                       excluded_source_rows = "Wahlbeteiligung",
                                       expected_seats = NULL,
                                       total_tolerance = 0.2) {
  tables <- rvest::read_html(result_url) |>
    rvest::html_table(fill = TRUE)

  year_matches <- function(table) {
    normalized_names <- gsub("[^0-9]", "", names(table))
    sum(normalized_names == as.character(result_year)) == 2
  }
  table_index <- which(vapply(tables, year_matches, logical(1)))

  if (length(table_index) != 1) {
    stop(
      "Expected exactly one Wahlrecht result table with two columns for ",
      result_year, "."
    )
  }

  result_table <- tables[[table_index]]
  year_columns <- which(
    gsub("[^0-9]", "", names(result_table)) == as.character(result_year)
  )
  result_raw <- result_table[, c(1, year_columns)]
  names(result_raw) <- c("party_label", "percent", "seats")

  lookup <- stats::setNames(
    unname(party_lookup),
    blog_party_key(names(party_lookup))
  )
  excluded_keys <- blog_party_key(excluded_source_rows)

  result <- result_raw |>
    dplyr::transmute(
      source_party = blog_party_key(.data$party_label),
      party = unname(lookup[.data$source_party]),
      percent = blog_parse_decimal(.data$percent),
      seats = blog_parse_decimal(.data$seats)
    ) |>
    dplyr::filter(!(.data$source_party %in% excluded_keys), !is.na(.data$percent)) |>
    dplyr::mutate(
      party = dplyr::if_else(is.na(.data$party), others_id, .data$party)
    ) |>
    dplyr::group_by(.data$party) |>
    dplyr::summarise(
      percent = sum(.data$percent),
      seats = sum(dplyr::coalesce(.data$seats, 0)),
      .groups = "drop"
    ) |>
    dplyr::mutate(label = unname(party_labels[.data$party])) |>
    dplyr::select(dplyr::all_of(c("label", "party", "percent", "seats"))) |>
    dplyr::arrange(match(.data$party, party_order))

  blog_validate_result(
    result,
    party_order = party_order,
    expected_seats = expected_seats,
    total_tolerance = total_tolerance
  )
  result
}

blog_validate_estimates <- function(estimates, party_order,
                                    group_column = "pollster",
                                    total_tolerance = 0.05) {
  required_columns <- c(group_column, "party", "percent")
  missing_columns <- setdiff(required_columns, names(estimates))
  if (length(missing_columns) > 0) {
    stop("Estimate data is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  if (!is.numeric(estimates$percent)) {
    stop("Estimate percentages must be numeric.")
  }

  groups <- split(estimates, estimates[[group_column]])
  if (length(groups) == 0) {
    stop("No estimates found.")
  }

  invalid <- vapply(groups, function(group) {
    nrow(group) != length(party_order) ||
      anyDuplicated(group$party) ||
      !setequal(group$party, party_order) ||
      any(!is.finite(group$percent)) ||
      any(group$percent < 0 | group$percent > 100) ||
      abs(sum(group$percent) - 100) > total_tolerance
  }, logical(1))

  if (any(invalid)) {
    stop(
      "Incomplete, duplicate, or off-total estimate for: ",
      paste(names(groups)[invalid], collapse = ", "), "."
    )
  }

  invisible(estimates)
}

blog_select_poll_snapshots <- function(polls_file, election_date, election_config,
                                       party_order,
                                       pooled_pollster = "pooled",
                                       expected_pooled_date = NULL,
                                       expected_poll_dates = NULL,
                                       expected_poll_count = NULL,
                                       total_tolerance = 0.05) {
  if (!file.exists(polls_file)) {
    stop("Poll file not found: ", polls_file)
  }

  election_date <- as.Date(election_date)
  polls <- jsonlite::fromJSON(polls_file) |>
    dplyr::mutate(date = as.Date(.data$date))
  polls_before_election <- polls |>
    dplyr::filter(.data$date <= election_date)

  pooled_rows <- polls_before_election |>
    dplyr::filter(.data$pollster == pooled_pollster)
  if (nrow(pooled_rows) == 0) {
    stop("No pooled estimate found on or before ", election_date, ".")
  }

  pooled_date <- max(pooled_rows$date)
  pooled_snapshot <- pooled_rows |>
    dplyr::filter(.data$date == pooled_date) |>
    dplyr::arrange(match(.data$party, party_order))

  if (!is.null(expected_pooled_date) &&
    pooled_date != as.Date(expected_pooled_date)) {
    stop(
      "Final pooled estimate is from ", pooled_date,
      ", expected ", as.Date(expected_pooled_date), "."
    )
  }

  pooling_window <- election_config$pooling$period_extended
  if (is.null(pooling_window)) {
    pooling_window <- election_config$pooling$period
  }

  configured_pollsters <- unlist(election_config$pollsters, use.names = FALSE)
  latest_polls <- polls_before_election |>
    dplyr::filter(
      .data$pollster != pooled_pollster,
      .data$pollster %in% configured_pollsters,
      .data$date >= pooled_date - pooling_window,
      .data$date <= pooled_date
    ) |>
    dplyr::group_by(.data$pollster) |>
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      dplyr::desc(.data$date),
      .data$pollster,
      match(.data$party, party_order)
    )

  blog_validate_estimates(
    latest_polls,
    party_order = party_order,
    total_tolerance = total_tolerance
  )
  blog_validate_estimates(
    pooled_snapshot,
    party_order = party_order,
    total_tolerance = total_tolerance
  )

  poll_count <- dplyr::n_distinct(latest_polls$pollster)
  if (!is.null(expected_poll_count) && poll_count != expected_poll_count) {
    stop(
      "Found ", poll_count, " institutes in the final pooling window, expected ",
      expected_poll_count, "."
    )
  }

  if (!is.null(expected_poll_dates)) {
    expected_poll_dates <- expected_poll_dates |>
      dplyr::mutate(date = as.Date(.data$date))
    actual_poll_dates <- latest_polls |>
      dplyr::distinct(.data$pollster, .data$date)
    missing_dates <- dplyr::anti_join(
      expected_poll_dates,
      actual_poll_dates,
      by = c("pollster", "date")
    )
    extra_dates <- dplyr::anti_join(
      actual_poll_dates,
      expected_poll_dates,
      by = c("pollster", "date")
    )
    if (nrow(missing_dates) > 0 || nrow(extra_dates) > 0) {
      stop("Final institute dates no longer match the blog configuration.")
    }
  }

  list(
    latest_polls = latest_polls,
    pooled_snapshot = pooled_snapshot,
    pooled_date = pooled_date
  )
}

blog_build_comparison <- function(election_result, latest_polls, pooled_snapshot,
                                  total_tolerance = 0.05) {
  comparison <- dplyr::bind_rows(latest_polls, pooled_snapshot) |>
    dplyr::left_join(
      dplyr::transmute(election_result, party = .data$party, result = .data$percent),
      by = "party"
    ) |>
    dplyr::mutate(diff = .data$percent - .data$result)

  if (anyNA(comparison$result)) {
    stop("At least one estimate category could not be matched to the result.")
  }
  blog_validate_estimates(
    comparison,
    party_order = election_result$party,
    total_tolerance = total_tolerance
  )
  comparison
}

# Metrics and simulations --------------------------------------------------

blog_calculate_error_metrics <- function(comparison) {
  comparison |>
    dplyr::group_by(.data$pollster) |>
    dplyr::summarise(
      categories = dplyr::n(),
      mae = mean(abs(.data$diff)),
      rmse = sqrt(mean(.data$diff^2)),
      mean_error = mean(.data$diff),
      .groups = "drop"
    )
}

blog_probability_summary <- function(success, simulation_draws) {
  if (length(success) != simulation_draws || anyNA(success)) {
    stop("Simulation event vector is incomplete.")
  }
  successes <- sum(success)
  probability <- successes / simulation_draws
  data.frame(
    successes = successes,
    simulations = simulation_draws,
    probability_percent = probability * 100,
    mcse_percentage_points = sqrt(probability * (1 - probability) / simulation_draws) * 100
  )
}

blog_minimal_majority <- function(seat_matrix, parties, majority_seats) {
  has_majority <- rowSums(seat_matrix[, parties, drop = FALSE]) >= majority_seats
  if (length(parties) == 1) {
    return(has_majority)
  }

  proper_subsets <- unlist(
    lapply(
      seq_len(length(parties) - 1L),
      function(size) utils::combn(parties, size, simplify = FALSE)
    ),
    recursive = FALSE
  )
  subset_has_majority <- Reduce(
    `|`,
    lapply(proper_subsets, function(subset) {
      rowSums(seat_matrix[, subset, drop = FALSE]) >= majority_seats
    })
  )
  has_majority & !subset_has_majority
}

blog_simulate_probabilities <- function(pooled_snapshot, election_config,
                                        coalition_specs = list(),
                                        simulation_draws = 10000L,
                                        simulation_seed = 1L,
                                        simulation_correction = 0.005,
                                        others_id = "others",
                                        party_order = NULL) {
  required_columns <- c("party", "percent", "votes")
  missing_columns <- setdiff(required_columns, names(pooled_snapshot))
  if (length(missing_columns) > 0) {
    stop("Pooled estimate is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  if (length(simulation_draws) != 1 || !is.finite(simulation_draws) ||
    simulation_draws < 1 || simulation_draws != round(simulation_draws)) {
    stop("simulation_draws must be a positive integer.")
  }
  if (!is.numeric(pooled_snapshot$votes) ||
    any(!is.finite(pooled_snapshot$votes)) ||
    any(pooled_snapshot$votes < 0) || sum(pooled_snapshot$votes) <= 0) {
    stop("Pooled vote counts must be finite, non-negative, and sum to a positive value.")
  }

  simulation_survey <- pooled_snapshot |>
    dplyr::select(dplyr::all_of(required_columns)) |>
    dplyr::arrange(.data$party)

  simulated_vote_shares <- coalitions::draw_from_posterior(
    survey = simulation_survey,
    nsim = simulation_draws,
    seed = simulation_seed,
    correction = simulation_correction
  )
  if (nrow(simulated_vote_shares) != simulation_draws) {
    stop(
      "Expected ", simulation_draws, " valid simulation draws, received ",
      nrow(simulated_vote_shares), "."
    )
  }

  modelled_parties <- setdiff(colnames(simulated_vote_shares), others_id)
  vote_shares_modelled <- simulated_vote_shares[, modelled_parties, drop = FALSE]
  hurdle <- election_config$parliament$hurdle

  hurdle_rows <- lapply(modelled_parties, function(party) {
    summary <- blog_probability_summary(
      vote_shares_modelled[[party]] >= hurdle,
      simulation_draws
    )
    cbind(data.frame(party = party), summary)
  })
  hurdle_probabilities <- dplyr::bind_rows(hurdle_rows)
  if (!is.null(party_order)) {
    hurdle_probabilities <- hurdle_probabilities |>
      dplyr::arrange(match(.data$party, party_order))
  }

  allocation_name <- election_config$parliament$seat_allocation
  allocation_function <- get0(
    allocation_name,
    envir = asNamespace("coalitions"),
    inherits = FALSE
  )
  if (!is.function(allocation_function)) {
    stop("Unknown seat-allocation function in election config: ", allocation_name)
  }
  seat_distributions <- coalitions::get_seats(
    dirichlet.draws = vote_shares_modelled,
    survey = dplyr::filter(simulation_survey, .data$party != others_id),
    distrib.fun = allocation_function,
    hurdle = hurdle,
    n_seats = election_config$parliament$seats
  )

  seat_matrix <- matrix(
    0,
    nrow = simulation_draws,
    ncol = length(modelled_parties),
    dimnames = list(NULL, modelled_parties)
  )
  seat_indices <- cbind(
    seat_distributions$sim,
    match(seat_distributions$party, modelled_parties)
  )
  if (anyNA(seat_indices) ||
    any(seat_indices[, 1] < 1 | seat_indices[, 1] > simulation_draws)) {
    stop("A simulated seat could not be matched to a modelled party.")
  }
  if (anyDuplicated(seat_indices)) {
    stop("Simulated seat allocation contains duplicate party rows.")
  }
  seat_matrix[seat_indices] <- seat_distributions$seats

  parliament_seats <- election_config$parliament$seats
  majority_seats <- election_config$parliament$majority
  if (any(rowSums(seat_matrix) != parliament_seats)) {
    stop("At least one simulated seat allocation does not sum to parliament size.")
  }

  if (length(coalition_specs) > 0 && is.null(names(coalition_specs))) {
    names(coalition_specs) <- paste0("coalition_", seq_along(coalition_specs))
  }
  coalition_rows <- lapply(seq_along(coalition_specs), function(index) {
    spec <- coalition_specs[[index]]
    coalition_id <- names(coalition_specs)[index]
    parties <- unlist(spec$parties, use.names = FALSE)
    if (length(parties) == 0 || !all(parties %in% modelled_parties)) {
      stop("Unknown or empty party set for coalition: ", coalition_id)
    }

    label <- if (is.null(spec$label)) coalition_id else spec$label
    has_majority <- rowSums(seat_matrix[, parties, drop = FALSE]) >= majority_seats
    majority_summary <- blog_probability_summary(has_majority, simulation_draws)
    event <- if (length(parties) == 1) {
      "absolute_seat_majority"
    } else {
      "seat_majority"
    }
    rows <- cbind(
      data.frame(
        coalition_id = coalition_id,
        coalition = label,
        event = event,
        stringsAsFactors = FALSE
      ),
      majority_summary
    )

    if (isTRUE(spec$include_minimal) && length(parties) > 1) {
      minimal <- blog_minimal_majority(seat_matrix, parties, majority_seats)
      rows <- dplyr::bind_rows(
        rows,
        cbind(
          data.frame(
            coalition_id = coalition_id,
            coalition = label,
            event = "minimal_seat_majority",
            stringsAsFactors = FALSE
          ),
          blog_probability_summary(minimal, simulation_draws)
        )
      )
    }
    rows
  })
  coalition_probabilities <- dplyr::bind_rows(coalition_rows)

  list(
    hurdle_probabilities = hurdle_probabilities,
    coalition_probabilities = coalition_probabilities,
    metadata = list(
      simulation_draws = simulation_draws,
      simulation_seed = simulation_seed,
      correction = simulation_correction,
      effective_sample_size = sum(simulation_survey$votes),
      hurdle_definition = "simulated vote share greater than or equal to hurdle",
      parliament_size = parliament_seats,
      majority_seats = majority_seats
    )
  )
}

# Static plots -------------------------------------------------------------

blog_prepare_plot_data <- function(comparison, party_order, party_labels,
                                   pollster_labels) {
  missing_parties <- setdiff(party_order, names(party_labels))
  missing_pollsters <- setdiff(unique(comparison$pollster), names(pollster_labels))
  if (length(missing_parties) > 0) {
    stop("Missing plot labels for parties: ", paste(missing_parties, collapse = ", "))
  }
  if (length(missing_pollsters) > 0) {
    stop("Missing plot labels for estimates: ", paste(missing_pollsters, collapse = ", "))
  }

  comparison |>
    dplyr::mutate(
      party = factor(.data$party, levels = party_order),
      pollster = unname(pollster_labels[.data$pollster]),
      pollster = factor(.data$pollster, levels = unname(pollster_labels))
    )
}

blog_faceted_theme <- function(base_size = 13) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 15, lineheight = 1.05),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 11
      ),
      axis.text.y = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      strip.background = ggplot2::element_rect(
        fill = "grey85",
        color = "grey40",
        linewidth = 0.7
      ),
      strip.text = ggplot2::element_text(size = 16, color = "grey15"),
      panel.border = ggplot2::element_rect(color = "grey40", linewidth = 0.7),
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.6),
      panel.grid.minor = ggplot2::element_line(color = "grey94", linewidth = 0.4),
      panel.spacing = grid::unit(0.45, "lines")
    )
}

blog_plot_estimates <- function(plot_data, party_labels, party_colors,
                                title, facet_rows = 2, y_step = 10) {
  upper <- max(y_step, ceiling(max(plot_data$percent) / y_step) * y_step)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$pollster, y = .data$percent, fill = .data$party)
  ) +
    ggplot2::geom_col(width = 0.88, alpha = 0.95) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$party),
      nrow = facet_rows,
      labeller = ggplot2::as_labeller(party_labels)
    ) +
    ggplot2::scale_fill_manual(values = party_colors, drop = FALSE) +
    ggplot2::scale_y_continuous(
      limits = c(0, upper),
      breaks = seq(0, upper, by = y_step),
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(title = title, x = "Sch\u00e4tzung", y = "Stimmenanteil") +
    blog_faceted_theme()
}

blog_plot_differences <- function(plot_data, party_labels, party_colors,
                                  title, facet_rows = 2, y_step = 5) {
  limit <- max(y_step, ceiling(max(abs(plot_data$diff)) / y_step) * y_step)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$pollster, y = .data$diff, fill = .data$party)
  ) +
    ggplot2::geom_col(width = 0.88, alpha = 0.95) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.7) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$party),
      nrow = facet_rows,
      labeller = ggplot2::as_labeller(party_labels)
    ) +
    ggplot2::scale_fill_manual(values = party_colors, drop = FALSE) +
    ggplot2::scale_y_continuous(
      limits = c(-limit, limit),
      breaks = seq(-limit, limit, by = y_step)
    ) +
    ggplot2::labs(
      title = title,
      x = "Sch\u00e4tzung",
      y = "Abweichung (Prozentpunkte)"
    ) +
    blog_faceted_theme()
}

blog_format_german <- function(x, digits) {
  sub("\\.", ",", sprintf(paste0("%.", digits, "f"), x))
}

blog_plot_result <- function(election_result, party_order, party_labels,
                             party_colors, title, y_step = 10) {
  result_data <- election_result |>
    dplyr::mutate(
      party = factor(.data$party, levels = party_order),
      value_label = paste0(blog_format_german(.data$percent, 1), " %")
    )
  upper <- max(y_step, ceiling(max(result_data$percent) / y_step) * y_step)

  ggplot2::ggplot(
    result_data,
    ggplot2::aes(x = .data$party, y = .data$percent, fill = .data$party)
  ) +
    ggplot2::geom_col(width = 0.75, alpha = 0.95) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$value_label),
      vjust = -0.45,
      size = 4
    ) +
    ggplot2::scale_fill_manual(values = party_colors) +
    ggplot2::scale_x_discrete(labels = party_labels) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, upper, by = y_step),
      limits = c(0, upper),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = title,
      x = "Partei",
      y = "Zweitstimmenanteil (Prozent)"
    ) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 15, lineheight = 1.05),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      panel.border = ggplot2::element_rect(color = "grey40", linewidth = 0.7),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey90", linewidth = 0.6),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_line(color = "grey94", linewidth = 0.4)
    )
}

blog_plot_error_metric <- function(metrics, pollster_labels, title,
                                   metric = c("mae", "rmse")) {
  metric <- match.arg(metric)
  missing_pollsters <- setdiff(metrics$pollster, names(pollster_labels))
  if (length(missing_pollsters) > 0) {
    stop("Missing error-plot labels for: ", paste(missing_pollsters, collapse = ", "))
  }

  metric_label <- if (metric == "mae") "MAE" else "RMSE"
  plot_data <- metrics
  plot_data$value <- plot_data[[metric]]
  plot_data$pollster <- unname(pollster_labels[plot_data$pollster])
  plot_data$pollster <- factor(plot_data$pollster, levels = unname(pollster_labels))
  plot_data$value_label <- blog_format_german(plot_data$value, 2)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$pollster, y = .data$value)
  ) +
    ggplot2::geom_col(fill = "darkgrey", width = 0.75) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$value_label),
      vjust = -0.45,
      size = 4
    ) +
    ggplot2::coord_cartesian(ylim = c(0, max(plot_data$value) * 1.2)) +
    ggplot2::labs(
      title = title,
      x = "Sch\u00e4tzung",
      y = paste0(metric_label, " (Prozentpunkte)")
    ) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, lineheight = 1.05),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 12
      ),
      axis.text.y = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.grid.minor = ggplot2::element_line(color = "grey95")
    )
}

blog_write_json <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(data, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}

blog_save_plot <- function(plot, path, width, height, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  invisible(path)
}

# End-to-end preparation ---------------------------------------------------

prepare_election_blog <- function(election_id, election_date, config_path,
                                  result_url, result_year, party_lookup,
                                  party_labels, party_colors, pollster_labels,
                                  blog_dir,
                                  party_order = names(party_labels),
                                  polls_file = file.path("data", "surveys", election_id, "polls.json"),
                                  evaluation_dir = file.path("data", "evaluations", election_id),
                                  result_status = "provisional",
                                  expected_result_seats = NULL,
                                  expected_pooled_date = NULL,
                                  expected_poll_dates = NULL,
                                  expected_poll_count = NULL,
                                  coalition_specs = list(),
                                  simulation_draws = 10000L,
                                  simulation_seed = 1L,
                                  simulation_correction = 0.005,
                                  others_id = "others",
                                  plot_titles = list(),
                                  facet_rows = 2,
                                  dpi = 300) {
  election_date <- as.Date(election_date)
  snapshot_id <- format(election_date, "%Y%m%d")
  election_config <- blog_read_config(config_path)
  dir.create(evaluation_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(blog_dir, recursive = TRUE, showWarnings = FALSE)

  election_result <- blog_read_wahlrecht_result(
    result_url = result_url,
    result_year = result_year,
    party_lookup = party_lookup,
    party_labels = party_labels,
    party_order = party_order,
    others_id = others_id,
    expected_seats = expected_result_seats
  )
  snapshots <- blog_select_poll_snapshots(
    polls_file = polls_file,
    election_date = election_date,
    election_config = election_config,
    party_order = party_order,
    expected_pooled_date = expected_pooled_date,
    expected_poll_dates = expected_poll_dates,
    expected_poll_count = expected_poll_count
  )
  comparison <- blog_build_comparison(
    election_result,
    snapshots$latest_polls,
    snapshots$pooled_snapshot
  )
  metrics <- blog_calculate_error_metrics(comparison)
  simulation <- blog_simulate_probabilities(
    pooled_snapshot = snapshots$pooled_snapshot,
    election_config = election_config,
    coalition_specs = coalition_specs,
    simulation_draws = simulation_draws,
    simulation_seed = simulation_seed,
    simulation_correction = simulation_correction,
    others_id = others_id,
    party_order = party_order
  )

  metadata <- list(
    election_id = election_id,
    election_date = as.character(election_date),
    poll_source = election_config$scraper$url,
    result_source = result_url,
    result_status = result_status,
    prepared_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  probability_metadata <- c(metadata, simulation$metadata)

  blog_write_json(
    list(metadata = metadata, election_result = election_result),
    file.path(evaluation_dir, paste0("election_result_", snapshot_id, ".json"))
  )
  blog_write_json(
    list(metadata = metadata, latest_polls = snapshots$latest_polls),
    file.path(evaluation_dir, paste0("latest_polls_", snapshot_id, ".json"))
  )
  blog_write_json(
    list(metadata = metadata, koala_snapshot = snapshots$pooled_snapshot),
    file.path(evaluation_dir, paste0("koala_snapshot_", snapshot_id, ".json"))
  )
  blog_write_json(
    list(
      metadata = metadata,
      errors_by_party = comparison,
      metrics_by_estimate = metrics
    ),
    file.path(evaluation_dir, paste0("comparison_metrics_", snapshot_id, ".json"))
  )
  blog_write_json(
    list(
      metadata = probability_metadata,
      coalition_probabilities = dplyr::mutate(
        simulation$coalition_probabilities,
        date = snapshots$pooled_date,
        .before = 1
      )
    ),
    file.path(evaluation_dir, paste0("coalition_probabilities_", snapshot_id, ".json"))
  )
  blog_write_json(
    list(
      metadata = probability_metadata,
      hurdle_probabilities = dplyr::mutate(
        simulation$hurdle_probabilities,
        date = snapshots$pooled_date,
        .before = 1
      )
    ),
    file.path(evaluation_dir, paste0("hurdle_probabilities_", snapshot_id, ".json"))
  )

  plot_data <- blog_prepare_plot_data(
    comparison,
    party_order = party_order,
    party_labels = party_labels,
    pollster_labels = pollster_labels
  )
  title_or <- function(name, fallback) {
    value <- plot_titles[[name]]
    if (is.null(value)) fallback else value
  }
  plots <- list(
    latest_polls = blog_plot_estimates(
      plot_data,
      party_labels,
      party_colors,
      title = title_or("latest_polls", "Letzte Wahlumfragen und KOALA-Sch\u00e4tzung"),
      facet_rows = facet_rows
    ),
    election_result = blog_plot_result(
      election_result,
      party_order,
      party_labels,
      party_colors,
      title = title_or("election_result", "Wahlergebnis")
    ),
    differences = blog_plot_differences(
      plot_data,
      party_labels,
      party_colors,
      title = title_or("differences", "Abweichung vom Wahlergebnis"),
      facet_rows = facet_rows
    ),
    mae = blog_plot_error_metric(
      metrics,
      pollster_labels,
      title = title_or("mae", "Mittlerer absoluter Fehler"),
      metric = "mae"
    )
  )

  blog_save_plot(
    plots$latest_polls,
    file.path(blog_dir, "latest_polls.png"),
    width = 9,
    height = 6.5,
    dpi = dpi
  )
  blog_save_plot(
    plots$election_result,
    file.path(blog_dir, "election_result.png"),
    width = 9,
    height = 5.5,
    dpi = dpi
  )
  blog_save_plot(
    plots$differences,
    file.path(blog_dir, "diff_vs_results.png"),
    width = 9,
    height = 6.5,
    dpi = dpi
  )
  blog_save_plot(
    plots$mae,
    file.path(blog_dir, "mae_by_pollster.png"),
    width = 7,
    height = 5,
    dpi = dpi
  )

  message("Prepared blog data and plots in ", blog_dir)
  invisible(list(
    metadata = metadata,
    election_result = election_result,
    snapshots = snapshots,
    comparison = comparison,
    metrics = metrics,
    simulation = simulation,
    plots = plots
  ))
}
