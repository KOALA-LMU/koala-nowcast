suppressPackageStartupMessages({
  library("parallel")
})

#' Compute probabilities for all theoretically possible coalitions
#'
#' Function which computes the coalition probabilities for all possible combinations
#' of parties in the parties vector. Also the function checks for each simulation if
#' a coalition has a majority AND no subset coalition of it already has a majority.
#' Workflow of the function:
#' 1) At first the function checks for a possible majority of a single party
#' 2) Then the function checks for possible majorities of all 2-party-coalitions
#'    (only for those coalitions where none of the included parties already has a majority on its own)
#' 3) Then the function checks for possible majorities of all 3-party-coalitions
#'    (only for those coalitions where no included 2-party-coalition already has a majority on its own)
#' 4) so on...
#' 
#' @param seat.distributions seat distributions list as returned by coalitions::get_seat_distributions
#' @param parties char vector of all parties
#' @param shares_sim simulated shares of all possible coalitions (including the single parties!)
#' not converted on seats in the parliament. Is only used to check for the biggest party in the
#' case that the parties in \code{strongest_party_coals} have exactly the same number of redistributed seats in parliament
#' @param strongest_party_coals Optional vector of the form \code{c("cdu|spd","spd|cdu")}. For all coalitions
#' not specified here the ordering of the parties in the coalition is not taken into account. But for all
#' values specified here the coalition is only counted as possible it the first party from the coalition
#' is the leading one. E.g. \code{"cdu|spd"} is only counted as a possible coalition if the coalition
#' both has a majority and cdu is the leading party in the coalition.
#' @param cores number of cores to use for parallel processing. Possible for both Linux-based systems and Windows.
#' @param cluster On Windows, an optional pre-built PSOCK cluster (as returned by
#' \code{parallel::makePSOCKcluster()}) to reuse instead of spawning a fresh one for this call.
#' Passing one in avoids repeatedly starting/stopping \code{cores} full R processes when this
#' function is called many times in a loop (e.g. once per survey date); if \code{NULL}, a cluster
#' is created and torn down for this call only, as before.
#' @return \code{list} containing the coalition probabilities, the shares of each coalition in each simulation
#'                     and for each simulation if the coalition has a majority while no subset coalition already has a majority.
#' @import parallel
#' @export
calc_allCoalProbs <- function(seat.distributions, parties, shares_sim, strongest_party_coals = NULL, cores = 1, cluster = NULL) {
  norm_coal <- function(c) paste(sort(strsplit(c, "\\|")[[1]]), collapse = "|")
  nsim <- max(as.numeric(seat.distributions$sim))
  nseats <- sum(seat.distributions$seats[seat.distributions$sim == 1])
  ### define all possible combinations of parties
  coalitions <- lapply(parties, function(p) combn(parties, m = match(p, parties)))
  # for the coalitions in 'strongest_party_coals' every setting with a different leading party is looked at
  if (!is.null(strongest_party_coals)) {
    # only loop through one of the multiple equal (equal if ignoring the ordering) strongest_party_coals
    spc_vec <- strongest_party_coals
    spc_secondary_index <- if (length(spc_vec) == 1) {
      FALSE
    } else {
      c(FALSE, sapply(seq(2, length(spc_vec)), function(i)
        norm_coal(spc_vec[i]) %in% sapply(spc_vec[seq_len(i - 1)], norm_coal)))
    }
    spc_primary <- spc_vec[!spc_secondary_index]
    for (spc in spc_primary) {
      p <- strsplit(spc, "\\|")[[1]]
      index_already_in_coalitions <- which.min(match(p, parties))
      coal_size <- length(p)
      for (i in 1:coal_size) {
        if (i != index_already_in_coalitions) {
          p_vector <- c(p[i], p[-i])
          coalitions[[coal_size]] <- cbind(coalitions[[coal_size]], matrix(p_vector, nrow = coal_size, ncol = 1))
        }
      }
    }
  }
  coal_names <- unlist(sapply(coalitions, function(x) apply(x, 2, function(y) paste0(y, collapse = "|"))))

  res_maj <- data.frame("coalition" = coal_names,
                        "coal_size" = unlist(sapply(coalitions, function(x) rep(nrow(x), times = ncol(x)))),
                        "coal_prob" = -1, # fill matrix with -1's which will be filled
                        stringsAsFactors = FALSE) %>%
    bind_cols(as.data.frame(matrix(nrow = length(coal_names), ncol = nsim, NA, dimnames = list(1:length(coal_names), paste0("coal_maj",1:nsim)))))
  res_shares <- data.frame("coalition" = coal_names, stringsAsFactors = FALSE) %>%
    bind_cols(as.data.frame(matrix(nrow = length(coal_names), ncol = nsim, NA, dimnames = list(1:length(coal_names), paste0("coal_share",1:nsim)))))
  shares <- lapply(coal_names, function(coal) {
    sh <- seat.distributions[seat.distributions$party %in% strsplit(coal, "\\|")[[1]],]
    if (!all((1:nsim) %in% sh$sim))
      sh <- sh %>% bind_rows(data.frame("sim" = (1:nsim)[!((1:nsim) %in% sh$sim)],
                                        "party" = sh$party[1], "seats" = 0,
                                        stringsAsFactors = FALSE)) %>% arrange(sim)
    sh %>% group_by(sim) %>% summarize(share = sum(seats) / nseats) %>% pull(share)
  })
  names(shares) <- coal_names
  res_shares[,grepl("share", colnames(res_shares))] <- do.call("rbind", shares)

    ### Helper function to calculate coalition majorities (possible using parallelization)
  calc_oneCoal <- function(coal) {
    # 1) extract the 0/1 vector of possible majorities for the coalition
    maj <- as.data.frame(t(res_shares[res_shares$coalition == coal,names(res_shares) != "coalition"]))
    colnames(maj) <- "V1"
    maj <- as.numeric(maj$V1 > 0.5)
    # 2) check if subset coalitions (or single parties) don't already have majorities on their own
    party_vector <- strsplit(coal, split = "\\|")[[1]]
    criterion_1 <- res_maj$coal_size < res_maj$coal_size[res_maj$coalition == coal]
    criterion_2 <- sapply(res_maj$coalition, function(x) {
      p <- strsplit(x, split = "\\|")[[1]]
      all(p %in% party_vector)
    }, USE.NAMES = FALSE)
    subsetCoals <- res_maj$coalition[criterion_1 & criterion_2]
    dat <- as.matrix(res_shares[res_shares$coalition %in% subsetCoals,names(res_shares) != "coalition"]) # The following operations are way faster on a numerical matrix than on a data.frame
    dat <- (dat > 0.5)
    aSubsetCoalitionIsPossible <- data.frame("maj" = apply(dat, 2, function(x) as.numeric(any(x)))) %>% pull(maj)
    # 3) when a subset coalition has a majority the bigger coalition is not counted as possible
    maj[aSubsetCoalitionIsPossible == 1] <- 0
    # 3.1) special case: strongest_party_coals are only possible depending on the leading party in the coalition
    if (!is.null(strongest_party_coals)) {
      if (coal %in% strongest_party_coals) {
        shares_list <- lapply(party_vector, function(x) shares_sim[,x])
        shares_dat <- dplyr::bind_cols(shares_list)
        shares_max_ind <- apply(shares_dat, 1, which.max)
        if (any(shares_max_ind != 1))
          maj[which(shares_max_ind != 1)] <- 0
      }
    }
    return(maj)
  }
  
  ### (Parallel) calculation over all coalitions
  if (cores == 1) { # no parallel call
    majorities <- lapply(coal_names, function(coal) { calc_oneCoal(coal) })
  } else if (Sys.info()["sysname"] != "Windows") { # parallel call for Linux-based systems
    majorities <- mclapply(coal_names, function(coal) { calc_oneCoal(coal) }, mc.cores = cores)
  } else { # parallel call for Windows
    # Reuse a caller-supplied cluster when given. Spawning + tearing down a fresh
    # PSOCK cluster of `cores` full R processes (each reloading dplyr/tidyr/coalitions
    # from scratch) on every call — as this used to do unconditionally — is cheap once,
    # but ruinous when called once per survey date across a multi-year history.
    owns_cluster <- is.null(cluster)
    if (owns_cluster) {
      cluster <- makePSOCKcluster(rep("localhost", cores)) # create cluster
      clusterEvalQ(cl = cluster, c(library(parallel), library(coalitions), library(dplyr), library(tidyr)))
    }
    # Export objects to the cluster
    clusterExport(cl = cluster, c("calc_oneCoal","res_shares","res_maj"), envir=environment())
    majorities <- parLapply(cl = cluster, X = coal_names,
                         fun = function(coal) { calc_oneCoal(coal) })
    if (owns_cluster) stopCluster(cl = cluster) # close cluster only if we created it
  }
  
  ### Preparating and returning results
  res_maj[,grepl("maj", colnames(res_maj))] <- do.call("rbind", majorities)
  # compute coalition probabilities
  res_maj$coal_prob <- rowSums(res_maj[,grepl("coal_maj",colnames(res_maj))]) / nsim
  return(list("coalProbs" = res_maj,
              "shares_perSimulation" = res_shares))
}

#' Dynamically derive which coalitions to expose, based on current pooled vote shares
#'
#' Enumerates every party subset up to \code{max_size}, keeps only those whose
#' combined pooled vote share clears \code{min_combined_pct}, and for each
#' surviving subset exposes one "who leads" ordering per member party whose own
#' pooled vote share is at least \code{leader_flip_ratio} of the subset's
#' biggest-polling member (the top member itself always qualifies trivially).
#' Used in place of a hand-authored \code{coalitions:} YAML list so the exposed
#' set tracks current polling instead of a fixed snapshot.
#'
#' @param parties_cfg the \code{parties} list from an election YAML (list of list(id=,label=,color=,...))
#' @param pooled_shares named numeric vector: party id -> current pooled vote share (percent)
#' @param max_size maximum coalition size to consider
#' @param min_combined_pct minimum combined pooled vote share (in percent) for a coalition to be computed at all.
#' Deliberately looser than the dashboard's own display threshold (33%, see leadershipVariants() in
#' dashboard/index.qmd) so the underlying data stays available even for combinations the site currently hides.
#' @param leader_flip_ratio minimum ratio (candidate leader's share / subset's top share) to expose that leader ordering
#' @export
derive_dynamic_coalitions <- function(parties_cfg, pooled_shares, max_size = 4,
                                       min_combined_pct = 25, leader_flip_ratio = 0.5) {
  ids    <- sapply(parties_cfg, `[[`, "id")
  labels <- setNames(sapply(parties_cfg, `[[`, "label"), ids)
  colors <- setNames(sapply(parties_cfg, `[[`, "color"), ids)
  ids    <- ids[ids != "others"]

  share_of <- function(p) {
    v <- pooled_shares[[p]]
    if (is.null(v) || is.na(v)) 0 else v
  }

  out <- list()
  for (size in seq_len(min(max_size, length(ids)))) {
    for (combo in combn(ids, size, simplify = FALSE)) {
      combined_pct <- sum(vapply(combo, share_of, numeric(1)))
      if (combined_pct < min_combined_pct) next

      if (size == 1) {
        p <- combo[[1]]
        out[[length(out) + 1]] <- list(parties = list(p), label = labels[[p]], color = colors[[p]])
        next
      }

      top_pct <- max(vapply(combo, share_of, numeric(1)))
      for (leader in combo) {
        if (share_of(leader) < leader_flip_ratio * top_pct) next
        ordering <- c(leader, combo[combo != leader])
        out[[length(out) + 1]] <- list(
          parties = as.list(ordering),
          label   = paste(labels[ordering], collapse = "-"),
          color   = colors[[leader]]
        )
      }
    }
  }
  out
}
