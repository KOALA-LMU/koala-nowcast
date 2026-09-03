#!/usr/bin/env Rscript
# Test entry point. Run from the project root:  Rscript tests/run-tests.R
#
# The pipeline is a set of scripts rather than a package, so the sources are
# loaded here (from the project root, because they source each other by relative
# path) and handed to test_dir(), which then runs with the test directory as its
# working directory.
#
# stop_on_failure = TRUE is what makes this usable in CI: it exits non-zero when
# anything fails.

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

if (!file.exists("scripts/calc_coalProbs_helpers.R"))
  stop("Run from the project root: Rscript tests/run-tests.R")

env <- new.env(parent = globalenv())
sys.source("scripts/calc_coalProbs_helpers.R", envir = env)
sys.source("scripts/pending_configs.R",        envir = env)
sys.source("scripts/scrape_polls.R",           envir = env)

testthat::test_dir("tests/testthat", env = env, stop_on_failure = TRUE)
