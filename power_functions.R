tte_run_power <- function ( # run the power analysis, based on trial design and number of 
  design,
  test = c("log-rank"), # what tests to implement (can be multiple), currently only logrank available
  n = 1000
) { 
  if (n < 1) { 
    cat ("Set n to a valid number (i.e. >1)\n\n")
    return()
  } 
  valid_tests <- c("log-rank")
  if (sum(test %in% valid_tests)) != length(valid_tests)) {
    cat (paste("Choose a valid test, valid options are: ", valid_tests, "\n\n",sep=""))
    return()
  } 
  trial_res <- list()
  test_res <- list()
  for (i in 1:n) { # simulate n trials
    trial_res[i] <- tte_run_trial (design)
    if (tte_log)
    test_res <- c(test_res, tte_log_rank_test (trial_res[i]))
  }
  pow <- list(
    trial_res,
    test_res
  )
  return(pow)
}

tte_log_rank_test <- function (
  tte_trial    # Trial simulation  
  ) {
  test_res <- FALSE
  return(test_res)
}
