require(ggplot2)

tte_plot_trial <- function ( # create plots to summarize the trial
    tte_trial,
    pdf_file = NULL
  ) { 
  pl <- list()
  
  # Plots:
  # - Kaplan-Meier with parametric CI
  pl$kaplan <- ggplot ()

  # - %events ~ time 
  pl$events_vs_time <- ggplot ()
  
  # - %event ~ time, split by arm
  pl$events_vs_time_per_arm <- ggplot ()

  # - %dropout ~ time 
  pl$dropout_vs_time <- ggplot ()
  
  # - %dropout ~ time, splity by arm
  pl$dropout_vs_time_per_arm <- ggplot ()

  # - n patients in each arm (not dropped out)
  pl$patients_vs_time <- ggplot ()
  
  if (!is.null(pdf)) {
    pdf (file = pdf_file)
    for (i in seq(pl)) {
      print (pl[i])
    }
    dev.off()
  } 
  return(pl)
}

tte_sum_trial <- function () { # create summary table for trial
  # Summary table with per visit:
  # n events
  # n dropouts
  # % remaining
  # n in each arm
  # % in each arm
  # % switched
  return(summ)
}

tte_plot_power <- function ( # create several plots to summarize the power analysis
    pdf_file = NULL
  ) {  
  pl <- list()
  # Plots:
  # - Kaplan-Meier with PI over simulations
  # - distribution of trial outcomes (% events in each trial, possibly split by visit)
  if (!is.null(pdf)) {
    pdf (file = pdf_file)
    for (i in seq(pl)) {
      print (pl[i])
    }
    dev.off()
  } 
  return(pl)
}

tte_sum_power <- function () { # create summary table for power analysis
  return(summ)
}