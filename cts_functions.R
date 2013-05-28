## Purpose: contains R-functions that form the main engine of the trial simulator
## Authors: Ron Keizer (ron.keizer@ucsf.edu), Rada Savic

## Notes: perhaps some of this should be implemented in C++, to speed up computation

require(plyr)

###########################################################################
## Functions to define enrollmenet, patients, trials, power analysis
###########################################################################

tte_patient_design <- function (arm_start = 1) { ## Create an object describing the patient
  patient <- list("arm_start" = arm_start)
  return(patient)
}

tte_arm_design <- function ( # hazards are defined for each arm
  hazard_event = 0.035,
  hazard_dropout = 0.1,
  hazard_switch = 0.1,
  n_patients = 3000,
  patient_design) {
  arm <- list(
    "hazard_event" = 0.035,
    "hazard_dropout"= 0.1,
    "hazard_switch" = 0.1,
    "n_patients" = n_patients,
    "patient_design" = patient_design
    )
  return(arm)  
}

# tte_site_design <- function (  # not implemented yet
#   # sites can modify the hazard defined in the arm (through de/increased relative risk ratios)
#   # use e.g. if more finegrained control is reqd.
#   site_no = 1, 
#   site_name = "no_name"
#   arm = arm_obj, 
#   rr_event = 1, 
#   rr_dropout = 1,
#   rr_switch = 1) {
#   site <- list()
#   return(site)
# }

tte_enrollment_design <- function (
  enrollment_rate  = c(4, 4, 4, 4) # enrollment rates in all arms (per day)
) {
  enrollment_design <- list(
    "enrollment_rate" = enrollment_rate
  )
  return(enrollment_design)
}

tte_sim_enrollment <- function ( ## Create an object describing the enrollment
  arm_design = list(),
  enrollment_design  = list() # enrollment rates in all arms (per day)
) { 
  rate <- enrollment_design$enrollment_rate
  offsets <- -log(1-runif(100))/rate[1]  # sample from Poisson distribution
  enrollment <- list()
  for (i in seq(arm_design)) {
    nam <- names(arm_design)[i]
    enrollment[[nam]] <- (-log(1-runif(arm_design[[nam]]$n_patients))) / rate[i]  # sample from Poisson distribution
    for (j in length(enrollment[[nam]]):2) {
      enrollment[[nam]][j] <- sum(enrollment[[nam]][1:j])
    }
  }
  return(enrollment)
}

tte_trial_design <- function (
    arm_design = list(),
    control_arm = 1, # which arm acts as control?
    enrollment_design = list(),
    max_individual_length = 18*30,                  # n days
    max_trial_length = 180,                         # n days
    visits = c(0, 30, 90, 120, 150, 180)           # visit times (days)
    ) 
  {
    tte_design <- list(
      "arm_design" = arm_design,
      "control_arm" = control_arm,
      "enrollment_design" = enrollment_design,
      "max_individual_length" = max_individual_length,
      "max_trial_length" = max_trial_length,
      "visits" = visits
      )
    return(tte_design)
}

###########################################################################
## Functions to simulate enrollmenet, patients, trials, power analysis
###########################################################################

tte_sim_patient <- function (patient_design, 
                             trial_design,
                             offset = 0
                             ) { 
  sim_p <- data.frame(cbind("time" = trial_design$visits, "event" = 0, "dropout" = 0, "switch" = 0, "arm" = patient_design$arm_start)) 
  if (length(sim_p$time) < 2) {
    cat ("Specify a trial design with at least two visits!\n\n")
    return()    
  }
  arms <- 1:length(trial_design$arm_design)
  arm_current <- patient_design$arm_start
  pat_haz <- c(unlist(trial_design$arm_design[arm_current])[1:3])
  switched <- 0 
  for (i in 2:length(sim_p$time)) { # can't do apply since hazard might change over time
    dtime <- sim_p$time[i] - sim_p$time[i-1]
    sim_p[i,2:4] <- (runif(3) < c(1-1*exp(-pat_haz * dtime/12)))*1    
    sim_p[i,5] <- arm_current
    if (sim_p[i,]$dropout == 1) {
      sim_p <- sim_p[1:i,]
      sim_p[i:length(sim_p$time), c(2,4)] <- -1
      break
    }
    if (sim_p[i,]$event == 1) {
      sim_p <- sim_p[1:i,]
      sim_p[i:length(sim_p$time), c(3,4)] <- -1
      break
    }
    if ((sim_p[i,]$switch == 1)&&(switched == 0)) {
      arm_current <- arms[-arm_current][round(runif(1)*length(arms[-arm_current])+0.5)] # switch to another arm     
      pat_haz <- c(unlist(trial_design$arm_design[arm_current])[1:3])
      switched <- 1
    }
  }
  sim_p$time <- sim_p$time + offset
  return(sim_p)  
}

tte_sim_enrollment <- function (trial_design) { # simulate enrollment, based on enrollment design. Generate offsets for patient start per trial
  sim_e <- list()
  return(sim_e)  
}

tte_sim_trial <- function (design) { # run a single trial, based on trial design
  res <- list()
  return(res)
}

tte_sim_power <- function (
  design,
  n = 1000) {  # run a power analysis, based on trial design
  pow <- list()
  return(pow)
}

