## Purpose: contains R-functions that form the main engine of the trial simulator
## Authors: Ron Keizer (ron.keizer@ucsf.edu), Rada Savic

## Notes: perhaps some of this should be implemented in C++, to speed up computation

require(plyr)

tte_trial_design <- function (
    n_patients = 12000,
    n_arms = 4,
    enrollment_design = list(),
    max_individual_length = 18*30,                  # n days
    max_trial_length = 180,                         # n days
    visits = c(0, 30, 90, 120, 150, 180),           # visit times (days)
    patient_design = list()
) {
    tte_design <- list()
    return(tte_design)
}

tte_enrollment_design <- function ( ## Create an object describing the enrollment
    n_patients = 12000,
    n_arms = 4,
    control_arm = 1,                                # which arm is the control
    enrollment_rate  = c(.5, .5, .5, .5),           # describe enrollment rates in all arms (per day)
    max_trial_length = 180                          # n days
  ) { 
    enrollment <- list()
    return(enrollment)
}

tte_patient_design <- function ( ## Create an object describing the patient
  hazard_event = 0.035,  # per year
  hazard_dropout = 0.1,           # per year
  hazard_switch = 0.1)             # per year, assuming switch is random to another arm
) { 
  enrollment <- list()
  return(enrollment)
}
  
tte_sim_enrollment <- function (design) { # simulate enrollment, based on enrollment design
  sim_e <- list()
  return(sim_e)  
}

tte_run_trial <- function (design) { # run a single trial, based on trial design
  res <- list()
  return(res)
}

