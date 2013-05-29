## Purpose: contains R-functions that form the main engine of the trial simulator
## Authors: Ron Keizer (ron.keizer@ucsf.edu), Rada Savic

## Notes: perhaps some of this should be implemented in C++, to speed up computation

require("plyr")
require("survival")
require("iterators")
require("foreach")
require("doMC")
registerDoMC(4)

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
    "hazard_event" = hazard_event,
    "hazard_dropout"= hazard_dropout,
    "hazard_switch" = hazard_switch,
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

tte_trial_design <- function (arm_design = list(),
                              control_arm = 1, # which arm acts as control?
                              enrollment_design = list(),
                              visits = c(0, 30, 90, 120, 150, 180), # visit times (days)
                              max_events = NULL)          
  {
    tte_design <- list(
      "arm_design" = arm_design,
      "control_arm" = control_arm,
      "enrollment_design" = enrollment_design,
      "visits" = visits,
      "max_events" = max_events
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
  sim_p <- data.frame(cbind("time" = trial_design$visits, "event" = 0, "dropout" = 0, "switch" = 0, "arm" = patient_design$arm_start, "offset"=offset)) 
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
    sim_p[i,2:4] <- (runif(3) < c(1-1*exp(-pat_haz * dtime/365)))*1    
    sim_p[i,5] <- arm_current
    if (sum(sim_p[i,2:4]) > 0) { # something happened
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
  }
  sim_p$time <- sim_p$time + offset
  return(sim_p)  
}

## Create an object describing the enrollment
tte_sim_enrollment <- function (arm_design = list(),
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

tte_sim_trial <- function (trial_design) {
  # simulate the enrollment of patients
  enrollment <- tte_sim_enrollment (trial_design$arm_design, trial_design$enrollment_design)
  
  # Simulate the trial
  arm_names <- names(trial_design$arm_design)
  dat <- c()
  for (i in seq(arm_names)) {
    for (j in 1:arm_design[[arm_names[i]]]$n_patients) {    
#     dat <- foreach (j=1:arm_design[[arm_names[i]]]$n_patients, .combine=rbind) %dopar% {
      dat <- rbind(dat, 
#        return(
                   cbind(arm_name=arm_names[i], patient=j, 
                         tte_sim_patient (patient_design = arm_design[[arm_names[i]]]$patient_design,  
                                          trial_design = trial_design,  
                                          offset = enrollment[[arm_names[i]]][j] ) 
                   ))
    }
  }
  # remove patient enrolled after the max_events were reached:
  if (!is.null(trial_design$max_events)) {
    dat <- apply_stopping_criterion(dat, trial_design$max_events)
  }
  return(dat)
}

apply_stopping_criterion <- function (sim_data, max_events = 572) {
  event_times <- sim_data[sim_data$event == 1,]$time
  if (max_events < length(event_times)) {
    max_event_time <- event_times[order(event_times)][max_events]
    #sim_data <- sim_data[sim_data$offset < max_event_time,]
    sim_data <- sim_data[sim_data$time < max_event_time,] # throw away all data after the nth event has been reached
  }
  return(sim_data)
}

extract_event_data <- function (sim_data, 
                                until_time = 365*2,
                                control_arm = 1) {
  get_event_time <- function (d) {
    event <- 0 # censored
    last <- d[d$time == max(d$time),] 
    arm <- d[1,]$arm # intention-to-treat, don't use last arm!
    time <- last$time
    if (last$event == 1) {
      event <- 1
      prev <- tail(d[d$time < max(d$time),],1) 
      time <- mean(c(prev$time,last$time))
    }
    return(
      c(time, event, arm)
    )
  }
  time_after_start <- function(d) {
    d$time <- d$time - min(d$time)
    return(d)
  }
  sim_data <- sim_data[sim_data$time <= until_time,]
  sim_data$grp <- paste(sim_data$arm_name, sim_data$patient, sep="")
  sim_data <- ddply (sim_data, "grp", time_after_start)
  event_dat <- ddply (sim_data, "grp", get_event_time)
  colnames(event_dat) <- c("patient", "time","event","arm")
  event_dat$arm <- factor(event_dat$arm)
  event_dat$arm_type <- "test"
  event_dat[event_dat$arm==control_arm,]$arm_type <- "control"
  return(event_dat)  
}

sum_events <- function(d) { sum (d$event[d$event==1]) }
sum_dropout <- function(d) { sum (d$dropout[d$dropout==1]) }
as.num <- function(x) {as.numeric(as.character(x))}

