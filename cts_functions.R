## Purpose: contains R-functions that form the main engine of the trial simulator
## Authors: Ron Keizer (ron.keizer@ucsf.edu), Rada Savic

## Notes: perhaps some of this should be implemented in C++, to speed up computation

require("plyr")
require("survival")
require("iterators")
require("foreach")
require("doMC")
registerDoMC(4)
require("Rcpp")
sourceCpp("../tte_sim_patient.cpp")

###########################################################################
## Functions to define enrollmenet, patients, trials, power analysis
###########################################################################

tte_patient_design <- function (arm_start = 1, 
                                binary_covariates = NULL) { ## Create an object describing the patient
  patient <- list("arm_start" = arm_start,
                  "binary_covariates" = binary_covariates)
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
apply_covariate_effects <- function(patient_design) { 
  cov_effects <- c(1,1,1)  
  if (!is.null(patient_design$binary_covariates)) {
    cov_names <- names(patient_design$binary_covariates)
    for (i in seq(cov_names)) {
      curr_cov <-  patient_design$binary_covariates[[cov_names[i]]] 
      if (runif(1) < curr_cov$prob) {
        cov_effects <- cov_effects * c(curr_cov$rr_hazard_event, curr_cov$rr_hazard_dropout, curr_cov$rr_hazard_switch) 
        sim_p[[paste("cov_",cov_names[i],sep="")]] <- 1
      } else {
        sim_p[[paste("cov_",cov_names[i],sep="")]] <- 0
      }
    }
  }
  return(cov_effects)
}  

sim_patient_core_R <- function (sim_p, haz_table, arm_current, cov_effects) {
  switched <- 0 
  arms <- 1:length(haz_table[,1])
  pat_haz <- unlist(haz_table[arm_current,] )
  pat_haz_eff <- pat_haz * cov_effects
  for (i in 2:length(sim_p$time)) { # can't do apply since hazard might change over time
    dtime <- sim_p$time[i] - sim_p$time[i-1]
    sim_p[i,2:4] <- (runif(3) < c(1-1*exp(-pat_haz_eff * dtime/365)))*1    
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
        cov_effects <- c(1,1,1)
        pat_haz <- unlist(haz_table[arm_current,] )
        pat_haz_eff <- pat_haz * cov_effects
        #sim_p[i,5] <- arm_current
        switched <- 1
      }
    }
  }
  return (sim_p)
}

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
  haz_table <- matrix(nrow=length(arms), ncol=3)
  for (i in seq(names(trial_design$arm_design))) {
    haz_table[i,1:3] <- unlist(trial_design$arm_design[[names(trial_design$arm_design)[i]]][1:3])
  }
    
  ## this part ported to C++ 
  c_names <- colnames(sim_p)
  sim_p <- sim_patient_core_R (sim_p, 
                               haz_table = haz_table, 
                               arm_current = patient_design$arm_start, 
                               cov_effects = apply_covariate_effects(patient_design))
#    sim_p <- sim_patient_core_Cpp (as.matrix(sim_p), 
#                                  sim_p$time,
#                                  haz_table = haz_table,
#                                  arm_current = as.integer(patient_design$arm_start), 
#                                  n_arms = as.integer(length(arms)),
#                                  cov_effects = apply_covariate_effects(patient_design))
  ## /C++
  colnames(sim_p) <- c_names
  sim_p <- data.frame(sim_p)
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
  dat <- foreach (i=seq(arm_names), .combine=rbind) %dopar% {
    n_pat <- arm_design[[arm_names[i]]]$n_patients
    n_visits <- length(trial_design$visits)
    tmp <- data.frame(matrix(ncol=8, nrow=n_pat*n_visits)) # creating matrix beforehand should be faster than doing repeated rbind
    idx <- 1
    for (j in 1:arm_design[[arm_names[i]]]$n_patients) {  
        tmp2 <- cbind(arm_name=arm_names[i], patient=j, 
                         tte_sim_patient (patient_design = arm_design[[arm_names[i]]]$patient_design,  
                                          trial_design = trial_design,  
                                          offset = enrollment[[arm_names[i]]][j]) 
                   )
        l <- length(tmp2[,1])
        tmp[c(idx:(idx+l-1)), ] <- tmp2
        idx <- idx+l
    }
    tmp <- data.frame(tmp)
    colnames(tmp) <- c("arm_name","patient","time", "event", "dropout", "switch", "arm", "offset")
    tmp$arm_name <- arm_names[i]
    return(tmp)
  }
  dat <- dat[!is.na(dat$time),]  
  
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
                                until_time = NULL,
                                control_arm = 1) {
  if (is.null(until_time)) {
    until_time <- max(sim_data$time)
  }
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
  sim_data <- sim_data[sim_data$time <= until_time,]
  sim_data$grp <- paste(sim_data$arm_name, sim_data$patient, sep="")
  sim_data$time <- sim_data$time - sim_data$offset
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

