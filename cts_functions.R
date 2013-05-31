## Purpose: contains R-functions that form the main engine of the trial simulator
## Authors: Ron Keizer (ron.keizer@ucsf.edu), Rada Savic

## Notes: perhaps some of this should be implemented in C++, to speed up computation

require("plyr")
require("survival")
require("iterators")
require("foreach")
require("doMC")
registerDoMC(4)
require(Rcpp)
if (file.exists("../tte_sim_patient.cpp")) {
  sourceCpp("../tte_sim_patient.cpp")
}
if (file.exists("tte_sim_patient.cpp")) {
  sourceCpp("tte_sim_patient.cpp")
}

###########################################################################
## Functions to define enrollmenet, patients, trials, power analysis
###########################################################################

tte_patient_design <- function (arm_start = 1, binary_covariates=list()) { ## Create an object describing the patient
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

apply_covariate_effects <- function (covariate_effects) {
  eff <- c(1,1,1)
  for (i in seq(covariate_effects)) {
    if (runif(1) < covariate_effects[[i]]$prob) {
      eff <- eff * c(covariate_effects[[i]]$rr_hazard_event, covariate_effects[[i]]$rr_hazard_dropout, covariate_effects[[i]]$rr_hazard_switch)
    }
  }  
  return(eff)
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
  arm_current <- patient_design$arm_start
  haz_table <- matrix(ncol=3, nrow=length(trial_design$arm_design))
  for (i in seq(trial_design$arm_design)) {
    haz_table[i,] <- unlist(trial_design$arm_design[[i]][1:3])
  }
  cov_effects <- apply_covariate_effects(patient_design$binary_covariates)
  #col_names <- colnames(sim_p)
  sim_p <- sim_patient_core_R (sim_p, haz_table, arms, arm_current, cov_effects)
  #sim_p <- sim_patient_core_Cpp (as.matrix(sim_p), 
  #                        as.vector(trial_design$visits), 
  #                        as.matrix(haz_table), 
  #                        as.integer(arm_current), as.integer(length(arms)), as.vector(cov_effects))
  #colnames(sim_p) <- col_names
  sim_p <- data.frame(sim_p)
  sim_p$time <- sim_p$time + offset
  return(sim_p)  
}

sim_patient_core_R <- function (sim_p, haz_table, arms, arm_current, cov_effects) {
  switched <- 0 
  pat_haz <- haz_table[arm_current,]
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
        pat_haz <- haz_table[arm_current,]
        pat_haz_eff <- pat_haz * cov_effects
        switched <- 1
      }
    }
  }
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
    n_patients <- arm_design[[arm_names[i]]]$n_patients
    n_visits <- length(trial_design$visits)
    tmp <- data.frame(matrix(ncol=8, nrow=n_visits*n_patients))
    for (j in 1:n_patients) {    
        tmp2 <- cbind(arm_name=arm_names[i], patient=j, 
                         tte_sim_patient (patient_design = arm_design[[arm_names[i]]]$patient_design,  
                                          trial_design = trial_design,  
                                          offset = enrollment[[arm_names[i]]][j] ) 
                   )
        l <- length(tmp2[,1])
        idx <- (j-1)*n_visits + 1
        tmp[c(idx:(idx+l-1)), ] <- tmp2
    }
    colnames(tmp) <- c("arm_name","patient","time","event","dropout","switch","arm","offset")
    tmp <- tmp[!is.na(tmp$time),]
    tmp$arm_name <- arm_names[i]
    return(tmp)
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

