## Purpose     : Run tte power analyses
## Authors     : Ron Keizer (ron.keizer@ucsf.edu), Rada Savic
## Description : This script is an example of how to set up a power analysis
##               It will walk through the required steps, and create the 
##               plots and summaries.

## read in functions for cts engine and plotting
## Will eventually be made into an R-module
require(plyr)
require(survival)
source ("cts_functions.R")   # the main simulation engine
source ("power_functions.R") # the functions for the statistical test and power analysis
source ("plot_functions.R")  # plotting & summarizing functions

## patient object
pat1 <- tte_patient_design (arm_start = 1)
pat2 <- tte_patient_design (arm_start = 2)
pat3 <- tte_patient_design (arm_start = 3)
pat4 <- tte_patient_design (arm_start = 4)

## Create an object describing the enrollment
enrollment_design <- tte_enrollment_design (
  enrollment_rate  = c(10, 10, 10, 10)  # describe enrollment rates in all arms (per day)
)

arm_design <- list (
  "control" = tte_arm_design (hazard_event = 0.035, hazard_dropout = 0.1, hazard_switch = 0.1, n_patients=1000, patient_design = pat1),
  "hc1" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1, hazard_switch = 0.1, n_patients=1000, patient_design = pat2),
  "hc2" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1, hazard_switch = 0.1, n_patients=1000, patient_design = pat3),
  "hc3" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1, hazard_switch = 0.1, n_patients=1000, patient_design = pat4)
)

trial_design <- tte_trial_design (
  arm_design = arm_design,
  control_arm = 1,                                # which arm is the control
  enrollment_design = enrollment_design,
  visits = c(0, 90, 180, 270, 360, 450, 540),      # visit times (days)
  max_events = 572  # not implemented yet
)

## simulate trial
dat <- tte_sim_trial (trial_design)

## summarize 
ddply(dat, "arm", sum_events) # number of events
ddply(dat, "arm", sum_dropout) # number of dropouts

## do logrank test:
## first extract data in right form from the simulation object
event_dat <- extract_event_data(dat, until_time=540)

## then calculate the kaplan meier estimators and make a plot
fit <- survfit(Surv(time, event) ~ arm, data=event_dat)
plot(fit, 
     mark.time=TRUE, 
     conf.int=FALSE,
     xlab = 'Time (days)', 
     col= c(1,2,3,4),
     ylab = 'Seroconversion probability', 
     ylim=c(0.8,1))

## do the logrank test
survdiff(Surv(time, event) ~ arm, data=event_dat)
survdiff(Surv(time, event) ~ arm, data=event_dat[event_dat$arm %in% c(1,2),])
survdiff(Surv(time, event) ~ arm, data=event_dat[event_dat$arm %in% c(1,3),])
survdiff(Surv(time, event) ~ arm, data=event_dat[event_dat$arm %in% c(1,4),])

## Run the power analysis
pow_1 <- tte_run_power (design = trial_design, 
                        test = c("logrank"), # currently only logrank test available
                        n_sim = 100) 

