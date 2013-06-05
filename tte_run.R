## Purpose     : Run tte power analyses
## Authors     : Ron Keizer (ron.keizer@ucsf.edu), Rada Savic
## Description : This script is an example of how to set up a power analysis
##               It will walk through the required steps, and create the 
##               plots and summaries.
##               The source files will eventually be made into an R-module

source ("cts_functions.R")   # the main simulation engine
source ("power_functions.R") # the functions for the statistical test and power analysis
source ("plot_functions.R")  # plotting & summarizing functions

## patient object
pat1 <- tte_patient_design (arm_start = 1) # control
pat2 <- tte_patient_design (arm_start = 2) # hc1
pat3 <- tte_patient_design (arm_start = 3) # hc2
pat4 <- tte_patient_design (arm_start = 4) # hc3

## Create an object describing the enrollment
enrollment_design <- tte_enrollment_design ( 
  enrollment_rate  = rep( 3000/(365/2), 4)   # enrollment rates in all arms (per day); all patients enrolled in 6 months
)

arm_design <- list (
  "control" = tte_arm_design (hazard_event = 0.035, hazard_dropout = 0.1/1.5, hazard_switch = 0.1/1.5, n_patients=3000, patient_design = pat1),
  "hc1" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1/1.5, hazard_switch = 0.1/1.5, n_patients=3000, patient_design = pat2),
  "hc2" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1/1.5, hazard_switch = 0.1/1.5, n_patients=3000, patient_design = pat3),
  "hc3" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1/1.5, hazard_switch = 0.1/1.5, n_patients=3000, patient_design = pat4)
)

trial_design <- tte_trial_design (
  arm_design = arm_design,
  control_arm = 1,                                # which arm is the control
  enrollment_design = enrollment_design,
  visits = c(0, 90, 180, 270, 360, 450, 540),      # visit times (days)
  max_events = NULL  # stopping criterion, can be implemented later as well
)

## simulate trial
registerDoMC(4) 
#source("cts_functions.R")

system.time(
 dat <- tte_sim_trial (trial_design)
)
dat_stop <- apply_stopping_criterion(dat, max_events=572)

## summarize 
ddply(dat, "arm", sum_events) # number of events
ddply(dat, "arm", sum_dropout) # number of dropouts

## do logrank test:
## first extract data in right form from the simulation object
event_dat <- extract_event_data(dat)
event_dat_stop <- extract_event_data(dat_stop)

## then calculate the kaplan meier estimators and make a plot
fit <- survfit(Surv(time, event) ~ arm, data=event_dat)
gg_surv_plot (fit, arms = c("Control", "HC 1", "HC 2", "HC 3"), pct=TRUE, 
              show_ci=c(1,0,0,0)) + ylab ("% HIV negative") + ylim (c(90,100))

dat[dat$arm > 1 & dat$event==1 & dat$time < 90,]
dat[dat$arm > 1 & dat$dropout==1 & dat$time < 90,]

## Make a plot of enrollment
enr_dat <- extract_enrollment_data(dat)
fit_enr <- survfit(Surv(time, event) ~ arm, data=enr_dat)
gg_surv_plot(fit_enr, arms=c("Control", "HC 1", "HC 2", "HC 3"), y_val="surv", reverse=TRUE, pct=TRUE) + ylab ("% enrollment per arm") 

## do the logrank test
test_res <-   do_tests(event_dat)
test_res_stop <- do_tests(event_dat_stop)


    