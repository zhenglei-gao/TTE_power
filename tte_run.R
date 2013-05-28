## Purpose     : Run tte power analyses
## Authors     : Ron Keizer (ron.keizer@ucsf.edu), Rada Savic
## Description : This script is an example of how to set up a power analysis
##               It will walk through the required steps, and create the 
##               plots and summaries.

## read in functions for cts engine and plotting
## Will eventually be made into an R-module
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
  enrollment_rate  = c(4,4,4,4)  # describe enrollment rates in all arms (per day)
)

arm_design <- list (
  "control" = tte_arm_design (hazard_event = 0.035, hazard_dropout = 0.1, hazard_switch = 0.1, n_patients=300, patient_design = pat1),
  "hc1" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1, hazard_switch = 0.1, n_patients=300, patient_design = pat1),
  "hc2" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1, hazard_switch = 0.1, n_patients=300, patient_design = pat1),
  "hc3" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.1, hazard_switch = 0.1, n_patients=300, patient_design = pat1)
)

trial_design <- tte_trial_design (
  arm_design = arm_design,
  control_arm = 1,                                # which arm is the control
  enrollment_design = enrollment_design,
  max_individual_length = 18*30,                  # n days
  max_trial_length = 180,                         # n days
  visits = c(0, 30, 90, 120, 150, 180)            # visit times (days)
)

## simulate trial
tte_sim_trial <- function (trial_design) {
  # simulate the enrollment of patients
  enrollment <- tte_sim_enrollment (trial_design$arm_design, trial_design$enrollment_design)

  # Simulate the trial
  arm_names <- names(trial_design$arm_design)
  dat <- c()
  for (i in seq(arm_names)) {
    for (j in 1:arm_design[[arm_names[i]]]$n_patients) {    
      dat <- rbind(dat, 
                 cbind(arm_names[i], j, 
                       tte_sim_patient (patient_design = arm_design[[arm_names[i]]]$patient_design,  
                                        trial_design = trial_design,  
                                        offset = enrollment[[arm_names[i]]][j] ) 
      ))
    }
  }
  return(dat)
}

dat <- tte_sim_trial (trial_design)

## Run just one trial simulation, to check whether our design is correct
trial_res <- tte_sim_trial (trial_des)

## Make plots and summary of the trial simulation
plots_1 <- tte_plot_trial (trial_1)
summ_1  <- tte_sum_trial (trial_1, format="text")

## Run the power analysis
pow_1 <- tte_run_power (design = sim_design1, 
                        test = c("log-rank"), # currently only log-rank test available
                        n_sim = 1000) 

## Make some informative plots and create text summary
pow_plots_1 <- tte_plot_power (pow1)
pow_summ_1  <- tte_sum_power (pow_1, format="text")