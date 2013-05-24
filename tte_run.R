## Purpose: Run tte power analyses
## Authors: Ron Keizer (ron.keizer@ucsf.edu), Rada Savic

## read in functions for cts engine and plotting
## Will eventually be made into an R-module
source ("cts_functions.R")   # the main simulation engine
source ("power_functions.R") # the functions for the statistical test and power analysis
source ("plot_functions.R")  # plotting & summarizing functions

## Create an object describing the enrollment
patient_design_1 <- tte_patient_design (
  cont_covariates = list (
    "WT" = list (mean = 70,
                 sd = 7,
                 distr = "rnorm"),
    "AGE" = list (mean = 70,
                  sd = 7,
                  distr = "rnorm"),
  ), 
  cat_covariates = list(
    "RACE" = list (values=c(1,2,3,4),
                   prob=c(0.25, 0.25, 0.25, 0.25)
    )
  )
)

## Create an object describing the enrollment
enrollment_design_1 <- tte_enrollment_design (
  n_patients = 12000,
  n_arms = 4,
  enrollment_rate  = c(.5, .5, .5, .5),           # describe enrollment rates in all arms (per day)
  max_trial_length = 180                          # n days
)

## Create an object describing the trial design
sim_design_1 <- tte_create_design (
  n_patients = 12000,
  n_arms = 4,
  control_arm = 1,                                # which arm is the control
  enrollment_design = enrollment_design_1,
  max_individual_length = 18*30,                  # n days
  max_trial_length = 180,                         # n days
  visits = c(0, 30, 90, 120, 150, 180)            # visit times (days)
  rate_event = c(0.035, 0.0525, 0.0525, 0.0525),  # per year
  rate_dropout = c(0.1, 0.1, 0.1, 0.1),           # per year
  rate_switch = c(0.1, 0.1, 0.1, 0.1)             # per year, assuming switch is random to another arm
)

## Run just one trial simulation, to check whether our design is correct
trial_1 <- tte_run_trial (sim_design_1)

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