## Purpose: Run tte power analyses
## Authors: Ron Keizer (ron.keizer@ucsf.edu), Rada Savic

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
  enrollment_design = enrollment_design_1,
  max_individual_length = 18*30,                  # n days
  max_trial_length = 180,                         # n days
  rate_event = c(0.035, 0.0525, 0.0525, 0.0525),  # per year
  rate_dropout = c(0.1, 0.1, 0.1, 0.1),           # per year
  rate_switch = c(0.1, 0.1, 0.1, 0.1)             # per year
)

## Run the analysis
res_1   <- tte_power_do (sim_design1,
                         nsim = 1000,
                         max_tria) 

## Make some informative plots
plots_1 <- tte_power_plots()

## Summarize results in text/table format
summary(tte_res_1, format="text")