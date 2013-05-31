## implement the power analysis
## Scenario 3: condom use, but 20% dropout

source ("../cts_functions.R")   # the main simulation engine
source ("../power_functions.R") # the functions for the statistical test and power analysis
source ("../plot_functions.R")  # plotting & summarizing functions

registerDoMC(1)

## patient object
bin_cov_control <- list (condom = list (prob = 0, rr_hazard_event = 1, rr_hazard_dropout = 1, rr_hazard_switch = 1))
bin_cov <- list (condom = list (prob = 0.1, rr_hazard_event = 0, rr_hazard_dropout = 1, rr_hazard_switch = 1))
pat1 <- tte_patient_design (arm_start = 1, binary_covariates = bin_cov_control)
pat2 <- tte_patient_design (arm_start = 2, binary_covariates = bin_cov)
pat3 <- tte_patient_design (arm_start = 3, binary_covariates = bin_cov) 
pat4 <- tte_patient_design (arm_start = 4, binary_covariates = bin_cov) 
# implement a check that covariates are defined for all groups equally!!

## Create an object describing the enrollment
enrollment_design <- tte_enrollment_design ( 
  enrollment_rate  = rep( 3000/(365/2), 4)   # enrollment rates in all arms (per day); all patients enrolled in 6 months
)

arm_design <- list (
  "control" = tte_arm_design (hazard_event = 0.035, hazard_dropout = 0.2/1.5, hazard_switch = 0.1/1.5, n_patients=3000, patient_design = pat1),
  "hc1" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.2/1.5, hazard_switch = 0.1/1.5, n_patients=3000, patient_design = pat2),
  "hc2" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.2/1.5, hazard_switch = 0.1/1.5, n_patients=3000, patient_design = pat3),
  "hc3" = tte_arm_design (hazard_event = 0.0525, hazard_dropout = 0.2/1.5, hazard_switch = 0.1/1.5, n_patients=3000, patient_design = pat4)
)

trial_design <- tte_trial_design (
  arm_design = arm_design,
  control_arm = 1,                                # which arm is the control
  enrollment_design = enrollment_design,
  visits = c(0, 90, 180, 270, 360, 450, 540),      # visit times (days)
  max_events = NULL  # stopping criterion, can be implemented later as well
)

tte_run_power_analysis (trial_design, n_sim=200, max_events=572, name="scen3b")
