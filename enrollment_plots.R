## plot enrollment curves
source ("../cts_functions.R")   # the main simulation engine
source ("../power_functions.R") # the functions for the statistical test and power analysis
source ("../plot_functions.R")  # plotting & summarizing functions

pat1 <- tte_patient_design (arm_start = 1)
pat2 <- tte_patient_design (arm_start = 2)
pat3 <- tte_patient_design (arm_start = 3) 
pat4 <- tte_patient_design (arm_start = 4) 

## Create an object describing the enrollment
enrollment_design <- tte_enrollment_design ( 
  enrollment_rate  = c(3000/(365*.5), 
                       3000/(365*1), 
                       3000/(365*1.5), 
                       3000/(365*2))   # enrollment rates in all arms (per day); all patients enrolled in 6 months
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

dat <- tte_sim_trial (trial_design)
dat_stop <- apply_stopping_criterion(dat, max_events=572)

enr_dat <- extract_enrollment_data(dat, trial_stop_time = 1.5*365)
fit_enr <- survfit(Surv(time, event) ~ arm, data=enr_dat)

enr_dat_s <- extract_enrollment_data(dat_stop, trial_stop_time = 1.5*365)
fit_enr_s <- survfit(Surv(time, event) ~ arm, data=enr_dat_s)

pl <- gg_surv_plot(fit_enr, arms=c("6 months", "1 year", "1.5 years", "2 years"), y_val="cum.event", 
            reverse=FALSE, pct=FALSE) + ylab ("Subjects enrolled per arm") +
        xlim(c(0,600)) +
        geom_vline(xintercept = max(fit_enr$time), linetype=3)
pdf (file="pdf/enrollment_rates.pdf", width=6, height=4)
print(pl)
dev.off()

pl <- gg_surv_plot(fit_enr_s, arms=c("6 months", "1 year", "1.5 years", "2 years"), y_val="cum.event", 
               reverse=FALSE, pct=FALSE) + ylab ("Subjects enrolled per arm") +
  xlim(c(0,600)) +
  geom_vline(xintercept = max(fit_enr_s$time), linetype=3)

pdf (file="pdf/enrollment_rates.pdf", width=6, height=4)
print(pl)
dev.off()
