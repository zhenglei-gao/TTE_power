## Purpose: contains R-functions that form the main engine of the trial simulator
## Authors: Ron Keizer (ron.keizer@ucsf.edu), Rada Savic

tte_create_design (n_patients = 12000,
                   n_arms = 4,
                   max_individual_length = 18*30,                 # days
                   rate_event = c(0.035, 0.0525, 0.0525, 0.0525), # /year
                   rate_dropout = c(0.1, 0.1, 0.1, 0.1),          # /year
                   rate_switch = c(0.1, 0.1, 0.1, 0.1))           # /year

tte_create_patient (
  )
