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

sim_patient_core_vectorized_R <- function (sim_p, haz_table, arms, arm_current, cov_effects, switch_only_once = TRUE) {
  switched <- FALSE
  pat_haz <- haz_table[arm_current,]
  pat_haz_eff <- pat_haz * cov_effects

  # first determine switch times
  n_visits <- length(sim_p[,1])
  sim_p$dtime <- c(0, sim_p$time[2:n_visits] - sim_p$time[1:(n_visits-1)])
  sim_p$haz_e <- 1-exp(-pat_haz_eff[1] * sim_p$dtime/365) * cov_effects[1]
  sim_p$haz_d <- 1-exp(-pat_haz_eff[2] * sim_p$dtime/365) * cov_effects[2]
  sim_p$haz_s <- 1-exp(-pat_haz_eff[3] * sim_p$dtime/365) * cov_effects[3]
  switch_sim <- (runif(n_visits) < sim_p$haz_s)*1
  switch_times <- c(1:n_visits)[!is.na(match (switch_sim,1))]
  #switch_times <- c(3,5)
  
  if (length(switch_times) > 0) {
    if (switch_only_once) { switch_times <- switch_times[1] }
    sim_p$switch[switch_times] <- 1
    for (i in 1:length(switch_times)) {
      sim_p_tmp <- sim_p[c(switch_times[i]:n_visits),]  
      arm_current <- arms[-arm_current][round(runif(1)*length(arms[-arm_current])+0.5)] # switch to another arm     
      sim_p_tmp$arm <- arm_current
      pat_haz <- haz_table[arm_current,]
      pat_haz_eff <- pat_haz * cov_effects
      sim_p_tmp$haz_e[-1] <- 1-exp(-pat_haz_eff[1] * sim_p_tmp$dtime/365)[-1] 
      sim_p_tmp$haz_d[-1] <- 1-exp(-pat_haz_eff[2] * sim_p_tmp$dtime/365)[-1]
      sim_p_tmp$haz_s[-1] <- 1-exp(-pat_haz_eff[3] * sim_p_tmp$dtime/365)[-1]
      sim_p[c(switch_times[i]:n_visits),] <- sim_p_tmp
    }
  }
  ## then simulate dropout
  dropout_sim <- (runif(n_visits) < sim_p$haz_d) * 1
  dropout_time <- c(1:n_visits)[!is.na(match (dropout_sim,1))][1]
  if(!is.na(dropout_time)) {
    sim_p[dropout_time,c(2:4)] <- c(-1,1,-1)
    sim_p <- sim_p[1:dropout_time,]
  }
  
  ## then simulate events
  l <- length(sim_p[,1])
  if (l>1) {
    event_sim <- (runif(l) < sim_p[1:l,]$haz_e) * 1
    event_time <- c(1:n_visits)[!is.na(match (event_sim,1))][1]
    if (!is.na(event_time)) {
      if (sim_p[event_time,2] != -1) {
        sim_p[event_time, c(2:4)] <- c(1,-1,-1)
        sim_p <- sim_p[c(1:event_time),]
      }
    }
  }
  return(sim_p[,c(1:6)])
}
