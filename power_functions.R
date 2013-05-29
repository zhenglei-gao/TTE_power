do_tests <- function (event_dat) {
  ## first approach: merge the test arms
  t1 <- survdiff(Surv(time, event) ~ arm_type, data=event_dat) # merged test-arms
  
  ## second approach: test arms separately vs control
  t2a <- survdiff(Surv(time, event) ~ arm, data=event_dat[event_dat$arm %in% c(1,2),])
  t2b <- survdiff(Surv(time, event) ~ arm, data=event_dat[event_dat$arm %in% c(1,3),])
  t2c <- survdiff(Surv(time, event) ~ arm, data=event_dat[event_dat$arm %in% c(1,4),])
  
  ## calculate p-values
  p_vals <- 1-c(pchisq(t1$chisq, 1), pchisq(t2a$chisq, 1), pchisq(t2b$chisq, 1), pchisq(t2c$chisq, 1))
}

tte_run_power_analysis <- function (trial_design, n_sim, max_events=572, name="scen1", write_csv = TRUE) {
  n_sim <- 200
  comb <- c()
  comb_stop <- c()
  for (i in 1:n_sim) {
    dat <- tte_sim_trial (trial_design)
    dat_stop <- apply_stopping_criterion(dat, max_events=max_events)
    event_dat <- extract_event_data(dat)
    test_res <- do_tests(event_dat)
    event_dat_stop <- extract_event_data(dat_stop)
    test_res_stop <- do_tests(event_dat_stop)  
    comb <- rbind(comb, test_res)
    comb_stop <- rbind(comb_stop, test_res_stop)
    if (write_csv) {
      write.csv(comb, file=paste(name,".csv", sep=""), quote=F, row.names=F)
      write.csv(comb_stop, file=paste(name, "_stop.csv", sep=""), quote=F, row.names=F)
    }
    cat (paste("Simulation: ", i, "\n", sep=""))
  }
}

calc_power <- function (csv) {
  # non-parametric and parametric (assuming normality of log(p)) power estimate
  power_stop <- list (
    "test1_nonpar" = sum(csv[,1] < 0.05) / length(csv[,1]),
    "test2_nonpar" = sum(apply((csv[,c(2:4)] < 0.017), 1, sum) == 3) / length(csv[,2]), # any of the 3 tests is positive
    "test1_par" = pnorm(log(0.05), mean(log(csv[,1])), sd(log(csv[,1]))),
    "test2_nonpar" = pnorm(log(0.017), mean(log(csv[,2])), sd(log(csv[,2]))) *
      pnorm(log(0.017), mean(log(csv[,3])), sd(log(csv[,3]))) *
      pnorm(log(0.017), mean(log(csv[,4])), sd(log(csv[,4]))) 
  )
  return(power_stop)
}


