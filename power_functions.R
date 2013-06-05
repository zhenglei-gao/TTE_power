
get_scen_data <- function (scen = "scen1") {
  csv <- read.csv(file=paste(scen, ".csv", sep=""))
  colnames(csv) <- c("Test combined", "Test HC1", "Test HC2", "Test HC3")
  csv_m <- melt(csv)
  return(csv_m)
}

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

tte_run_power_analysis <- function (trial_design, 
                                    n_sim = 200, 
                                    trial_stop_time = 1.5 * 365,
                                    max_events=572, 
                                    name="scen1", 
                                    write_csv = TRUE) {
  comb <- c()
  comb_stop <- c()
  for (i in 1:n_sim) {
    dat <- tte_sim_trial (trial_design)
    dat_stop <- apply_stopping_criterion(dat, max_events=max_events)
    event_dat <- extract_event_data(dat, until_time=trial_stop_time)
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

