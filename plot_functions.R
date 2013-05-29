require(ggplot2)

extract_enrollment_data <- function (dat) {
  extract_enrollment_function <- function (d) {
    return(c(min(d$time), d$arm[1]))
  }
  dat$grp <- paste(dat$arm_name, dat$patient, sep="_")
  enr_dat <- ddply (dat, "grp", extract_enrollment_function)[-1]
  colnames(enr_dat) <- c("time","arm")
  enr_dat <- enr_dat[order(enr_dat$arm, enr_dat$time),]
  enr_dat$event <- 1
  return(enr_dat)  
}

gg_surv_plot <- function (fit, arms=NULL, y_val = "surv", 
                          reverse=FALSE, pct=TRUE, show_ci=0) {
  gg_format_survfit <- function (d) {
    baseline <- d[d$time == min(d$time),][1,]
    baseline$time <- 0
    baseline$surv <- 1
    baseline$upper <- 1
    baseline$lower <- 1
    baseline$n.event <- 0
    newdat <- rbind(baseline, d) 
    step_points <- newdat[1:(length(newdat[,1])-1),]
    step_points$time <- newdat[2:length(newdat[,1]),]$time
    ggdat <- rbind(newdat, step_points)
    ggdat <- ggdat[order(ggdat$time, rev(ggdat$surv)),]
    return (ggdat)
  }
  dat <- data.frame(cbind(time = fit$time, 
                          arm = rep(1:length(fit$strata), as.num(fit$strata)), 
                          n.event=fit$n.event, 
                          surv=fit$surv,
                          upper=fit$upper, 
                          lower=fit$lower))
  if (y_val == "surv") {
    pl_dat <- ddply (dat, "arm", gg_format_survfit)
  } else {
    pl_dat <- dat
  }
  if (is.null(arms)) {
    arms <- unique(pl_dat$arm)
  }
  pl_dat$arm_name <- factor(pl_dat$arm, labels=arms)
  pl_dat$y_val <- pl_dat[[y_val]]
  if (reverse) {pl_dat$y_val <- 1-pl_dat$y_val}
  if (pct) {
    pl_dat$y_val <- pl_dat$y_val*100
    pl_dat$upper <- pl_dat$upper*100
    pl_dat$lower <- pl_dat$lower*100
  }
  pl <- ggplot(pl_dat, aes(x=time, y=y_val, colour=arm_name, group=arm_name)) + 
      geom_line() + 
      xlab ("Time (days)") +
      scale_colour_brewer("", palette="Set1") 
  if (length(show_ci) == 1) {
    if (show_ci) {
      pl <- pl + geom_ribbon(aes(ymin=lower, ymax=upper, group=arm_name), colour=NA, fill=rgb(0.6,0.6,0.6,0.4))
    }
  } else {
    for (i in seq(show_ci)) { # show ci for 1 or more arms
      if (show_ci[i]) {
        pl <- pl + geom_ribbon(data=pl_dat[pl_dat$arm==i,], aes(x=time, ymin=lower, ymax=upper), colour=NA, fill=rgb(0.6,0.6,0.6,0.4))
      }
    }
  }
  return(pl)
}

tte_plot_trial <- function ( # create plots to summarize the trial
    tte_trial,
    pdf_file = NULL
  ) { 
  pl <- list()
  
  # Plots:
  # - Kaplan-Meier with parametric CI
  pl$kaplan <- ggplot ()

  # - %events ~ time 
  pl$events_vs_time <- ggplot ()
  
  # - %event ~ time, split by arm
  pl$events_vs_time_per_arm <- ggplot ()

  # - %dropout ~ time 
  pl$dropout_vs_time <- ggplot ()
  
  # - %dropout ~ time, splity by arm
  pl$dropout_vs_time_per_arm <- ggplot ()

  # - n patients in each arm (not dropped out)
  pl$patients_vs_time <- ggplot ()
  
  if (!is.null(pdf)) {
    pdf (file = pdf_file)
    for (i in seq(pl)) {
      print (pl[i])
    }
    dev.off()
  } 
  return(pl)
}

tte_sum_trial <- function () { # create summary table for trial
  # Summary table with per visit:
  # n events
  # n dropouts
  # % remaining
  # n in each arm
  # % in each arm
  # % switched
  return(summ)
}

tte_plot_power <- function ( # create several plots to summarize the power analysis
    pdf_file = NULL
  ) {  
  pl <- list()
  # Plots:
  # - Kaplan-Meier with PI over simulations
  # - distribution of trial outcomes (% events in each trial, possibly split by visit)
  if (!is.null(pdf)) {
    pdf (file = pdf_file)
    for (i in seq(pl)) {
      print (pl[i])
    }
    dev.off()
  } 
  return(pl)
}

tte_sum_power <- function () { # create summary table for power analysis
  return(summ)
}