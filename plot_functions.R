require(ggplot2)

plot_scenario <- function (scen_info = list(), 
                           smooth = TRUE,
                           lm_fit = FALSE, 
                           add_stop_crit = TRUE, 
                           parametric = FALSE, # parametric power estimate?
                           pdf = NULL) {
  cols <- c("#444499", "#994444", "#444499", "#994444")
  linetypes <- c(1, 1, 3, 3)
  ncol <- 5
  if (add_stop_crit) { ncol <- 9 }
  res <- data.frame(matrix(ncol=ncol , nrow=length(scen_info$pl_dat$xval)))
  for (i in seq(scen_info$pl_dat[,1])) {
    csv_tmp <- read.csv(file=paste(scen_info$folder, "/scen", scen_info$pl_dat[i,]$file, ".csv", sep=""))
    res[i,c(2:5)] <- as.num(unlist(calc_power(csv_tmp)) )
    res[i,1] <- as.num(scen_info$pl_dat[i,]$xval)
  }
  if (add_stop_crit) {
    for (i in seq(scen_info$pl_dat[,1])) {
      csv_tmp <- read.csv(file=paste(scen_info$folder, "/scen", scen_info$pl_dat[i,]$file, "_stop.csv", sep=""))
      res[i,c(6:9)] <- as.num(unlist(calc_power(csv_tmp)) )
    }
  }
  col_nams <- c("x_var", "method 1", "method 2", "method 1 param", "method 2 param")
  if (add_stop_crit) { 
    col_nams <- c(col_nams, paste(col_nams[2:5], "+ stop"))  
    colnames(res) <- col_nams
    if (parametric) {    
      res_m <- melt(res, id=c("x_var"), meas=c("method 1 param", "method 2 param", "method 1 param + stop", "method 2 param + stop"))
    } else {  
      res_m <- melt(res, id=c("x_var"), meas=c("method 1", "method 2", "method 1 + stop", "method 2 + stop"))
    }
  } else {
    colnames(res) <- col_nams
    if (parametric) {    
      res_m <- melt(res, id=c("x_var"), meas=c("method 1 param","method 2 param"))
    } else {  
      res_m <- melt(res, id=c("x_var"), meas=c("method 1","method 2"))
    }
  }
  pl <- ggplot (res_m, aes(x=x_var, y=value*100, colour=variable, group=variable)) +
    scale_colour_manual("", values=cols) +
    scale_linetype_manual ("", values=linetypes) +
    ylim(c(0,100)) + 
    ylab("Power") +
    xlab(scen_info$xlab) 
  if (smooth) {
    pl <- pl + geom_smooth(size = 1.25, fill=NA, span=3, aes(linetype=variable))
  } else {
    pl <- pl + geom_line(size=1.25, aes(linetype=variable))    
  }
  if (lm_fit) {
    pl <- pl + geom_smooth(method="lm", fill=NA, linetype=3) 
  }
  if (!is.null(pdf)) {
    pdf(file=pdf)
    print(pl)
    dev.off()
  }
  return(pl)
}

extract_enrollment_data <- function (dat, trial_stop_time = 1.5*365) {
  extract_enrollment_function <- function (d) {
    return(c(min(d$time), d$arm[1]))
  }
  dat <- dat[dat$time <= trial_stop_time,]
  dat$grp <- paste(dat$arm, dat$patient, sep="_")
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
  dat$cum.event <- 0
  dat <- ddply(dat, "arm", function (d) { d$cum.event <- 1:length(d$cum.event); d } )
  if (is.null(arms)) {
    arms <- unique(pl_dat$arm)
  }
  if (y_val == "surv") {
    pl_dat <- ddply (dat, "arm", gg_format_survfit)
  } else {
    pl_dat <- dat
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