#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix sim_patient_core_Cpp (NumericMatrix sim_p, NumericVector times, NumericMatrix haz_table,
                                    int arm_current, int n_arms, NumericVector cov_effects) {
  int switched = 0; 
  NumericVector pat_haz = haz_table( (arm_current-1), _) ;
  NumericVector pat_haz_eff = NumericVector::create (0, 0, 0) ;
  int sth_happened = 0;
  for (int i = 0; i < 3; ++i){
     pat_haz_eff(i) = pat_haz(i)*cov_effects(i);
  }
  for (int i = 2; i <= sim_p.nrow(); i++) {
    double dtime = times(i-1) - times(i-2);  
    for (int j = 1; j < 4; j++) {
      double r = (float)rand()/(float)RAND_MAX;
      if (r < (1-exp(-pat_haz_eff[j-1]*dtime/365))) {
        sim_p(i-1, j) = 1;
      }
    }
    sim_p(i-1,4) = arm_current;
    NumericMatrix sum1 = sim_p(Range(i-1, i-1), Range(1,3));
    if (sum(sum1) > 0) {
      // something happened 
      if (sim_p(i-1,2) == 1) {
        // dropout event
        NumericMatrix sim_p_new = sim_p(Range(0,(i-1)),Range(0,5));
        NumericMatrix::Row upd_row = sim_p_new( i-1, _);
        upd_row [1] = -1;
        upd_row [3] = -1;
        return sim_p_new;
      }
      if (sim_p(i-1,1) == 1) {
        // event of interest
        NumericMatrix sim_p_new = sim_p(Range(0,(i-1)),Range(0,5));
        NumericMatrix::Row upd_row = sim_p_new( i-1, _);
        upd_row [2] = -1;
        upd_row [3] = -1;
        return sim_p_new;
      }
      if (sim_p(i-1,3) == 1) {
          // switch arms
        if (switched == 0) {
          int r = rand() % n_arms + 1;
          if (r == arm_current) { r = n_arms; }
          arm_current = r;
          pat_haz = haz_table (Range(arm_current-1, arm_current-1), Range(0,2)) ;
          for (int i = 0; i < 3; ++i){
            pat_haz_eff(i) = pat_haz(i)*cov_effects(i);
          }
          switched = 1;
        }
      }
    }
  }
  return sim_p;
}

// R original function (note: arguments are not exactly the same)
//sim_patient_core_R <- function (sim_p, pat_haz, arms, arm_current, cov_effects) {
//  switched <- 0 
//  pat_haz <- haz_table[arm_current,]
//  pat_haz_eff <- pat_haz * cov_effects
//  for (i in 2:length(sim_p$time)) { # can't do apply since hazard might change over time
//    dtime <- sim_p$time[i] - sim_p$time[i-1]
//    sim_p[i,2:4] <- (runif(3) < c(1-1*exp(-pat_haz_eff * dtime/365)))*1    
//    sim_p[i,5] <- arm_current
//    if (sum(sim_p[i,2:4]) > 0) { # something happened
//      if (sim_p[i,]$dropout == 1) {
//        sim_p <- sim_p[1:i,]
//        sim_p[i:length(sim_p$time), c(2,4)] <- -1
//        break
//      }
//      if (sim_p[i,]$event == 1) {
//        sim_p <- sim_p[1:i,]
//        sim_p[i:length(sim_p$time), c(3,4)] <- -1
//        break
//      }
//      if ((sim_p[i,]$switch == 1)&&(switched == 0)) {
//        arm_current <- arms[-arm_current][round(runif(1)*length(arms[-arm_current])+0.5)] # switch to another arm     
//        pat_haz <- haz_table[arm_current,]
//        pat_haz_eff <- pat_haz * cov_effects
//        switched <- 1
//      }
//    }
//  }
//  return(sim_p)
//}
