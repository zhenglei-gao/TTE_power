#setwd("scenarios")

source(file="../plot_functions.R")
dir.create("pdf")

## Scenario 2: Dropout higher in all arms arms
scen2 <- list (folder = "scen2", xlab = "Dropout %",
               pl_dat = data.frame(cbind(file= c("2b", "2", "2a", "2c", "2d"), 
                                         xval = c(5, 10, 20, 35, 50)) ) )
plot_scenario (scen2, smooth = F, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen2.pdf")

## Scenario 4: Dropout higher in active arms arms
scen4 <- list (folder = "scen4", xlab = "Dropout %",
               pl_dat = data.frame(cbind(file= c("4a", "2", "4b", "4c", "4d"), 
                                         xval = c(5, 10, 20, 35, 50)) ) )
plot_scenario (scen4, smooth = F, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen4.pdf")

## Scenario 3: Condom use, dropout same in all arms
scen3 <- list (folder = "scen3", xlab = "Dropout %",
               pl_dat = data.frame(cbind(file= c("3a", "3", "3b", "3c", "3d"), 
                                         xval = c(5, 10, 20, 35, 50)) ) )
plot_scenario (scen3, smooth = F, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen3.pdf")

## Scenario 5: Condom use, dropout only varies in active arms
scen5 <- list (folder = "scen5", xlab = "Dropout %",
               pl_dat = data.frame(cbind(file= c("5a", "3", "5b", "5c", "5d"), 
                                         xval = c(5, 10, 20, 35, 50)) ) )
plot_scenario (scen5, smooth = FALSE, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen5.pdf")

## Scenario 6: Drop out high in only one active arm (up to 100%) 
scen6 <- list (folder = "scen6", xlab = "Dropout %",
               pl_dat = data.frame(cbind(file= c("2", "6a", "6b", "6c", "6d", "6e"), 
                                         xval = c(10, 20, 35, 50, 75, 100)) ) )
plot_scenario (scen6, smooth = FALSE, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen6.pdf")

## Scenario 7: Drop out high in two active arms  
scen7 <- list (folder = "scen7", xlab = "Dropout %",
               pl_dat = data.frame(cbind(file= c("2", "7a", "7b", "7c", "7d", "7e"), 
                                         xval = c(10, 20, 35, 50, 75, 100)) ) )
plot_scenario (scen7, smooth = FALSE, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen7.pdf")

## Scenario 8: Enrollment slower in 1 active arm
scen8 <- list (folder = "scen8", xlab = "Enrollment done after (months)",
               pl_dat = data.frame(cbind(file= c("2", "8a", "8b", "8c"), 
                                         xval = c(6, 12, 18, 24)) ) )
plot_scenario (scen8, smooth = FALSE, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen8.pdf")

## Scenario 9: Enrollment slower in 2 active arm
scen9 <- list (folder = "scen9", xlab = "Enrollment done after (months)",
               pl_dat = data.frame(cbind(file= c("2", "9a", "9b", "9c"), 
                                         xval = c(6, 12, 18, 24)) ) )
plot_scenario (scen9, smooth=FALSE, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen9.pdf")

## Scenario 10: Condom use 10-20-35-50% in active arms
scen10 <- list (folder = "scen10", xlab = "Condom use (%)",
               pl_dat = data.frame(cbind(file= c("3", "10a", "10b", "10c"), 
                                         xval = c(10, 20, 35, 50)) ) )
plot_scenario (scen10, smooth=FALSE, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen10.pdf")

## Scenario 11: Condom use 10-20-35-50% in all arms
scen11 <- list (folder = "scen11", xlab = "Condom use (%)",
                pl_dat = data.frame(cbind(file= c("3", "11a", "11b", "11c"), 
                                          xval = c(10, 20, 35, 50)) ) )
plot_scenario (scen11, smooth=FALSE, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen11.pdf")

## Scenario 12: Condom use 10% in active: relative risk not zero but 0-25-50-75%
scen12 <- list (folder = "scen12", xlab = "Relative risk HIV infection (%)",
                pl_dat = data.frame(cbind(file= c("3", "12a", "12b", "12c"), 
                                          xval = c(0, 25, 50, 75)) ) )
plot_scenario (scen12, smooth=FALSE, add_stop_crit = TRUE, lm_fit = FALSE, pdf = "pdf/scen11.pdf")






