# analyze results from the power analysis
require(reshape)
source("../power_functions.R")

p <- list()

scen1 <- get_scen_data("scen1")
tte_boxplot_power (scen1)
csv1 <- read.csv(file="scen1.csv")
p$d10 <- calc_power(csv1)

## Scenario 2a: dropout rate = 20% / 18 months
scen2a <- get_scen_data("scen2a")
tte_boxplot_power (scen2a)
csv2a <- read.csv(file="scen2a.csv")
p$d20 <- calc_power(csv2a)

scen2a_s <- get_scen_data("scen2a_stop")
tte_boxplot_power (scen2a_s)
csv2a_s <- read.csv(file="scen2a_stop.csv")
p$d20s <- calc_power(csv2a_s)

## Scenario 2a: dropout rate = 5% / 18 months
scen2b <- get_scen_data("scen2b")
tte_boxplot_power (scen2b)
csv2b <- read.csv(file="scen2b.csv")
p$d5 <- calc_power(csv2b)

scen2b_s <- get_scen_data("scen2b_stop")
tte_boxplot_power (scen2b_s)
csv2b_s <- read.csv(file="scen2b_stop.csv")
p$d5s <- calc_power(csv2b_s)

## Scenario 2c: dropout rate = 35% / 18 months
scen2c <- get_scen_data("scen2c")
tte_boxplot_power (scen2c)
csv2c <- read.csv(file="scen2c.csv")
p$d35 <- calc_power(csv2c)

scen2c_s <- get_scen_data("scen2c_stop")
tte_boxplot_power (scen2c_s)
csv2c_s <- read.csv(file="scen2c_stop.csv")
p$d35s <- calc_power(csv2c_s)

## Scenario 2d: dropout rate = 50% / 18 months
scen2d <- get_scen_data("scen2d")
tte_boxplot_power (scen2d)
csv2d <- read.csv(file="scen2d.csv")
p$d50 <- calc_power(csv2d)

scen2d_s <- get_scen_data("scen2d_stop")
tte_boxplot_power (scen2d_s)
csv2d_s <- read.csv(file="scen2d_stop.csv")
p$d50s <- calc_power(csv2d_s)

## summary plot of power vs dropout rate
summ <- data.frame(cbind("dropout"= c(5, 10, 20, 35, 50), 
                         "method_1"= as.num(c(p$d5[1], p$d10[1], p$d20[1], p$d35[1], p$d50[1])), 
                          "method_2"= as.num(c(p$d5[2], p$d10[2], p$d20[2], p$d35[2], p$d50[2]))))
summ_s <- data.frame(cbind("dropout"= c(5, 10, 20, 35, 50), 
                         "method_1"= as.num(c(p$d5s[1], p$d10s[1], p$d20s[1], p$d35s[1], p$d50s[1])), 
                         "method_2"= as.num(c(p$d5s[2], p$d10s[2], p$d20s[2], p$d35s[2], p$d50s[2]))))

summ_m <- melt(summ, id=c("dropout"), meas=c("method_1","method_2"))
summ_s_m <- melt(summ_s, id=c("dropout"), meas=c("method_1","method_2"))
ggplot (summ_m, aes(x=factor(dropout), y=value*100, colour=variable, group=variable)) +
  geom_line(size=1.5) + geom_smooth(method="lm", fill=NA, linetype=3) +
  scale_colour_manual("", values=cols)+
  ylim(c(0,100)) + 
  ylab("Power") +
  xlab("Dropout %")
ggplot (summ_s_m, aes(x=factor(dropout), y=value*100, colour=variable, group=variable)) +
  geom_line(size=1.5) + geom_smooth(method="lm", fill=NA, linetype=3) +
  scale_colour_manual("", values=cols)+
  ylim(c(0,100)) + 
  ylab("Power") +
  xlab("Dropout %")

## 2 new simulations
# - Dropout up to 50%
# - Dropout only for active arms
# - Condom use
#   in each progestron arm 10% use condom --> risk is 90% lower: 0.00525
#   what if 25-50% uses condom
# - Enrollment
