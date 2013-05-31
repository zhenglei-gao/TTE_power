# analyze results from the power analysis
require(reshape)
source("../power_functions.R")

p <- list()

scen1 <- get_scen_data("scen1")
#tte_boxplot_power (scen1)
csv1 <- read.csv(file="scen1.csv")
p$d10 <- calc_power(csv1)

## Scenario 2a: dropout rate = 20% / 18 months
scen4a <- get_scen_data("scen4a")
#tte_boxplot_power (scen4a)
csv4a <- read.csv(file="scen4a.csv")
p$d5 <- calc_power(csv4a)

scen4a_s <- get_scen_data("scen4a_stop")
#tte_boxplot_power (scen4a_s)
csv4a_s <- read.csv(file="scen4a_stop.csv")
p$d5s <- calc_power(csv4a_s)

## Scenario 2a: dropout rate = 5% / 18 months
scen4b <- get_scen_data("scen4b")
# tte_boxplot_power (scen4b)
csv4b <- read.csv(file="scen4b.csv")
p$d5 <- calc_power(csv4b)

scen4b_s <- get_scen_data("scen4b_stop")
# tte_boxplot_power (scen4b_s)
csv4b_s <- read.csv(file="scen4b_stop.csv")
p$d5s <- calc_power(csv4b_s)

## Scenario 4c: dropout rate = 35% / 18 months
scen4c <- get_scen_data("scen4c")
# tte_boxplot_power (scen4c)
csv4c <- read.csv(file="scen4c.csv")
p$d35 <- calc_power(csv4c)

scen4c_s <- get_scen_data("scen4c_stop")
#tte_boxplot_power (scen4c_s)
csv4c_s <- read.csv(file="scen4c_stop.csv")
p$d35s <- calc_power(csv4c_s)

## Scenario 4d: dropout rate = 50% / 18 months
scen4d <- get_scen_data("scen4d")
#tte_boxplot_power (scen4d)
csv4d <- read.csv(file="scen4d.csv")
p$d50 <- calc_power(csv4d)

scen4d_s <- get_scen_data("scen4d_stop")
#tte_boxplot_power (scen4d_s)
csv4d_s <- read.csv(file="scen4d_stop.csv")
p$d50s <- calc_power(csv4d_s)

## summary plot of power vs dropout rate
summ <- data.frame(cbind("dropout"= c(5, 10, 20, 35), 
                         "method_1"= as.num(c(p$d5[1], p$d10[1], p$d20[1], p$d35[1])), 
                          "method_2"= as.num(c(p$d5[2], p$d10[2], p$d20[2], p$d35[2]))))
summ_s <- data.frame(cbind("dropout"= c(5, 10, 20, 35, 50), 
                         "method_1"= as.num(c(p$d5s[1], p$d10s[1], p$d20s[1], p$d35s[1])), 
                         "method_2"= as.num(c(p$d5s[2], p$d10s[2], p$d20s[2], p$d35s[2]))))

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
