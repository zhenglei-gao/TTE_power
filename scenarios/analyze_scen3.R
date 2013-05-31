# analyze results from the power analysis
require(reshape)
source("../power_functions.R")

p <- list()

scen3 <- get_scen_data("scen3")
tte_boxplot_power (scen3)
csv3 <- read.csv(file="scen3.csv")
calc_power(csv3)

scen3s <- get_scen_data("scen3_stop")
tte_boxplot_power (scen3s)
csv3s <- read.csv(file="scen3_stop.csv")
calc_power(csv3s)


## 2 new simulations
# - Dropout up to 50%
# - Dropout only for active arms
# - Condom use
#   in each progestron arm 10% use condom --> risk is 90% lower: 0.00525
#   what if 25-50% uses condom
# - Enrollment
