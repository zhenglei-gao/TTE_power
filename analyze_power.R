## analyze results from the power analysis
## (for run1a)
require(reshape)

csv1 <- rbind(read.csv(file="scen1.csv"), read.csv(file="scen1b.csv"))
csv1_stop <- rbind(read.csv(file="scen1_stop.csv"), read.csv(file="scen1b_stop.csv"))
colnames(csv1) <- c("Test combined", "Test HC1", "Test HC2", "Test HC3")
colnames(csv1_stop) <- c("Test combined", "Test HC1", "Test HC2", "Test HC3")

csv1_m <- melt(csv1)
csv1s_m <- melt(csv1_stop)

cols <- c("#444499","#994444","#994444","#994444")

## Boxplot & power calculation
tte_boxplot_power (csv1_m)
calc_power(csv1)
calc_power(csv1_stop)

####################################################
## Old versions of plots 

# histogram
ggplot(csv1_m, aes(x=log(value), group=variable, colour=variable, fill=variable)) + 
  geom_histogram() +
  geom_vline (xintercept = log(c(0.05, 0.017)), linetype=c(1,2))

# densityplot
ggplot(csv1_m, aes(x=log(value), group=variable, colour=variable, fill=NULL)) + 
  geom_vline (xintercept = log(c(0.05, 0.017)), linetype=c(1,3), size=1, colour="#555555") +
  geom_density(size=1.5) +
  geom_rug() +
  scale_colour_manual(values=cols) +
  xlab ("Log (p-value)") + ylab("Density")

# densityplot (with stop-criterion)
ggplot(csv1s_m, aes(x=log(value), group=variable, colour=variable, fill=NULL)) + 
  geom_vline (xintercept = log(c(0.05, 0.017)), linetype=c(1,3), size=1, colour="#555555") +
  geom_density(size=1.5) +
  geom_rug() +
  scale_colour_manual(values=c("#4444BB","#BB4444","#BB4444","#BB4444")) + 
  xlab ("Log (p-value)") + ylab("Density")

# boxplot+jitter 
ggplot(csv1_m, aes(y=log(value), x=variable, colour=variable, fill=NULL)) + 
  geom_hline (yintercept = log(c(0.05, 0.017)), linetype=c(1,3), size=1, colour="#555555") +
  geom_boxplot() +
  geom_point(position = position_jitter(w = 0.15, h = 0), size=3) +
  scale_colour_manual(values=cols) +
  ylab ("Log (p-value)") +
  annotate(geom="text", x=4.4, y=log(.05*1.4), label="0.05", colour="#444444", size=4) +
  annotate(geom="text", x=4.4, y=log(.017*1.4), label="0.017", colour="#444444", size=4) 

# boxplot+jitter (with stop criterion)
ggplot(csv1s_m, aes(y=log(value), x=variable, colour=variable, fill=NULL)) + 
  geom_hline (yintercept = log(c(0.05, 0.017)), linetype=c(1,3), size=1, colour="#555555") +
  geom_boxplot() +
  geom_point(position = position_jitter(w = 0.15, h = 0), size=3) +
  scale_colour_manual(values=cols) +
  ylab ("Log (p-value)") +
  annotate(geom="text", x=4.4, y=log(.05*1.4), label="0.05", colour="#444444", size=4) +
  annotate(geom="text", x=4.4, y=log(.017*1.4), label="0.017", colour="#444444", size=4) 
  
