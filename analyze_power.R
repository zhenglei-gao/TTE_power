# analyze results from the power analysis
require(reshape)

csv1 <- rbind(read.csv(file="scen1.csv"), read.csv(file="scen1b.csv"))
csv1_stop <- rbind(read.csv(file="scen1_stop.csv"), read.csv(file="scen1b_stop.csv"))
colnames(csv1) <- c("Test all", "Test HC1", "Test HC2", "Test HC3")
colnames(csv1_stop) <- c("Test all", "Test HC1", "Test HC2", "Test HC3")

csv1_m <- melt(csv1)
csv1s_m <- melt(csv1_stop)

ggplot(csv1_m, aes(x=log(value), group=variable, colour=variable, fill=variable)) + 
  geom_histogram() +
  geom_vline (xintercept = log(c(0.05, 0.017)), linetype=c(1,2))

ggplot(csv1_m, aes(x=log(value), group=variable, colour=variable, fill=NULL)) + 
  geom_vline (xintercept = log(c(0.05, 0.017)), linetype=c(1,3), size=1, colour="#555555") +
  geom_density(size=1.5) +
  geom_rug() +
  scale_colour_manual(values=c("#4444BB","#BB4444","#BB4444","#BB4444")) +
  xlab ("Log (p-value)") + ylab("Density")

ggplot(csv1s_m, aes(x=log(value), group=variable, colour=variable, fill=NULL)) + 
  geom_vline (xintercept = log(c(0.05, 0.017)), linetype=c(1,3), size=1, colour="#555555") +
  geom_density(size=1.5) +
  geom_rug() +
  scale_colour_manual(values=c("#4444BB","#BB4444","#BB4444","#BB4444")) + 
  xlab ("Log (p-value)") + ylab("Density")

sum(csv1[["Test HC1"]] < 0.017) / length(csv1[,1])
sum(csv1[["Test HC2"]] < 0.017) / length(csv1[,1])
sum(csv1[["Test HC3"]] < 0.017) / length(csv1[,1])

# non-parametric and parametric (assuming normality of log(p)) power estimate
power <- list (
  "test1_nonpar" = sum(csv1[["Test all"]] < 0.05) / length(csv1[,1]),
  "test2_nonpar" = sum(apply((csv1[,c(2:4)] < 0.017), 1, sum) == 3) / length(csv1[,2]), # any of the 3 tests is positive
  "test1_par" = pnorm(log(0.05), mean(log(csv1[,1])), sd(log(csv1[,1]))),
  "test2_nonpar" = pnorm(log(0.017), mean(log(csv1[,2])), sd(log(csv1[,2]))) *
                   pnorm(log(0.017), mean(log(csv1[,3])), sd(log(csv1[,3]))) *
                   pnorm(log(0.017), mean(log(csv1[,4])), sd(log(csv1[,4]))) 
)
