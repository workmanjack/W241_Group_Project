#install.packages("pwr")
library(pwr)

# Parameters:
ate <- 1 # effect size
sigma <- 1 # standard deviation
alpha <- 0.05
beta <- 0.1 # 1-power

# Manual calculation:
#z_alpha <- qnorm(1-alpha/2) # two-sided
#z_alpha <- qnorm(1-alpha) # one-sided
#z_beta <- qnorm(1-beta)
#n <- (4*((z_alpha + z_beta)**2)*sigma**2)/ate**2

# Built-in formula
d = ate/sigma # assuming sigma is pooled standard deviation (equal variance)
pwr.t.test(d = d, sig.level = alpha, power = 1-beta, type = 'two.sample', alternative = 'greater')


