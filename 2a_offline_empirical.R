# This is the demo file for the online and offline parameter tuning of Lock-in Feedback
# For policy and bandit specific code, please look at the files (as sourced above).
# First make sure to install contextual 
# (see https://github.com/Nth-iteration-labs/contextual for a how to).
#
# For any questions, please contact the authors.

library(contextual)
library(here)
library(ggplot2)
library(cowplot)

source("./bandit_continuum_offline.R")
source("./policy_cont_lif_randstart.R")
source("./policy_tbl.R")
source("./policy_efirst.R")
source("./policy_ur.R")

#############################################################
#                                                           #
#  Online and Offline evaluation                            #
#                                                           #
#############################################################

### Set seed
set.seed(1)

### Set number of interactions (horizon) and number of repeats (simulations)
### In the paper we used a horizon of the data and 1000 simulations
simulations        <- 1000

### Data

dt <- read.table("results.csv", TRUE, sep=",")
dt <- dt[dt$type=="setreward",]
dt$store <- as.factor(dt$context.StoreID)

dt <- dt[dt$store=="15337",]

dt$choice <- dt$split
dt$reward <- (1 - dt$split) * dt$revenue

horizon <- nrow(dt)

### Set up different deltas for the delta method
deltas <- 0.1

### Parameters for LiF
int_time <- 10
amplitude <- 0.035
learn_rate <- 2*pi/int_time 
omega <- 1


### Set up all agents with different amplitudes and run them for each bandit
bandit <- OfflineContinuumBandit$new(data = dt, max_bool = TRUE, delta = deltas, horizon = horizon)

agents <- list(Agent$new(UniformRandomPolicy$new(), bandit),
             Agent$new(EpsilonFirstLinearRegressionPolicy$new(), bandit),
             Agent$new(LifPolicyRandstart$new(int_time, amplitude, learn_rate, omega), bandit),
             Agent$new(ThompsonBayesianLinearPolicy$new(), bandit))

history            <- Simulator$new(agents      = agents,
                                  horizon     = horizon,
                                  simulations = simulations,
                                  do_parallel = TRUE,
                                  save_interval = 10)$run()


cairo_ps("offline_empirical.eps")
plot(history, regret=FALSE, type="cumulative", legend_labels = c("UR", "E-First", "LiF", "TBL"), disp="ci", trunc_per_agent = FALSE)     
dev.off()

