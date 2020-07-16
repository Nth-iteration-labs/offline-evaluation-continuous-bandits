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

agents <- list(Agent$new(UniformRandomPolicy$new(), bandit, name = "UR"),
             Agent$new(EpsilonFirstLinearRegressionPolicy$new(epsilon = 100), bandit, name = "E-First"),
             Agent$new(LifPolicyRandstart$new(int_time, amplitude, learn_rate, omega), bandit, name = "LiF"),
             Agent$new(ThompsonBayesianLinearPolicy$new(), bandit, name = "TBL"))

history            <- Simulator$new(agents      = agents,
                                  horizon     = horizon,
                                  simulations = simulations,
                                  do_parallel = TRUE,
                                  save_interval = 10)$run()


history$update_statistics()

history_cumulative <- history$get_cumulative_data()
colnames(history_cumulative)[which(names(history_cumulative) == "agent")] <- "Policy"

g <- ggplot(history_cumulative) + 
     geom_line(aes(y = cum_reward, x = t, color=Policy)) +
     geom_ribbon(aes(ymin = cum_reward - cum_reward_ci, ymax = cum_reward + cum_reward_ci, x = t, fill=Policy), alpha = 0.2, show.legend=FALSE) +
     ylab("Cumulative reward") +
     xlab("Time") +
     theme(legend.position = "none") +
     theme_bw(base_size = 15) 
ggsave("offline_empirical.pdf", g, device="pdf", width=10, height=7)
print(g)
