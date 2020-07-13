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

source("./bandit_continuum_function_unimodal.R")
source("./bandit_continuum_function_bimodal.R")
source("./bandit_continuum_offon.R")
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
### In the paper we used a horizon of 10000 and 10000 simulations
horizon            <- 10
simulations        <- 1

### Set up two different bandits
bandits <- c(ContinuumBanditUnimodal$new(), ContinuumBanditBimodal$new())

### Set up functions to make offline dataset
unimodal_data <- function(x){
    c1 <- runif(1, 0.25, 0.75)
    c2 <- 1
    return(list("data" = -(x - c1) ^2 + c2 + rnorm(length(x), 0, 0.01), "max" = c2))
}

bimodal_data <- function(x){
    mu1 <- runif(1, 0.15, 0.2)
    sd1 <- runif(1, 0.1, 0.15)
    mu2 <- runif(1, 0.7, 0.85)
    sd2 <- runif(1, 0.1, 0.15)
    y1 <- truncnorm::dtruncnorm(x, a=0, b=1, mean=mu1, sd=sd1)
    y2 <- truncnorm::dtruncnorm(x, a=0, b=1, mean=mu2, sd=sd2)
    maxval <- truncnorm::dtruncnorm(mu2, a=0, b=1, mean=mu1, sd=sd1) + truncnorm::dtruncnorm(mu2, a=0, b=1, mean=mu2, sd=sd2)
    return(list("data" = y1 + y2 + rnorm(length(x), 0, 0.01), "max" = maxval))
}

functions <- list(list("unimodal", bimodal_data))#, list("bimodal", bimodal_data))

### Set up different deltas for the delta method. If delta = 0 we do online
#deltas <- c(0, 0.01, 0.05, 0.1, 0.2, 0.5)
deltas <- c(0, 0.5, 0.2, 0.1, 0.05, 0.01)

### Parameters for LiF
int_time <- 10
amplitude <- 0.035
learn_rate <- 2*pi/int_time 
omega <- 1

histories <- vector(mode='list', length=length(deltas))

### Set up all agents with different amplitudes and run them for each bandit
for (f in functions){
  if(f[[1]] == "unimodal"){
      bandit_online <- ContinuumBanditUnimodal$new()
  } else {
      bandit_online <- ContinuumBanditBimodal$new()
  }
  for (i in 1:length(deltas)){
      d = deltas[i]
      if(d == 0){
          bandit <- bandit_online
      } else {
          bandit <- OnlineOfflineContinuumBandit$new(FUN = f[[2]], max_bool = TRUE, delta = d, horizon = horizon)
      }

      agents <- list(Agent$new(UniformRandomPolicy$new(), bandit),
                     Agent$new(EpsilonFirstLinearRegressionPolicy$new(), bandit),
                     Agent$new(LifPolicyRandstart$new(int_time, amplitude, learn_rate, omega), bandit),
                     Agent$new(ThompsonBayesianLinearPolicy$new(), bandit))

      history            <- Simulator$new(agents      = agents,
                                          horizon     = horizon,
                                          simulations = simulations,
                                          do_parallel = TRUE,
                                          save_interval = 10)$run()
      
      histories[[i]] <- history 
 }
 
#pdf(paste0("offline_",f[[1]],".pdf"))

#layout(matrix(c(1,2,3,4,5,6,7,7,7), ncol=3, byrow=TRUE), heights=c(3, 3, 1))
#par(oma=c(4,4,2,2), las=1)
##par(cex=1.05)
#for(hist in histories){
#    plot(hist, type="cumulative", no_par = TRUE, disp="ci", legend=FALSE, use_colors=TRUE, trunc_per_agent = FALSE, xlab = "", ylab = "")
#}
#par(cex=1.05)
#mtext("Time step", 1, 3, outer=TRUE, las = 0)
#mtext("Cumulative regret", 2, 3, outer=TRUE, las = 0)
#par(mar=c(1,1,1,1))
#plot.new()
#legend(x="center", ncol=4,legend=c("UR","E-First","LiF", "TBL"), fill=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), title="Legend")
#
#dev.off()
}

