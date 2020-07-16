# This is the demo file for the online and offline parameter tuning of Lock-in Feedback
# For policy and bandit specific code, please look at the files (as sourced above).
# First make sure to install contextual 
# (see https://github.com/Nth-iteration-labs/contextual for a how to).
#
# For any questions, please contact the authors.

library(contextual)
library(here)
library(ggplot2)
library(patchwork)

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
set.seed(333)

### Set number of interactions (horizon) and number of repeats (simulations)
### In the paper we used a horizon of 10000 and 10000 simulations
horizon            <- 10000
simulations        <- 1000

### Set up functions to make offline dataset
unimodal_data <- function(x){
    c1 <- runif(1, 0.25, 0.75)
    c2 <- 1
    return(list("data" = -(x - c1) ^2 + c2 + rnorm(length(x), 0, 0.05), "max" = c2))
}

bimodal_data <- function(x){
    mu1 <- runif(1, 0.15, 0.2)
    sd1 <- runif(1, 0.05, 0.15)
    mu2 <- runif(1, 0.7, 0.85)
    sd2 <- runif(1, 0.05, 0.15)
    y1 <- truncnorm::dtruncnorm(x, a=0, b=1, mean=mu1, sd=sd1)
    y2 <- truncnorm::dtruncnorm(x, a=0, b=1, mean=mu2, sd=sd2)
    if (sd2 >= sd1) {
        maxval <- truncnorm::dtruncnorm(mu2, a=0, b=1, mean=mu1, sd=sd1) + truncnorm::dtruncnorm(mu2, a=0, b=1, mean=mu2, sd=sd2)
    } else {
        maxval <- truncnorm::dtruncnorm(mu1, a=0, b=1, mean=mu1, sd=sd1) + truncnorm::dtruncnorm(mu2, a=0, b=1, mean=mu2, sd=sd2)
    }
    return(list("data" = y1 + y2 + rnorm(length(x), 0, 0.05), "max" = maxval))
}

functions <- list(list("unimodal", unimodal_data), list("bimodal", bimodal_data))

### Set up different deltas for the delta method. If delta = 0 we do online
#deltas <- c(0, 0.01, 0.05, 0.1, 0.2, 0.5)
deltas <- c(0, 0.5, 0.2, 0.1, 0.05, 0.01)

### Parameters for LiF
int_time <- 25
amplitude <- 0.05
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

      agents <- list(Agent$new(UniformRandomPolicy$new(), bandit, name = "UR"),
                     Agent$new(EpsilonFirstLinearRegressionPolicy$new(), bandit, name = "E-First"),
                     Agent$new(LifPolicyRandstart$new(int_time, amplitude, learn_rate, omega), bandit, name = "LiF"),
                     Agent$new(ThompsonBayesianLinearPolicy$new(), bandit, name = "TBL"))

      history            <- Simulator$new(agents      = agents,
                                          horizon     = horizon,
                                          simulations = simulations,
                                          do_parallel = TRUE,
                                          save_interval = 10)$run()
      
      history$update_statistics()

      histories[[i]] <- history$get_cumulative_data()
      colnames(histories[[i]])[which(names(histories[[i]]) == "agent")] <- "Policy"
 }

plots <- vector(mode='list', length=length(deltas))

for(i in 1:length(deltas)){
    if(deltas[i] == 0){
        g <- ggplot(histories[[i]]) + 
             geom_line(aes(y = cum_regret, x = t, color=Policy)) +
             geom_ribbon(aes(ymin = cum_regret - cum_regret_ci, ymax = cum_regret + cum_regret_ci, x = t, fill=Policy), alpha = 0.2, show.legend=FALSE) +
             ylab("Cumulative regret") +
             xlab("Time") +
             theme(legend.position = "none") +
             theme_bw(base_size = 15) +
             ggtitle("Online")
        plots[[i]] <- g
    } else { 
        g <- ggplot(histories[[i]]) +
             geom_line(aes(y = cum_regret, x = t, color=Policy)) +
             geom_ribbon(aes(ymin = cum_regret - cum_regret_ci, ymax = cum_regret + cum_regret_ci, x = t, fill=Policy), alpha = 0.2, show.legend=FALSE) +
             ylab("Cumulative regret") +
             xlab("Time") +
             theme(legend.position = "none") +
             theme_bw(base_size = 15) +
             ggtitle(paste0("Delta ", deltas[i]))
        plots[[i]] <- g
    }

}

p <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]] + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(f[[1]],"_offline.pdf"), p, device="pdf", width=10, height=7)

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

