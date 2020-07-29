#' @export
OnlineOfflineContinuumBanditKernel <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    class_name = "OnlineOfflineContinuumBanditKernel",
    arm_function = NULL,
    choice = NULL,
    h = NULL,
    kernel = NULL,
    horizon = NULL,
    max_bool = FALSE,
    maxval = NULL,
    S = NULL,
    n = NULL,
    initialize   = function(FUN, max_bool, horizon) {
      self$arm_function <- FUN
      self$k <- 1
      self$horizon <- horizon
      self$h <- horizon^(-1/5)
      self$kernel <- function(action_true, action_choice, bandwith){ 1/sqrt(2*pi)*exp(((action_choice - action_true) / bandwith)^2/2) }
      self$max_bool <- max_bool
    },
    post_initialization = function() {
      self$choice <- runif(self$horizon, min=0, max=1)
      temp_data <- self$arm_function(self$choice)
      if(self$max_bool == TRUE){
          self$S <- data.frame(self$choice, temp_data$data)
          self$maxval <- temp_data$max
      } else {
          self$S <- data.frame(self$choice, temp_data)
      }
      self$S <- self$S[sample(nrow(self$S)),]
      colnames(self$S) <- c('choice', 'reward')
      self$n <- 0
    },
    get_context = function(index) {
      context           <- list()
      context$k         <- self$k
      context
    },
    get_reward = function(index, context, action) {
      reward_at_index <- as.double(self$S$reward[[index]])
      temp_u <- (action$choice - self$S$choice[[index]]) / self$h
      kern_value <- 1/sqrt(2*pi) * exp(-temp_u^2 / 2)
      reward <- list(
        reward = (kern_value * reward_at_index),
        optimal_reward = ifelse(self$max_bool, self$maxval, NA)
      )
    }
  )
)
