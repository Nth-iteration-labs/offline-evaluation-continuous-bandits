#' @export
OnlineOfflineContinuumBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    class_name = "OnlineOfflineContinuumBandit",
    delta = NULL,
    horizon = NULL,
    choice = NULL,
    arm_function = NULL,
    max_bool = FALSE,
    maxval = NULL,
    S = NULL,
    initialize   = function(FUN, max_bool, delta, horizon) {
      self$arm_function <- FUN
      self$horizon <- horizon
      self$delta <- delta
      self$k <- 1
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
    },
    get_context = function(index) {
      context           <- list()
      context$k         <- self$k
      context
    },
    get_reward = function(index, context, action) {
      reward_at_index <- as.double(self$S$reward[[index]])
      if (abs(self$S$choice[[index]] - action$choice) < self$delta) {
        reward <- list(
                  reward = reward_at_index,
                  optimal_reward = ifelse(self$max_bool, self$maxval, NA)
                  )
      } else {
        NULL
      }
    }
  )
)
