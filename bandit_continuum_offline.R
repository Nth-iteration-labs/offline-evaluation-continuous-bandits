#' @export
OfflineContinuumBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    class_name = "OfflineContinuumBandit",
    delta = NULL,
    horizon = NULL,
    choice = NULL,
    S = NULL,
    initialize   = function(data, max_bool, delta, horizon) {
      self$S <- data 
      self$horizon <- horizon
      self$delta <- delta
      self$k <- 1
    },
    post_initialization = function() {
      self$S <- self$S[sample(nrow(self$S)),]
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
              reward = reward_at_index
            )
      } else {
        NULL
      }
    }
  )
)
