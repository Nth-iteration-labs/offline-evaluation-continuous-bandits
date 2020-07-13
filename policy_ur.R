UniformRandomPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "UniformRandomPolicy",
    initialize = function() {
      super$initialize()
    },
    get_action = function(t, context) {
      action$choice <- runif(1,0,1)
      action
    },
    set_reward = function(t, context, action, reward) {
    }
  )
)
