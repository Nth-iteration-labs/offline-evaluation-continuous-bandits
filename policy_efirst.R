EpsilonFirstLinearRegressionPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "EpsilonFirstLinearRegressionPolicy",
    b = NULL,
    A = NULL,
    epsilon = NULL,
    initialize = function(b = matrix(c(0, 0, 0), nrow=1, ncol=3, byrow = TRUE),
                          A = matrix(diag(c(1,1,1)), nrow=3, ncol=3, byrow = TRUE),
                          epsilon = 2000
                          ) {
      super$initialize()
      self$b <- b
      self$A <- A
      self$epsilon <- epsilon
    },
    set_parameters = function(context_params) {
      self$theta <- list('b' = self$b, 'A' = self$A, 'n' = 0)
    },
    get_action = function(t, context) {
      if(self$theta$n <= self$epsilon){
          action$choice <- runif(1,0,1)
      } else{
          betas <- solve(self$theta$A, tol = 1e-200)%*% matrix(self$theta$b)
          action$choice <- -(betas[2] / (2*betas[3]))
          if(action$choice > 1){
            action$choice <- 1
          } else if(action$choice < 0) {
            action$choice <- 0
          }
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      if(self$theta$n <= self$epsilon){
          y <- reward$reward
          x <- action$choice
          x <- matrix(c(1,x,x^2), nrow = 1, ncol = 3, byrow = TRUE)
          self$theta$b <- (x*y) + self$theta$b
          self$theta$A <- t(x)%*%x + self$theta$A
      }
      self$theta$n <- self$theta$n + 1
      self$theta
    }
  )
)
