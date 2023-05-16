rm(list = ls())

a <- rnorm(100)

ar_ML_estimation <- function(xreal) {
  l <- length(xreal)
  param <- c(ar1 = 0) # set 0 as starting value. Other values also possible
  res <- vector()
  res[1] <- 0 # "zeroth" observation set to zero
  # define negative log-likelihood function
  neg_log_L <- function(param) {
    ar1 <- param[1]
    d <- vector() # vector which will be filled with densities
    d[1] <- dnorm(res[1], mean = 0, sd = 1) 
    # fill vector with evaluated densities
    for (i in 2:l) {
      res[i] <- xreal[i] - (ar1 * xreal[i - 1])
      d[i] <- dnorm(res[i], mean = 0, sd = 1)
    }
    nLL <- -sum(log(d)) # calculate negative log-likelihood
    return(nLL)
  }
  # "nlminb" -> Non-Linear MINimization with Bounds
  out <- nlminb(
    objective = neg_log_L,
    start = param,
    lower = c(-0.99),
    upper = c(0.99)
  )
  return(out)
}


# test: 

test <- rnorm(100)

ML_estimate <- ar_ML_estimation(test)
ML_estimate$par








