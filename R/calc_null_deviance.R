#' Function returns null deviance for a model.
#'
#' @title Get pseudo R^2
#' @param gradient_analysis Gradient analysis table derived from slope_analysis function.
#' @param null_deviance Null deviance value derived from calc_null_deviance function.
#' @param parameter Parameter to analyse.
#'
#' @return Your pseudo R^2 value.

#gradient_analysis <- gradient_analysis_perch_justdistance
#parameter <- "distance"

calc_null_deviance <- function(upstream_slopes, species) {

  # Run null model
  cat(file = "Null_Bayes_GLM.txt","
      model {

      # Priors
      beta ~ dnorm(0, 0.0001)

      # Likelihood
      for (i in 1:nsite){
      C[i] ~ dbern(p[i])
      logit(p[i]) <- beta
      }
      }
      ")
  species_presence <- upstream_slopes[,species]
  nsites <- length(presence)
  site <- 1:nsites
  jags.data <- list(C = species_presence, nsite = nsites)
  inits  <- function () {
    list(
      beta = rnorm(1, 0, 0.00001))  }
  params <- c("beta")
  sink("/dev/null")
  species_output <- jags(jags.data, inits, params, "Null_Bayes_GLM.txt", n.chains = 1,
                       n.thin = 30, n.burnin = 10000, n.iter = 40000)
  sink()

  # Calculate null deviance
  null_deviance <- species_output$BUGSoutput$mean$deviance
  return(null_deviance)
}
