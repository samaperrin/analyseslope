#' Function performs Bayesian analysis on the relationship between a slope parameter and the presence/absence of a fish species.
#'
#' @title Analyse slope
#' @param upstream_slopes_test Table derived from the extract_slope_params function.
#' @param parameters_for_analysis All slope parameters that you want to be evaluated. Note that these do not have to be all of the parameters in your upstream_slopes_test table.
#' @param species Species to analyse
#' @return A) A summary table showing the means and confidence interval for the beta values of the effects of each slope parameter on fish presence/absence and B) Full Bayesian analysis for each parameter.
#' @export



#parameters_for_analysis <- names(upstream_slopes_test)[6:7]

slope_analysis <- function(upstream_slopes_test, parameters_for_analysis, species){

  # Specify model in BUGS language
  cat(file = "Slope_Bayes_GLM.txt","
      model {

      # Priors
      beta ~ dnorm(0, 0.00001)        # Prior for slopes

      alpha ~ dnorm(0, 1.0E-06)       # Prior for intercepts

      # Likelihood
      for (i in 1:nsite){
      C[i] ~ dbern(p[i])
      logit(p[i]) <- alpha + beta*slope[i]
      }
      }
      ")

  z=list()
  y=as.data.frame(matrix(NA,nrow=length(parameters_for_analysis),ncol=9))

  for(i in 1:length(parameters_for_analysis))
  {
    presence <- upstream_slopes_test[,species]
    nsites <- length(presence)
    site <- 1:nsites
    slope <- upstream_slopes_test[,parameters_for_analysis[i]]/100

    jags.data <- list(C = presence, nsite = nsites, slope = slope)

    # Initial values
    inits <- function () list(alpha = runif(1, -10, 10), beta = runif(1,-1,1))

    # Parameters monitored
    params <- c("alpha", "beta","p")

    # MCMC settings
    ni <- 10000
    nt <- 10
    nb <- 5000
    nc <- 3

    # Call winbugs from R
    print(paste("Running", parameters_for_analysis[i]))
    sink("/dev/null")
    output <- jags(jags.data, inits, params, "Slope_Bayes_GLM.txt", n.chains = nc,
                   n.thin = nt, n.burnin = nb, n.iter = ni)
    sink()
    print(paste("Running", parameters_for_analysis[i],"update"))
    sink("/dev/null")
    output_update <- update(output, n.iter = 50000, n.thin = 10)
    sink()
    z[[i]] <- output_update
    y[i,] <- output_update$BUGSoutput$summary["beta",]
  }
  #output_short <- round(output_update,dig=3)
  names(z) <- parameters_for_analysis
  colnames(y) <- colnames(output_update$BUGSoutput$summary)
  rownames(y) <- parameters_for_analysis
  final_output <- list(all_data = z, summary = y)
  return(final_output)
}

# slope_analysis_test <- slope_analysis(upstream_slopes_test, parameters_for_analysis)
# slope_analysis_test$slope_max_max$BUGSoutput$summary["beta",]
