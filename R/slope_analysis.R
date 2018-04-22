#' Function performs Bayesian analysis on the relationship between a slope parameter and the presence/absence of a fish species.
#'
#' @title Analyse slope
#' @param upstream_slopes_test Table derived from the extract_slope_params function.
#' @param parameters_for_analysis All slope parameters that you want to be evaluated. Note that these do not have to be all of the parameters in your upstream_slopes_test table.
#' @param species Species to analyse
#' @return A) A summary table showing the means and confidence interval for the beta values of the effects of each slope parameter on fish presence/absence and B) Full Bayesian analysis for each parameter.
#' @export



#upstream_slopes_test <- upstream_slopes
#parameters_for_analysis <- names(upstream_slopes_test)[6:9]
#species <- "Perch"
#include.distance=TRUE
#just.distance=FALSE
#slope_analysis_pike <- slope_analysis(upstream_slopes_test,parameters_for_analysis,include.distance=TRUE,species="Pike")
#slope_analysis_perch <- slope_analysis(upstream_slopes_test,parameters_for_analysis,include.distance=TRUE,species="Perch")
#sink()

slope_analysis <- function(upstream_slopes_test, parameters_for_analysis, include.distance = FALSE, just.distance = FALSE, species, n.iter=10000,n.thin=10,n.burn=2000,n.chai=3,n.upda=20000){

  if (just.distance==TRUE & include.distance==FALSE) {
    stop("You can't not include distance and have just distance, you derp.")
  }

  # Specify model in BUGS language
      cat(file = "Slope_Bayes_GLM.txt","
      model {

      # Priors
      for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}

      # Likelihood
      for (i in 1:nsite){
      C[i] ~ dbern(p[i])
      logit(p[i]) <- inprod(beta[], X[i,])
      }
      }
      ")

  param_length <- ifelse(just.distance==TRUE,1,length(parameters_for_analysis))
  z=list()
  y <- vector("list", ifelse(include.distance==FALSE,1,ifelse(just.distance==TRUE,1,2)))
  for (k in 1:length(y)) {
  y[[k]] <- as.data.frame(matrix(NA,nrow=param_length,ncol=9))
  if (just.distance==TRUE) {
  rownames(y[[k]]) <- "distance"
  } else {
    rownames(y[[k]]) <- parameters_for_analysis
  }
  }



  for(i in 1:param_length)
  {
    presence <- upstream_slopes_test[,species]
    nsites <- length(presence)
    site <- 1:nsites
    slope <- upstream_slopes_test[,parameters_for_analysis[i]]/100
    distance <- log(upstream_slopes_test[,"total_stream_length"])
    if (include.distance==FALSE) {
      X <- model.matrix(~ + slope)
    } else {
      if (just.distance==TRUE) {
        X <- model.matrix(~ + distance)
      } else {
        X <- model.matrix(~ + slope + distance)
      }
    }
    K <-  ncol(X)

    jags.data <- list(C = presence, X = X, nsite = nsites, K = K)
    # Initial values
    inits  <- function () {
      list(
        beta = rnorm(K, 0, 0.00001))  }

        # Parameters monitored
    params <- c("beta","p")


    # Call winbugs from R
    print(paste("Running parameter",i))
    sink("/dev/null")
    output <- jags(jags.data, inits, params, "Slope_Bayes_GLM.txt", n.chains = n.chai,
                   n.thin = n.thin, n.burnin = n.burn, n.iter = n.iter)
    sink()
    print(paste("Running parameter",i,"update"))
    sink("/dev/null")
    output_update <- update(output, n.iter = n.upda, n.thin = n.thin)
    sink()

    # Need to fix this so we get
    z[[i]] <- output_update
    for (k in 1:(K-1)) {
    y[[k]][i,] <- round(output_update$BUGSoutput$summary[k+1,],5)
    }
  }
  #output_short <- round(output_update,dig=3)
  if(just.distance==TRUE) {
    names(z) <- "distance"
  } else {names(z) <- parameters_for_analysis}
  names(y) <- colnames(X)[-1]
  for (k in 1:(K-1)) {colnames(y[[k]]) <- colnames(output_update$BUGSoutput$summary)}
  final_output <- list(all_data = z, summaries = y)
  return(final_output)
}

# slope_analysis_test <- slope_analysis(upstream_slopes_test, parameters_for_analysis)
# slope_analysis_test$slope_max_max$BUGSoutput$summary["beta",]
