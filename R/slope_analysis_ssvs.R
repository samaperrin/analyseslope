#' Function performs Bayesian analysis on the relationship between a slope parameter and the presence/absence of a fish species. This function also introduces an ssvs component, reducing the parameter effect of parameters which don't have too much impact on the model.
#'
#' @title Analyse slope
#' @param upstream_slopes_test Table derived from the extract_slope_params function.
#' @param parameters_for_analysis All slope parameters that you want to be evaluated. Note that these do not have to be all of the parameters in your upstream_slopes_test table.
#' @param species Species to analyse
#' @param include.distance Defines whether or not your analysis should include distance between focal lakes as well as slope
#' @param just.distance Defines whether or not you just want to analyse distance between focal lakes
#' @param n.iter All of these parameters (n.iter, n.burn, n.thin, n.chai and n.upda) concern your MCMC sampling. Defaults are 10000, 2000, 10, 3 and 20000, respectively.
#' @param interaction Specifies whether or not you want to include the interaction term in your model
#'
#' @return A) A summary table showing the means and confidence interval for the beta values of the effects of each slope parameter and distance between focal lakes on fish presence/absence and B) Full Bayesian analysis for each parameter.
#'
#' @export



#upstream_slopes_test <- upstream_slopes
#parameters_for_analysis <- names(upstream_slopes_test)[6:9]
#species <- "Perch"
#include.distance=TRUE
#just.distance=FALSE
#interaction=FALSE
#scale=TRUE
#gradient_analysis_pike <- slope_analysis(upstream_slopes_test,parameters_for_analysis,include.distance=TRUE,scale=TRUE,species="Pike")
#gradient_analysis_perch <- slope_analysis(upstream_slopes_test,parameters_for_analysis,include.distance=TRUE,scale=TRUE,species="Perch")
#sink()

slope_analysis_ssvs <- function(upstream_slopes_test, parameters_for_analysis, species, include.distance = FALSE, just.distance = FALSE, scale = FALSE, interaction=FALSE, n.iter=10000,n.thin=10,n.burn=2000,n.chai=3,n.upda=20000){

  if (just.distance==TRUE & include.distance==FALSE) {
    stop("You can't not include distance and have just distance.")
  }

  # Specify model in BUGS language
  if(interaction==TRUE) {
    cat(file = "Slope_Bayes_GLM.txt","
      model {

      # Priors
      for (i in 1:K) {
        ssvs.ind[k] ~ dbern(0.5)
        ssvs.Var[k] <- (1-ssvs.ind[k])*1e-06 + ssvs.ind[k]
        ssvs.tau[k] <- pow(ssvs.Var[k], -1)

        beta[i] ~ dnorm(0, 0.0001)}

      # Likelihood
      for (i in 1:nsite){
      C[i] ~ dbern(p[i])
      logit(p[i]) <- beta[1] + beta[2]*slope[i,] + beta[3]*distance[i] + beta[4]*slope[i,]*distance[i]
      }
      }
      ")
  } else {
      cat(file = "Slope_Bayes_GLM.txt","
      model {

      # Priors
      for (k in 1:K) {
        ssvs.ind[k] ~ dbern(0.5)
        ssvs.Var[k] <- (1-ssvs.ind[k])*1e-06 + ssvs.ind[k]
        ssvs.tau[k] <- pow(ssvs.Var[k], -1)

        beta[k] ~ dnorm(0, ssvs.tau[k])
      }

      # Likelihood
      for (i in 1:nsite){
        C[i] ~ dbern(p[i])
        logit(p[i]) <- inprod(beta[], X[i,])

      }
      }
      ")
  }

  # Identify # of parameters to be calculated
  param_length <- ifelse(just.distance==TRUE,1,length(parameters_for_analysis))

  # Create lists to populate later on
  z=list()
  y <- vector("list", ifelse(include.distance==FALSE,1,ifelse(just.distance==TRUE,1,ifelse(interaction==TRUE,3,2))))
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
    if(scale==TRUE) {slope <- scale(upstream_slopes_test[,parameters_for_analysis[i]]/100)
    } else {
      slope <- upstream_slopes_test[,parameters_for_analysis[i]]/100
    }
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
    K <-  ifelse(interaction==FALSE,ncol(X),ncol(X)+1)

    if(interaction==FALSE) {
    jags.data <- list(C = presence, X = X, nsite = nsites, K = K)
    } else {
      jags.data <- list(C = presence, distance = distance, slope= slope, nsite = nsites, K = K)
    }
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
  if (interaction==TRUE) {
  names(y) <- c(colnames(X)[-1],"interaction")
  } else {names(y) <- colnames(X)[-1]}
  for (k in 1:(K-1)) {colnames(y[[k]]) <- colnames(output_update$BUGSoutput$summary)}
  final_output <- list(all_data = z, summaries = y, data = upstream_slopes_test,species = species)
  return(final_output)
}

# slope_analysis_test <- slope_analysis(upstream_slopes_test, parameters_for_analysis)
# slope_analysis_test$slope_max_max$BUGSoutput$summary["beta",]
