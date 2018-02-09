###### ----- Function 4: Bayesian analysis ################

# parameters_for_analysis <- colnames(upstream_slopes_test)[6:7]

slope_analysis <- function(upstream_slopes_test, parameters_for_analysis){
  
  # Specify model in BUGS language
  cat(file = "Perch_Bayes_GLM.txt","
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
    presence <- upstream_slopes_test$Present
    nsites <- length(presence)
    site <- 1:nsites
    slope <- upstream_slopes_test[,parameters_for_analysis[i]]
    
    jags.data <- list(C = presence, nsite = nsites, slope = slope)
    
    # Initial values
    inits <- function () list(alpha = runif(1, -10, 10), beta = runif(1,-1,1))
    
    # Parameters monitored
    params <- c("alpha", "beta","p")
    
    # MCMC settings
    ni <- 50000
    nt <- 10
    nb <- 40000
    nc <- 3
    
    # Call winbugs from R
    print(paste("Running", parameters_for_analysis[i]))
    sink("/dev/null")
    output <- jags(jags.data, inits, params, "Perch_Bayes_GLM.txt", n.chains = nc,
                   n.thin = nt, n.burnin = nb, n.iter = ni)
    output_update <- update(output, n.iter = 100000, n.thin = 10)
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
