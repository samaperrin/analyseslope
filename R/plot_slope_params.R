#' Creates a series of plots comparing slope parameters to presence of fish upstream.
#'
#' @title Plots slope parameter
#' @param upstream_slopes_test Table generated from the extract_slope_params function.
#' @param dim The dimensions your plots should appear in
#' @param species The species to plot
#'
#' @export

# Note: this function assumes that you want to plot all the slope parameters you derived for the upstream_slopes_test table.

####### ---- Plot/model data #######################

plot_slope_params <- function(upstream_slopes_test, dimen, species, parameters_of_interest,main.lab = "") {
  par(mfrow = c(dimen[1],dimen[2]))
  n <- length(parameters_of_interest)
  for (i in 1:n){
    y.lab <- ifelse(i %in% seq(1,n,dimen[2]),"Presence/absence upstream","")
    plot(upstream_slopes_test[,species] ~ upstream_slopes_test[,parameters_of_interest[i]], xlab = parameters_of_interest[i], ylab = y.lab, main = main.lab)
  }

}

# plot_slope_params(upstream_slopes_test)
