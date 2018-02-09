#' Creates a series of plots comparing slope parameters to presence of fish upstream.
#'
#' @param upstream_slopes_test Table generated from the extract_slope_params function.
#'
#' Note: this function assumes that you want to plot all the slope parameters you derived for the upstream_slopes_test table.

####### ---- Plot/model data #######################

plot_slope_params <- function(upstream_slopes_test) {
  par(mfrow = c(2,2))
  short_poi <- gsub("upstream_lakes_","",parameters_of_interest)

  for (i in 1:(length(parameters_of_interest)-1)){
    plot(data=upstream_slopes_test,Present ~ upstream_slopes_test[,5+i], xlab = short_poi[i+1], ylab = "Presence/absence upstream", main = paste("Perch presence/absence by",short_poi[i+1]))
  }

}

# plot_slope_params(upstream_slopes_test)
