#' Creates a series of plots comparing slope parameters to presence of fish upstream.
#'
#' @param upstream_slopes_test Table generated from the extract_slope_params function.
#' @param dim The dimensions your plots should appear in
#' @param species The species to plot
#'
#' @export

# Note: this function assumes that you want to plot all the slope parameters you derived for the upstream_slopes_test table.

####### ---- Plot/model data #######################

plot_slope_params <- function(upstream_slopes_test, dim, species) {
  par(mfrow = c(dim[1],dim[2]))
  short_poi <- gsub("upstream_lakes_","",parameters_of_interest)

  for (i in 1:(length(parameters_of_interest)-1)){
    plot(upstream_slopes_test[,species] ~ upstream_slopes_test[,short_poi[-1][i]], xlab = short_poi[i+1], ylab = "Presence/absence upstream", main = paste(species, "presence/absence by",short_poi[i+1]))
  }

}

# plot_slope_params(upstream_slopes_test)
