#' Calculates likelihood of a fish species to disperse upstream over a given slope.
#'
#' @title Likelihood of dispersal calculation
#' @param slopes List of lakes with their slopes upstream
#' @param slope_analysis_species Bayesian slope analysis given by slope_analysis function
#' @param parameter Slope parameter you wish to evaluate likelihood based on
#'
#' @return Table with lake ids and slopes, with likelihood of dispersal attached
#' @export
#'
#'
#slopes_analysis_species <- gradient_analysis_perch
#parameter <- "gradient_mean"
#slopes

dispersal_likelihood <- function(slopes, slopes_analysis_species, parameter, scaled.model=TRUE,upstream_slopes) {
  if (scaled.model = TRUE)
  {mean.oldslopes <- mean(slopes_analysis_species$data[,parameter])
  sd.oldslopes <- sd(slopes_analysis_species$data[,parameter])
    slopes$new_slopes <- (slopes$new_slopes-mean.oldslopes)/sd.oldslopes}
  slopes$new_distances <- log(slopes$new_distances)

  q_parameter <- slopes_analysis_species$all_data[[parameter]]
  b1 <- q_parameter$BUGSoutput$mean$beta[1]
  b2 <- q_parameter$BUGSoutput$mean$beta[2]
  b3 <- q_parameter$BUGSoutput$mean$beta[3]

  s <- slopes$new_slopes
  d <- slopes$new_distances
  eq <- c(b1) + s*c(b2) + d*c(b3)
  likeli <- round(exp(eq)/(1+exp(eq)),digits=4)
  c <- as.data.frame(cbind(slopes,likeli))
  return(c)
}

# Make fake slope data for testing
# new_slopes <- c(0.073,0.012,0.003,0.021,0.123,0.311,0.065,0.113)
# lakeid <- seq(1,length(new_slopes),1)
# slopes <- as.data.frame(cbind(slope_names,new_slopes))

# Use pike and slope_perc_90 for this example
# slopes_analysis_species <- slope_analysis_pike
# parameter <- "slope_perc_90_max"

# dispersal_likelihood(slopes, slope_analysis_pike, "slope_perc_90_max")

