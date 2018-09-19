#' Creates a series of plots comparing slope parameters to presence of fish upstream.
#'
#' @title Plots slope parameter
#' @param upstream_slopes_test Table generated from the extract_slope_params function.
#' @param dim The dimensions your plots should appear in
#' @param species The species to plot
#' @param type Type of plot to create. Current options are "scatter" and "boxplot".
#'
#' @export

# Note: this function assumes that you want to plot all the slope parameters you derived for the upstream_slopes_test table.
#type="violin"
#upstream_slopes_test <- upstream_slopes
####### ---- Plot/model data #######################

plot_slope_params <- function(upstream_slopes_test, dimen, species, parameters_of_interest,main.lab = "",type="scatter") {
  par(mfrow = c(dimen[1],dimen[2]))
  n <- length(parameters_of_interest)
  upstream_slopes_test <- upstream_slopes_test[,c(species,parameters_of_interest)]
  upstream_slopes_test <- upstream_slopes_test[complete.cases(upstream_slopes_test),]

  # Create labels for x axis
  simpleCap <- function(x) {
    s <- strsplit(x, "_")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  xlab <- sapply(parameters_of_interest, simpleCap)


  for (i in 1:n){
    if (type=="scatter") {
    y.lab <- ifelse(i %in% seq(1,n,dimen[2]),"Presence/absence upstream","")
    plot(upstream_slopes_test[,species] ~ scale(upstream_slopes_test[,parameters_of_interest[i]]), xlab = xlab[i], ylab = y.lab, main = main.lab)
  } else if (type=="boxplot") {
    y.lab <- ifelse(i > (n-dimen[2]),"Presence/absence upstream","")
    boxplot(scale(upstream_slopes_test[,parameters_of_interest[i]]) ~ upstream_slopes_test[,species], xlab = y.lab, ylab = xlab[i], main = main.lab)
  }
    }
  }

# plot_slope_params(upstream_slopes_test)
