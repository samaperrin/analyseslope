#' Creates a series of plots showing significance of slope effect.
#'
#' @title Plots beta values of slope parameters
#' @param slope_analysis_fish The slope analysis table for your species
#' @param parameters_to_plot The parameters you wish to plot
#'
#'
#' @return A graph showing significance of effects
#'
#' @export

plot_beta <- function(slope_analysis_fish,parameters_to_plot) {
  fsum <- slope_analysis_fish$summary[parameters_to_plot,]
  min.y <- min(fsum$`2.5%`)
  max.y <- max(c(fsum$`97.5%`),0.5)
  plot(0, 0, xlim = c(1, length(parameters_to_plot)), ylim = c(min.y, max.y), type = "n",xaxt='n',xlab='',
       ylab = "Beta distribution", main="")
  abline(h = 0,col = "grey")
  arrows(x0 = 1:length(parameters_to_plot), x1 = 1:length(parameters_to_plot), y0 = fsum[, "2.5%"], y1 = fsum[, "97.5%"], code = 3, angle = 90, length = 0.05,lwd=2)
  axis(side=1,at = 1:length(parameters_to_plot),labels=parameters_to_plot,las=2,cex.axis=0.7)
}
