#' Creates a series of plots showing significance of slope effect.
#'
#' @title Plots beta values of slope parameters
#' @param slope_analysis_fish The slope analysis table for your species
#' @param parameters_to_plot The parameters you wish to plot
#' @param species The species you fish to plot. Can be either "Pike" or "Perch".
#' @param DoS Distance or Slope. Indicates which parameter you want to plot.
#'
#' @return A graph showing significance of effects
#'
#' @export

#plot_beta(slope_analysis_perch,parameters_for_analysis,"Perch")
#slope_analysis_fish <- slope_analysis_perch
#species <- "Perch"
#parameters_to_plot <- c("slope_max", "slope_max_max")

plot_beta <- function(slope_analysis_fish,parameters_to_plot,species,DoS) {
  if (DoS == "slope") {
    fsum <- slope_analysis_fish$summaries$slope[parameters_to_plot,]
  } else {
    if (nrow(slope_analysis_fish$summaries$distance)==1) {
    fsum <- slope_analysis_fish$distance$distance
    } else {
      fsum <- slope_analysis_fish$summaries$distance[parameters_to_plot,]
    }
  }
  min.y <- min(fsum$`2.5%`)
  max.y <- max(c(fsum$`97.5%`),-0.5*min.y)
  plot(0, 0, xlim = c(0.5, nrow(fsum)+0.5), ylim = c(min.y, max.y), type = "n",xaxt='n',xlab='',
       ylab = paste0("Beta distribution (",species,")"))
  abline(h = 0,col = "grey")
  arrow_col <- character()
  for (i in 1:nrow(fsum)) {
    arrow_col[i] <- ifelse(fsum[i,"97.5%"]<0,"blue","black")
  }
  arrows(x0 = 1:nrow(fsum), x1 = 1:nrow(fsum), y0 = fsum[, "2.5%"], y1 = fsum[, "97.5%"], code = 3, angle = 90, length = 0.05,lwd=2,col=arrow_col)
  points(x = 1:nrow(fsum), y = fsum[, "50%"],col=arrow_col)
  axis(side=1,at = 1:nrow(fsum),labels=row.names(fsum),las=2,cex.axis=0.7)
}
