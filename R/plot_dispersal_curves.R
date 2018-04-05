#' Creates a plot showing a series of parameter curves.
#'
#' @title Plots dispersal curves
#' @param your_upstream_slopes Table generated from the extract_slope_params function.
#' @param slope_analysis_fish The slope analysis table for your species
#' @param species The species to plot
#' @param slope_parameters The parameters you wish to plot
#'
#' @export

#test
#slope_analysis_fish <- slope_analysis_perch
#your_upstream_slopes <- upstream_slopes
#species <- "Perch"
#slope_parameters <- c("slope_mean","slope_max","slope_max_max")

#plot_dispersal_curves(your_upstream_slopes, slope_analysis_fish, species, slope_parameters)

plot_dispersal_curves <- function(your_upstream_slopes, slope_analysis_fish, species, slope_parameters){
  n <- length(slope_parameters)
  x.limit <- max(your_upstream_slopes[,slope_parameters])
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

  plot(your_upstream_slopes[,species] ~ your_upstream_slopes[,slope_parameters[1]],type='n',ylab=paste0(species," Presence/Absence"),xlab="Slope in degrees x 100",xlim=c(0,x.limit))
    for(i in 1:length(slope_parameters)){
    x <- your_upstream_slopes[,slope_parameters[i]]
    order.x <- order(x)
    pre.y <- slope_analysis_fish$all_data[[slope_parameters[i]]]
    y <- pre.y$BUGSoutput$mean$p[order.x]
    ltype <- ifelse(slope_analysis_fish$summary[slope_parameters[i],'97.5%']>0,2,1)
    lines(x[order.x], y,type='l',col=col_vector[i],lty=ltype)

  }
legend(x.limit*.5,1,legend=slope_parameters,cex=0.8,fill=col_vector[1:length(slope_parameters)],bty='n')
  }


