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
##your_upstream_slopes <- upstream_slopes
#species <- "Perch"
#slope_parameters <- c("slope_mean","slope_third_quart","slope_max","slope_max_max")
#DoS <- "distance"
#plot_dispersal_curves(your_upstream_slopes, slope_analysis_fish, species, slope_parameters,DoS = "slope")

plot_dispersal_curves <- function(your_upstream_slopes, slope_analysis_fish, species, slope_parameters,DoS,slope.scaled=FALSE){

  # CASE 1: We want slope and we have both slope and distance

  condition2 <- length(slope_analysis_fish$summaries)
  if (DoS=="slope" & condition2 == 2) {
    n <- length(slope_parameters)
    if (slope.scaled==TRUE) {
    range.X <- range(scale(your_upstream_slopes[,slope_parameters]/100))
    } else {
      range.X <- range(your_upstream_slopes[,slope_parameters])/100
    }
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


    plot(your_upstream_slopes[,species] ~ your_upstream_slopes[,slope_parameters[1]],type='n',ylab=paste0(species," Presence/Absence"),xlab="Slope in degrees x 100",xlim=range.X)
    for(i in 1:length(slope_parameters)){
      full_analysis <- slope_analysis_fish$all_data[[slope_parameters[i]]]
      betas <- full_analysis$BUGSoutput$mean$beta
      if (slope.scaled ==TRUE) {
      range.x <- range(scale(your_upstream_slopes[,slope_parameters[i]]/100))
      } else {range.x <- range(your_upstream_slopes[,slope_parameters[i]]/100)}
      x <- seq(range.x[1],range.x[2],(range.x[2]-range.x[1])/100)
      avg.dist <- log(median(upstream_slopes$total_stream_length))
      ltype <- ifelse(slope_analysis_fish$summaries$slope[slope_parameters[i],'97.5%']>0,2,1)
      expres <- exp(betas[1]+betas[2]*x+betas[3]*avg.dist)
      lines(x,expres/(1+expres),type='l',col=col_vector[i],lty=ltype)
    }
    legend(range.X[1]*.5,1,legend=slope_parameters,cex=0.8,fill=col_vector[1:length(slope_parameters)],bty='n')

    # Case 2: We want slope and only have slope

      } else {
    if (DoS=="slope" & condition2 == 1){

      # Set range and colours
      n <- length(slope_parameters)
      if (slope.scaled==TRUE) {
        range.X <- range(scale(your_upstream_slopes[,slope_parameters]/100))
      } else {
        range.X <- range(your_upstream_slopes[,slope_parameters])/100
      }
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
      # Make plot
      plot(your_upstream_slopes[,species] ~ your_upstream_slopes[,slope_parameters[1]],type='n',ylab=paste0(species," Presence/Absence"),xlab="Slope in degrees x 100",xlim=range.X)
      for(i in 1:length(slope_parameters)){
        full_analysis <- slope_analysis_fish$all_data[[slope_parameters[i]]]
        betas <- full_analysis$BUGSoutput$mean$beta
        if (slope.scaled ==TRUE) {
          range.x <- range(scale(your_upstream_slopes[,slope_parameters[i]]/100))
        } else {
          range.x <- range(your_upstream_slopes[,slope_parameters[i]]/100)
          }
        x <- seq(range.x[1],range.x[2],(range.x[2]-range.x[1])/100)
        ltype <- ifelse(slope_analysis_fish$summaries$slope[slope_parameters[i],'97.5%']>0,2,1)
        expres <- exp(betas[1]+betas[2]*x)
        lines(x,expres/(1+expres),type='l',col=col_vector[i],lty=ltype)
      }
      legend(range.X[1]*.5,1,legend=slope_parameters,cex=0.8,fill=col_vector[1:length(slope_parameters)],bty='n')

    } else {

      # Case 3: We want distance and only have distance

      if (DoS=="distance" & condition2 == 1) {

        # Set range, only 1 line, no need for colour
        n <- 1
        x.limit <- range(log(your_upstream_slopes[,"total_stream_length"]))

        # Make plot
        plot(your_upstream_slopes[,species] ~ your_upstream_slopes[,"total_stream_length"],type='n',ylab=paste0(species," Presence/Absence"),xlab="Distance",xlim=x.limit)
          full_analysis <- slope_analysis_fish$all_data$distance
          betas <- full_analysis$BUGSoutput$mean$beta
          x <- seq(x.limit[1],x.limit[2],(x.limit[2]-x.limit[1])/100)
          ltype <- ifelse(slope_analysis_fish$summaries$distance[,'97.5%']>0,2,1)
          expres <- exp(betas[1]+betas[2]*x)
          lines(x,expres/(1+expres),type='l',lty=ltype)
          # Case 4: We want distance and have distance and slope

          } else {

        # Set range and colours
        n <- length(slope_parameters)
        x.limit <- range(log(your_upstream_slopes[,"total_stream_length"]))
        qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
        col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
        # Make plot
        plot(your_upstream_slopes[,species] ~ your_upstream_slopes[,"total_stream_length"],type='n',ylab=paste0(species," Presence/Absence"),xlab="Distance",xlim=x.limit)
        for(i in 1:length(slope_parameters)){
          full_analysis <- slope_analysis_fish$all_data[[slope_parameters[i]]]
          betas <- full_analysis$BUGSoutput$mean$beta
          x <- seq(x.limit[1],x.limit[2],(x.limit[2]-x.limit[1])/100)
          ltype <- ifelse(slope_analysis_fish$summaries$distance[slope_parameters[i],'97.5%']>0,2,1)
          if (slope.scaled == TRUE) {avg.slope <- median(scale(log(upstream_slopes[,slope_parameters[i]])))
          } else {avg.slope <- median(log(upstream_slopes[,slope_parameters[i]]))}
          expres <- exp(betas[1]+betas[3]*x+betas[2]*avg.slope)
          lines(x,expres/(1+expres),type='l',col=col_vector[i],lty=ltype)
        }
        legend(x.limit[1],0.5,legend=slope_parameters,cex=0.8,fill=col_vector[1:length(slope_parameters)],bty='n')
              }
      # Need to insert code here for what happens if we have only distance and just want to plot distance, and for if we have slope and distance and want to plot distance
    }
  }

}

