###### ---- Function 2: Plot parameters vs presence/absence ##############


####### ---- Plot/model data #######################

plot_slope_params <- function(upstream_slopes_test) {
  par(mfrow = c(2,2))
  short_poi <- gsub("upstream_lakes_","",parameters_of_interest)
  
  for (i in 1:(length(parameters_of_interest)-1)){
    plot(data=upstream_slopes_test,Present ~ upstream_slopes_test[,5+i], xlab = short_poi[i+1], ylab = "Presence/absence upstream", main = paste("Perch presence/absence by",short_poi[i+1]))
  }
  
}

# plot_slope_params(upstream_slopes_test)
