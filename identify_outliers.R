##### ---- Function 3: Identify outliers ##################

# parameter_of_interest <- "slope_max"

identify_outliers <- function(upstream_slopes_test,parameter_of_interest) {
  par(mfrow = c(1,1))
  plot(data=upstream_slopes_test,Present ~ upstream_slopes_test[,parameter_of_interest], xlab = parameter_of_interest, ylab = "Presence/absence upstream", main = paste("Perch presence/absence by",parameter_of_interest))
  
  outliers <- identify(x = upstream_slopes_test[,parameter_of_interest],
                       y = upstream_slopes_test$Present)
  outliers <- connectivity[outliers,]
  print(paste(nrow(outliers)," outliers identified", sep=""))
  return(outliers)
}

# identify_outliers_test <- identify_outliers(upstream_slopes_test,"slope_max")

