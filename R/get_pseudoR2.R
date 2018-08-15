#' Function returns pseduo R^2 value for your parameter based on your model and null deviance.
#'
#' @title Get pseudo R^2
#' @param gradient_analysis Gradient analysis table derived from slope_analysis function.
#' @param null_deviance Null deviance value derived from calc_null_deviance function.
#' @param parameter Parameter to analyse.
#'
#' @return Your pseudo R^2 value.
#'
#' @export




get_pseudoR2 <- function(gradient_analysis,null_deviance,parameter){
  # Extract residual deviance
  parameter_analysis <- gradient_analysis$all_data[parameter]
  names(parameter_analysis) <- "parameter"
  residual_deviance <- parameter_analysis$parameter$BUGSoutput$mean$deviance

  #Calculate psuedo R2
  pseudo_R2 <- 1-residual_deviance/null_deviance

  return(pseudo_R2)
}
