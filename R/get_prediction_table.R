#' Gives predictions of colonisation of lakes. Similar to dispersal_likelihood function, but instead of slope parameters, returns actual colonisation alongside predictions.
#'
#' @title Predict lake colonisation
#' @param upstream_slopes Table containing lake parameters, ids and peresence/absence of species.
#' @param gradient_analysis Gradient analysis tables derived from slope_analysis function.
#' @param parameter Parameter for which you want likelihoods.
#' @param distance States whether or not you want distance included in the model.
#' @param scaled.model Indicates whether or not gradient_analysis was scaled when you used it.
#'
#' @return Table with true presences, false presences, true absences, false absences.
#' @export
#'
#'
#'
get_prediction_table <- function(upstream_slopes,gradient_analysis,parameter,distance=TRUE,scaled.model=TRUE) {

  species <- gradient_analysis$species

new_slopes <- upstream_slopes[,parameter]
new_distances <- upstream_slopes$total_stream_length
lakeid <- upstream_slopes$locationID
slopes <- as.data.frame(cbind(lakeid,new_slopes,new_distances))

predictions_results <- cbind(upstream_slopes[,species],dispersal_likelihood(slopes, gradient_analysis, parameter, scaled.model=scaled.model,distance=distance)[,"likeli"])
predictions_results <- as.data.frame(predictions_results[!is.na(predictions_results[,1]),])
names(predictions_results) <- c("survived","pred")
return(predictions_results)

}
