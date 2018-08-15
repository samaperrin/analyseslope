#' Function gives best threshold above and below of which species will or will not colonise
#'
#' @title Species model validation
#' @param predictions_results Data frame which lists the chance that species will colonise a lake, and whether or not they in fact did.
#' @param fp The cost of a false presence.
#' @param fn Cost of a false negative.
#'
#' @return Your pseudo R^2 value.
#'
#' @export


species_validate <- function(predictions_results,fp=2,fn=1) {
  names(predictions_results) <- c("survived","pred")

  roc <- calculate_roc(predictions_results, fp, fn, n = 100)
  threshold_new <- roc[roc$cost==min(roc$cost),"threshold"]
  plot_roc(roc, threshold_new, fp, fn)

  AUC <- auc(predictions_results$survived,predictions_results$pred)
  short_auc <- as.numeric(gsub("Area under the curve: ","",AUC))
  a <- list(predictions_results = predictions_results,AUC = short_auc,threshold=threshold_new)
  return(a)
}
