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

  calculate_roc1 <- function(df, cost_of_fp, cost_of_fn, n=100) {
    tpr <- function(df, threshold) {
      sum(df$pred >= threshold & df$survived == 1) / sum(df$survived == 1)
    }

    fpr <- function(df, threshold) {
      sum(df$pred >= threshold & df$survived == 0) / sum(df$survived == 0)
    }

    cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
      sum(df$pred >= threshold & df$survived == 0) * cost_of_fp +
        sum(df$pred < threshold & df$survived == 1) * cost_of_fn
    }

    roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
    roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
    roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
    roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))

    return(roc)
  }

  roc <- calculate_roc1(predictions_results, fp, fn, n = 100)
  threshold_new <- roc[roc$cost==min(roc$cost),"threshold"]
  plot_roc(roc, threshold_new, fp, fn)

  AUC <- auc(predictions_results$survived,predictions_results$pred)
  short_auc <- as.numeric(gsub("Area under the curve: ","",AUC))
  a <- list(predictions_results = predictions_results,AUC = short_auc,threshold=threshold_new)
  return(a)
}
