#' Gives predictive accuracy of a model.
#'
#' @title Check predictive accuracy
#' @param p_table Table with two columns, one showing likelihood of colonisation and one showing whether or not there was in fact colonisation
#' @param threshold Percentage threshold above which species will colonise a lake and below which they will not
#'
#' @return Table with true presences, false presences, true absences, false absences.
#' @export
#'
#'


prediction_check <- function(p_table, threshold) {

  check <- as.data.frame(matrix(data=NA,nrow=2,ncol=2))
  rownames(check) <- c("predicted_absent","predicted_present")
  colnames(check) <- c("absent","present")
  check[1,1] <- sum(ifelse(p_table$survived == 0 & p_table$pred < threshold,1,0))
  check[2,2] <- sum(ifelse(p_table$survived == 1 & p_table$pred > threshold,1,0))
  check[1,2] <- sum(ifelse(p_table$survived == 1 & p_table$pred < threshold,1,0))
  check[2,1] <- sum(ifelse(p_table$survived == 0 & p_table$pred > threshold,1,0))
  s_presence <- round(check[2,2]/(check[2,2]+check[1,2]),digits=3)*100
  a_presence <- round(check[1,1]/(check[1,1]+check[2,1]),digits=3)*100
  print(paste0("Model predicted ",s_presence,"% of presences correctly and ",a_presence,"% of absences correctly."))
  return(check)

}
