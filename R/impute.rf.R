#' This function performs imputation
#' @param x matrix
#' @param y vector
#' @param nTree number of trees desired, default is 500
#' @param replace bootstrap samples or subsamples, default is bootstrap samples
#' @param keepForest keep forest or not, default is TRUE
#' @param mtry tuning
#' @param nodeSize node size
#' @param nSamp number of samples
#' @param maxNodes maximum number of nodes
#' @keywords random forest, causal inference
#' @export
#' @examples
#' impute.rf()

impute.rf <- function(data, mtry=(length(data)-2)/2, ...) {
#z <- data$z;  data$z <- NULL;
#y <- data$y;  data$y <- NULL;
#  <   cntrl <- fastRF( x=data[z==0,], y=y[z==0], mtry=mtry)
#  <   treat <- fastRF( x=data[z==1,], y=y[z==1], mtry=mtry)
#  <   return(mean(rfPredict(treat, data) - rfPredict(cntrl, data)))
}