#' Prediction wrapper function
#'
#' This function is a wrapper for the C++ implementation of random forest predictions
#' @param object random forest output
#' @param x matrix
#' @keywords random forest, causal inference
#' @export
#' @examples
#' rfPredict()

rfPredict <- function (object, x) {
  
  ## make sure that variables are correctly ordered
  x <- data.matrix(x[, object$varNames, drop = FALSE])
  
  #get predictions using C++ function 
  cppPredict(data.matrix(x), 
             object$forest$splitVar, 
             object$forest$split,
             object$forest$leftDaughter, 
             object$forest$rightDaughter,
             object$forest$nodePred)  
}