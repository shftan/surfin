#' Infinitesimal jacknife variance of random forest
#'
#' This function calculates the infinitesimal jacknife variance of the random forest
#' @param object random forest object
#' @keywords random forest, infitesimal jacknife variance
#' @export
#' @examples
#' rf.varIJ()

rf.varIJ <- function (object) {
  # Get number of observations
  n = dim(object$inbag.times)
  
  # Get prediction accuracy for each tree
  predAccuracy = 1 - object$err.rate 
  
  # Convert counts to indicator
  inbag = object$inbag.times
  inbag[inbag>0] = 1
  
  # Calculate infinitesimal jacknife estimate of variance
  varInfJacknife = sum(apply(inbag,1,function(x)cov(x,predAccuracy))^2)
  
  return(varInfJacknife)
}