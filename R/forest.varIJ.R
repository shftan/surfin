#' The infinitesimal jackknife for random forests
#'
#' Calculate the infinitesimal jackknife variance
#' @param object A random forest trained with replace = TRUE
#' @return predictions for each observation and corresponding variance
#' @author Hui Fen Tan <\email{ht395@cornell.edu}>, Stefan Wager
#' @references Stefan Wager, Trevor Hastie, and Bradley Efron. (2014). Confidence Intervals for Random Forests: The Jackknife and the Infinitesimal Jackknife. Journal of Machine Learning Research, 15(May), 1625-1651. http://jmlr.org/papers/v15/wager14a.html
#' @seealso \code{\link{forest.varU}} 
#' @keywords random forest, variance, infinitesimal jackknife
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' forestobject = forest(x=features,y=response,individualTrees=TRUE)
#' varIJ = forest.varIJ(forestobject)

forest.varIJ <- function (object) {
  # Ensure correct sampling scheme
  if (!object$replace) {
    stop('infinitesimal jackknife variance estimate requires sampling with replacement')  
  }

  # Ensure individual trees were stored
  if (!object$individualTrees) {
    stop('variance estimation requires individual tree predictions')  
  }
  
  # Extract tree-wise predictions and variable counts from random forest
  B = object$ntree
  n = dim(object$inbag.times)[1]
  s = sum(object$inbag.times) / object$ntree
  
  pred = factorToNumber(object$predictedAll)
  y.hat = rowMeans(pred)
  if (object$type == "binary classification") y.hat = numberToFactor(y.hat,object$key)
  pred.centered = pred - rowMeans(pred)   # centering does not change variance

  N = Matrix::Matrix(object$inbag.times, sparse = TRUE)
  N.avg = Matrix::rowMeans(N)
  
  #
  # Compute raw infinitesimal jackknife
  #
  
  C = N %*% t(pred.centered) - Matrix::Matrix(N.avg, nrow(N), 1) %*% Matrix::Matrix(rowSums(pred.centered), 1, nrow(pred.centered))
  raw.IJ = Matrix::colSums(C^2) / B^2
  
  #
  # Apply Monte Carlo bias correction
  #
  
  N.var = mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
  boot.var = rowSums(pred.centered^2) / B
  bias.correction = n * N.var * boot.var / B
  varIJ = raw.IJ - bias.correction
  
  return(data.frame(y.hat=y.hat, var.hat=varIJ))
}