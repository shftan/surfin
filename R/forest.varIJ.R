#' The infinitesimal jackknife for random forests
#'
#' Calculate the infinitesimal jackknife variance
#' @param predictedAll a matrix with ntree rows where each element is the individual tree's prediction for that prediction
#' @param object A random forest trained with replace = TRUE
#' @return predictions for each observation and corresponding variance
#' @author Sarah Tan <\email{ht395@cornell.edu}>, Stefan Wager
#' @references Stefan Wager, Trevor Hastie, and Bradley Efron. (2014). Confidence Intervals for Random Forests: The Jackknife and the Infinitesimal Jackknife. Journal of Machine Learning Research, 15(May), 1625-1651. http://jmlr.org/papers/v15/wager14a.html
#' @seealso \code{\link{forest.varU}} 
#' @keywords random forest, variance, infinitesimal jackknife
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' object = forest(x=features,y=response,individualTrees=TRUE)
#' varIJ = forest.varIJ(object$predictedAll,object)

forest.varIJ <- function (predictedAll,object) {
  # Ensure correct sampling scheme
  if (!object$replace) stop('infinitesimal jackknife variance estimate requires sampling with replacement')  

  # Ensure predictions have same number of trees as forest
  if (is.null(dim(predictedAll))) stop('predictedAll must be a matrix of individual tree predictions')
  if (ncol(predictedAll)!=object$ntree) stop('predictedAll do not have the same number of columns as the number of trees in the forest object')

  # Extract parameters from forest
  B = object$B
  L = object$ntree / B
  n = dim(predictedAll)[1]  
  
  # Extract parameters from forest
  B = object$ntree
  n = dim(predictedAll)[1]       
  s = sum(object$inbag.times) / object$ntree
  
  if (class(predictedAll[1,1])%in%c("factor","character")) {
    y.hat = collapseClassPred(predictedAll) 
    pred = factorToNumber(predictedAll,object$key)
  } else 
  {
  	y.hat = collapseRegPred(predictedAll)
  	pred = predictedAll
  }
  pred.centered = pred - rowMeans(pred)    # centering does not change variance
  
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