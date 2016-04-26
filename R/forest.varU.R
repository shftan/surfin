#' U-statistic based estimate for random forests
#'
#' Calculate the u-statistic based variance
#' @param object A random forest trained with replace=FALSE but with common observations
#' @return predictions for each observation and corresponding variance
#' @author Hui Fen Tan <\email{ht395@cornell.edu}>, Lucas K. Mentch, Giles J. Hooker
#' @references Lucas K. Mentch and Giles J. Hooker. (2016). Quantifying Uncertainty in Random Forests via Confidence Intervals and Hypothesis Tests. Journal of Machine Learning Research, 17(26), 1-41. http://www.jmlr.org/papers/volume17/14-168/14-168.pdf  
#' @seealso \code{\link{forest.varIJ}} 
#' @keywords random forest, variance, u-statistic based
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' forestobject = forest(x=features,y=response,var.type="ustat",B=5)
#' varU = forest.varU(forestobject)

forest.varU <- function (object) {
  # Ensure correct sampling scheme
  if (object$replace | is.null(object$var.type)) {
    stop('u-statistic based variance estimate requires sampling without replacement')  
  }
  if (object$var.type!="ustat") {
    stop('random forest trained for other variance estimates')
  }
  
  # Ensure individual trees were stored
  if (!object$individualTrees) {
    stop('variance estimation requires individual tree predictions')  
  }
  
  # Extract tree-wise predictions and variable counts from random forest
  B = object$B
  L = object$ntree / B
  n = dim(object$inbag.times)[1]
  
  pred = factorToNumber(object$predictedAll)
  y.hat = rowMeans(pred)
  if (object$type == "binary classification") y.hat = numberToFactor(y.hat,object$key)
  pred.centered = pred - rowMeans(pred)    # centering does not change variance
  
  Bs = rep(1:B,each=L)
  predB = matrix(nrow=n,ncol=B)
  for (i in 1:B)
  {
    predB[,i] = rowMeans(pred.centered[,Bs==i])
  }
  var_predB = apply(predB,1,var)
  var_pred = apply(pred.centered,1,var)
  varU = object$sampSize^2 / n * var_predB + var_pred/object$ntree
  
  return(data.frame(y.hat=y.hat, var.hat=varU))
}