#' U-statistic based estimate for random objects
#'
#' Calculate the u-statistic based variance
#' @param predictedAll a matrix with ntree rows where each element is the individual tree's prediction for that prediction
#' @param object A random object trained with replace=FALSE but with common observations
#' @return predictions for each observation and corresponding variance
#' @author Sarah Tan <\email{ht395@cornell.edu}>, Lucas K. Mentch, Giles J. Hooker
#' @references Lucas K. Mentch and Giles J. Hooker. (2016). Quantifying Uncertainty in Random objects via Confidence Intervals and Hypothesis Tests. Journal of Machine Learning Research, 17(26), 1-41. http://www.jmlr.org/papers/volume17/14-168/14-168.pdf  
#' @seealso \code{\link{object.varIJ}} 
#' @keywords random object, variance, u-statistic based
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' object = forest(x=features,y=response,var.type="ustat",B=5)
#' varU = forest.varU(object$predictedAll,object)

forest.varU <- function (predictedAll,object) {
  # Ensure correct sampling scheme
  if (object$replace | is.null(object$var.type)) stop('u-statistic based variance estimate requires sampling without replacement')  
  if (object$var.type!="ustat") stop('random object trained for other variance estimates')
  
  # Ensure predictions have same number of trees as forest
  if (is.null(dim(predictedAll))) stop('predictedAll must be a matrix of individual tree predictions')
  if (ncol(predictedAll)!=object$ntree) stop('predictedAll do not have the same number of columns as the number of trees in the forest object')

  # Extract parameters from forest
  B = object$B
  L = object$ntree / B
  n = dim(predictedAll)[1]       
  
  if (class(predictedAll[1,1])%in%c("factor","character")) {
    y.hat = collapseClassPred(predictedAll) 
    pred = factorToNumber(predictedAll,object$key)
  } else 
  {
  	y.hat = collapseRegPred(predictedAll)
  	pred = predictedAll
  }
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