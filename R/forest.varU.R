#' U-statistic based estimate for random objects
#'
#' Calculate the u-statistic based variance
#' @param predictedAll a matrix with ntree rows where each element is the individual tree's prediction for that prediction
#' @param object A random object trained with replace=FALSE but with common observations
#' @param covariance whether covariance should be returned instead of variance, default is FALSE
#' @return if covariance=TRUE, a list with predictions for each observation and covariances between predictions; otherwise, predictions for each observation and corresponding variance
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

forest.varU <- function (predictedAll,object,covariance=FALSE) {
  # Ensure correct sampling scheme
  if (object$replace | is.null(object$var.type)) stop('u-statistic based variance estimate requires sampling without replacement')  
  if (object$var.type!="ustat") stop('random object trained for other variance estimates')
  
  # Ensure predictions have same number of trees as forest
  if (is.null(dim(predictedAll))) stop('predictedAll must be a matrix of individual tree predictions')
  if (ncol(predictedAll)!=object$ntree) stop('predictedAll do not have the same number of columns as the number of trees in the forest object')

  # Extract parameters from forest
  M = object$ntree
  B = object$B
  L = M / B
  n = dim(predictedAll)[1]    
  k = object$sampSize  
  
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
  varU = k^2 / n * var_predB + var_pred/M
  	
  if (covariance) {
    cov_predB = cov(t(predB))
  	cov_pred = cov(t(pred))
  	covU = k^2 / n * cov_predB + cov_pred/M
  	return(list(y.hat,covU))
  }
  else return(data.frame(y.hat=y.hat, var.hat=varU))
}