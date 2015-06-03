#' U-statistic based estimate for random forests
#'
#' Calculate the u-statistic based variance
#' @param object A random forest trained with replace=FALSE but with common observations
#' @keywords random forest, variance, u-statistic based
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"y")]
#' response = birds[,"y"]
#' forestobject = forest(x=features,y=response,replace=FALSE,var.type="ustat", B=5)
#' varU = forest.varU(forestobject)

forest.varU <- function (object) {
  # Ensure correct sampling scheme
  if (object$replace | is.null(object$var.type)) {
    stop('u-statistic based variance estimate requires sampling without replacement')  
  }
  if (object$var.type!="ustat") {
    stop('random forest trained for other variance estimates')
  }

  # Extract tree-wise predictions and variable counts from random forest
  B = object$B
  L = object$ntree / B
  n = dim(object$inbag.times)[1]
  
  pred = object$predictedAll
  if (object$type == "binary classification") pred = factorToNumber(pred)
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
  varU = object$nSamp^2 / n * var_predB + var_pred/object$ntree
  
  return(data.frame(y.hat=y.hat, var.hat=varU))
}