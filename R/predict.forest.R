#' Prediction wrapper function
#'
#' Wrapper for the C++ implementation of random forest predictions
#' @param object random forest objectput
#' @param newdata matrix
#' @param individualTrees whether to return predictions of individual trees, default is FALSE
#' @param ... further arguments passed to or from other methods
#' @return vector of predictions for each observation if individualTrees=FALSE (default), otherwise matrix of predictions by each tree for each observation
#' @author Sarah Tan <\email{ht395@cornell.edu}>, David I. Miller
#' @references Leo Breiman. (2001). Random Forests. Machine Learning 45(1), 5-32. http://link.springer.com/article/10.1023/A:1010933404324
#' @seealso \code{\link{forest.varIJ}}, \code{\link{forest.varU}}
#' @keywords random forest, prediction
#' @export
#' @examples
#' n = dim(birds)[1]
#' n_train = round(n/2)
#' features = birds[1:n_train,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' forestobject = forest(x=features,y=response)
#' testfeatures = birds[n_train+1:n,setdiff(names(birds),"detected")]
#' predict(object=forestobject,newdata=testfeatures)
#' predict(object=forestobject,newdata=testfeatures, individualTrees=TRUE)

predict.forest <- function (object, newdata=NULL, individualTrees=FALSE, ...) {  
  if (!inherits(object, "forest")) stop("object not of class forest")
  if (is.null(object$forest)) stop("object does not have forest")

  ## Make sure that variables are correctly ordered
  newdata <- data.matrix(newdata[, object$varNames, drop = FALSE])
  
  # Quick and dirty fix for categorical predictors, needs to be changed
  newdata = data.matrix(newdata)
  
  # Determine predictions of individual trees
  predictedRaw = cppPredict(newdata, 
             object$forest$splitVar, 
             object$forest$split,
             object$forest$leftDaughter, 
             object$forest$rightDaughter,
             object$forest$nodePred) 
  
  # Initialize output
  out = list()
             
  # Process and store predictions
  if (object$type == "binary classification") {
    predictedProb = predictedRaw - 1
    predicted = collapseProbPred(predictedProb,object$key)
    if (individualTrees) {
        out$predictedAll = rawProbToFactor(predictedRaw,object$key)
    	out$predictedProb = predictedProb
    	out$predicted = predicted
    }
  } else 
  {
  	predicted = collapseRegPred(predictedRaw)
  	if (individualTrees) {
  		out$predictedAll = predictedRaw
  		out$predicted = predicted
  	}
  }
  
  if (individualTrees) return(out)
  else return(predicted)
}