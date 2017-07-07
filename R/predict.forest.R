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

predict.forest <- function (object, newdata=NULL, individualTrees=FALSE,...) {  
  if (!inherits(object, "forest"))
    stop("object not of class forest")
  if (is.null(object$forest)) 
    stop("object does not have forest")
  ## make sure that variables are correctly ordered
  newdata <- data.matrix(newdata[, object$varNames, drop = FALSE])
  
  # Determine predictions of individual trees
  predictedAll = cppPredict(data.matrix(newdata), 
             object$forest$splitVar, 
             object$forest$split,
             object$forest$leftDaughter, 
             object$forest$rightDaughter,
             object$forest$nodePred) 
             
  # Convert numbers to characters if binary classification
  if (object$type == "binary classification")
  {
  	 predictedAll = numberToFactor(predictedAll,object$key)
  	 if (!individualTrees)
  	 {
  	 	predicted = unlist(apply(predictedAll,1,function(x) names(sort(table(x),decreasing=T)[1])))
  	 	return(predicted)
  	 }  
  	 return(predictedAll)
  }
  else    # regression
  {
  	  if (!individualTrees)
  	  {
  	  	 predicted = rowMeans(predictedAll)
  	  	 return(predicted)
  	  }
  	  return(predictedAll)
  }
}