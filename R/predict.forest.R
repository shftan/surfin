#' Prediction wrapper function
#'
#' Wrapper for the C++ implementation of random forest predictions
#' @param object random forest objectput
#' @param newdata matrix
#' @param ... further arguments passed to or from other methods
#' @keywords random forest, prediction
#' @export
#' @examples
#' n = dim(birds)[1]
#' n_train = round(n/2)
#' features = birds[1:n_train,setdiff(names(birds),"y")]
#' response = birds[,"y"]
#' forestobject = forest(x=features,y=response)
#' testfeatures = birds[n_train+1:n,setdiff(names(birds),"y")]
#' predict(object=forestobject,newdata=testfeatures)

predict.forest <- function (object, newdata=NULL, ...) {  
  if (!inherits(object, "forest"))
    stop("object not of class forest")
  if (is.null(object$forest)) 
    stop("object does not have forest")
  if (is.null(newdata))
    stop("OOB predictions already available in forest object")
  ## make sure that variables are correctly ordered
  newdata <- data.matrix(newdata[, object$varNames, drop = FALSE])
  
  #get predictions using C++ function 
  predicted = cppPredict(data.matrix(newdata), 
             object$forest$splitVar, 
             object$forest$split,
             object$forest$leftDaughter, 
             object$forest$rightDaughter,
             object$forest$nodePred)  
  
  # Convert numbers to factor levels if binary classification
  if (object$type == "binary classification")
  {
    predicted = numberToFactor(predicted,object$key)
    predicted = factor(predicted)
  }
  return(predicted)
}