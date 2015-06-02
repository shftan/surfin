#' Prediction wrapper function
#'
#' Wrapper for the C++ implementation of random forest predictions
#' @param object random forest output
#' @param newdata matrix
#' @param ... further arguments passed to or from other methods
#' @keywords random forest, prediction
#' @export
#' @examples
#' features = matrix(rnorm(100),nrow=10)
#' response = runif(10) 
#' forestobject = forest(x=features,y=response)
#' testfeatures = matrix(rnorm(100),nrow=10)
#' predict(object=forestobject,newdata=testfeatures)

predict.forest <- function (object, newdata=NULL, ...) {  
  if (!inherits(object, "forest"))
    stop("object not of class forest")
  if (is.null(object$forest)) 
    stop("object does not have forest")
  if (is.null(newdata))
  {
    # oob prediction on forest object  
  }
  else
  {
    ## make sure that variables are correctly ordered
    newdata <- data.matrix(newdata[, object$varNames, drop = FALSE])
  
    #get predictions using C++ function 
    cppPredict(data.matrix(newdata), 
             object$forest$splitVar, 
             object$forest$split,
             object$forest$leftDaughter, 
             object$forest$rightDaughter,
             object$forest$nodePred)  
  }
}