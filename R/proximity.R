#' Proximity matrix function
#'
#' Wrapper for the C++ implementation of random forest proximity matrix. Not ready, do not use.
#' @param terminalNodes n by nTree matrix of terminal nodes
#' @param ... further arguments passed to or from other methods
#' @keywords random forest, proximity matrix
#' @export
#' @examples
#' n = dim(birds)[1]
#' n_train = round(n/2)
#' features = birds[1:n_train,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' forestobject = forest(x=features,y=response)

proximity = function (terminalNodes,...) {  
  if (!inherits(object, "forest"))
    stop("object not of class forest")
  if (is.null(object$forest)) 
    stop("object does not have forest")
  if (is.null(newdata))
    stop("OOB predictions already available in forest object")
  ## make sure that variables are correctly ordered
  newdata <- data.matrix(newdata[, object$varNames, drop = FALSE])
  
  #get predictions using C++ function 
  predictedAll = cppPredict(data.matrix(newdata), 
                            object$forest$splitVar, 
                            object$forest$split,
                            object$forest$leftDaughter, 
                            object$forest$rightDaughter,
                            object$forest$nodePred)  
  if (individualTrees)
  {
    return(predictedAll)
  } else
  {
    predicted = rowSums(predictedAll)/object$ntree
    # Convert numbers to factor levels if binary classification
    if (object$type == "binary classification")
    {
      predicted = numberToFactor(predicted,object$key)
      predicted = factor(predicted)
    }
    return(predicted)
  } 
}