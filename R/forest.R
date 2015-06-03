#' Random forest wrapper function
#'
#' Wrapper for the C++ implementation of random forest
#' @param x matrix
#' @param y vector
#' @param nTree number of trees desired, default is 500
#' @param replace bootstrap samples or subsamples, default is bootstrap samples
#' @param keepForest keep forest or not, default is TRUE
#' @param mtry tuning
#' @param nodeSize node size
#' @param nSamp number of samples
#' @param maxNodes maximum number of nodes
#' @keywords random forest
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"y")]
#' response = birds[,"y"]
#' forest(x=features,y=response)

forest <- function (x, y, nTree = 500, replace = TRUE, keepForest = TRUE,
          mtry     = if (is.factor(y)) floor(sqrt(ncol(x))) else max(floor(ncol(x)/3), 1), 
          nodeSize = if (is.factor(y)) 1                    else 5, 
          nSamp    = if (replace) nrow(x) else ceiling(0.632 * nrow(x)),
          maxNodes = 2 * trunc(nSamp/max(1, nodeSize - 4)) + 1) {
  
  # Only run for regression or binary classification
  # multiclass classification requires different splitting, unsupervised learning requires additional code
  if (is.factor(y)) {
      if (length(levels(y))>2) {
   			stop('can only handle regression or binary classification')  
       }
  }
  
  # Only run if no NA's in either response or predictor(s)
  if (sum(is.na(y)) != 0)
  {
    stop('NA not permitted in response') 
  }
  
  if (sum(is.na(x)) != 0)
  {
    stop('NA not permitted in predictors') 
  }
    
  varNames <- if (is.null(colnames(x))) 1:ncol(x) else colnames(x)
  #yOffset <- if (is.factor(y)) 0 else mean(y)     # consider moving this back in for speed improvements
  #if (!is.factor(y)) y <- y - yOffset
  out <- cppForest(data.matrix(x), y, nSamp, nodeSize, maxNodes, nTree, mtry, 
                      keepForest, replace, is.factor(y))
  #if (!is.factor(y)) y = y + yOffset
  #out$forest$nodePred <- out$forest$nodePred + yOffset
  #out$predictedByTree <- out$predictedByTree + yOffset
  out$predictedOOB = out$predictedAll
  out$predictedOOB[out$inbag.times!=0] = NA
  out$predicted = rowMeans(out$predictedOOB,na.rm=T)
  
  ## If classification, format predictions as factors and calculate oob err.rate
  # R orders levels of a factor alphabetically
  ## If regression, calculate oob mse
  if (is.factor(y)) {
    out$type = "binary classification"
    
    # Find mapping of factor level to number
    out$key = unique(data.frame(y,as.numeric(y)))
    out$key[,1] = as.character(out$key[,1])
    
    ## Convert numbers to factor levels
    # All predictions by tree
    index = out$predictedAll<mean(out$key[,2])   # specific to binary classification
    out$predictedAll[index] = out$key[1,1]
    out$predictedAll[!index] = out$key[2,1]
    
    # OOB predictions by tree
    out$predictedOOB = out$predictedAll
    out$predictedOOB[out$inbag.times!=0] = NA
    
    # Prediction for each observation based on OOB predictions by tree
    index = out$predicted<mean(out$key[,2]) 
    out$predicted[index] = out$key[1,1]
    out$predicted[!index] = out$key[2,1]
    out$predicted = factor(out$predicted)
    
    out$err.rate = apply(out$predictedOOB,2,function(x) mean(x!=as.character(y),na.rm=TRUE))
  }
  else {
    out$type = "regression"
    out$mse = apply(out$predictedOOB,2,function(x) mean((x-y)^2,na.rm=TRUE))
  }
  
  out$varNames        <- varNames
  if (!keepForest) out$forest <- NULL
  
  class(out) = "forest"
  return(out)
}