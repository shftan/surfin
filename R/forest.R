#' Random forest wrapper function
#'
#' Wrapper for the C++ implementation of random forest
#' @param x matrix
#' @param y vector
#' @param ntree number of trees desired, default is 500
#' @param replace bootstrap samples or subsamples, default is bootstrap samples
#' @param var.type default is NULL. Type of variance estimate to be computed. 2 options: "ustat" for u-statistic based which needs replace=FALSE or "infjack" for infinitesimal jackknife which needs replace=TRUE
#' @param B default is NULL. Number of unique common observations for u-statistic based variance estimate. Note that L, the number of trees sharing a common observation, typically >> B.
#' @param keepForest keep forest or not, default is TRUE
#' @param mtry tuning
#' @param nodeSize node size
#' @param nSamp number of samples
#' @param maxNodes maximum number of nodes
#' @keywords random forest
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' forest(x=features,y=response)

forest <- function (x, y, ntree = 500, replace=TRUE, var.type=NULL, B = NULL, keepForest = TRUE,
          mtry     = if (is.factor(y)) floor(sqrt(ncol(x))) else max(floor(ncol(x)/3), 1), 
          nodeSize = if (is.factor(y)) 1                    else 5, 
          nSamp    = if (replace) nrow(x) else ceiling(0.632 * nrow(x)),
          maxNodes = 2 * trunc(nSamp/max(1, nodeSize - 4)) + 1) {
  
  # Ensure correct sampling scheme
  ustat = FALSE
  if (!is.null(var.type)) {
    if (var.type != "ustat" & var.type != "infjack") {
      stop('wrong input method for type of variance estimate')  
    }
    if (var.type=="ustat") {
      if (replace) stop("U-statistic based variance estimate requires sampling without replacement")
      if (is.null(B)) stop("need to specify B")
      if (B < 0 | ntree %% B != 0) stop("ntree and B values needed such that ntree = B * positive integer")
      ustat=TRUE
    }
    if (var.type == "infjack" & !replace) stop("infinitesimal jackknife variance estimate requires sampling with replacement")
  }  
  if (is.null(B))
  {
    B = 0
  }
  
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
  out <- cppForest(data.matrix(x), y, nSamp, nodeSize, maxNodes, ntree, mtry, 
                      keepForest, replace, is.factor(y), ustat, B)
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
    # first (alphabetically-speaking) level becomes 1, second level becomes 2
    out$key = unique(data.frame(y,as.numeric(y)))
    out$key[,1] = as.character(out$key[,1])
    
    ## Convert numbers to factor levels
    # All predictions by tree
    out$predictedAll = numberToFactor(out$predictedAll,out$key)
    
    # OOB predictions by tree
    out$predictedOOB = numberToFactor(out$predictedOOB,out$key)
    
    # Prediction for each observation based on OOB predictions by tree
    out$predicted = numberToFactor(out$predicted,out$key)
    out$predicted = factor(out$predicted)
    
    out$err.rate = apply(out$predictedOOB,2,function(x) mean(x!=as.character(y),na.rm=TRUE))
  }
  else {
    out$type = "regression"
    out$mse = apply(out$predictedOOB,2,function(x) mean((x-y)^2,na.rm=TRUE))
  }
  
  out$varNames        <- varNames
  
  if (!keepForest) out$forest <- NULL
  
  out$replace = replace
  
  out$var.type = var.type
  out$B = B
  out$nSamp = nSamp
  out$ntree = ntree
  
  class(out) = "forest"
  return(out)
}