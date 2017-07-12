#' Random forest wrapper function
#'
#' Wrapper for the C++ implementation of random forest
#' @param x matrix of predictors. Currently non-response is not supported, and categorical predictors are converted to their numeric equivalents, not made into indicator variables. The latter feature is pending. 
#' @param y vector of response. If it is a factor, classification is assumed. Otherwise, regression is assumed. Currently, only binary classification is supported, and non-response is not supported. 
#' @param individualTrees whether to return predictions of individual trees, default is FALSE. If var.type is not NULL, this is set to TRUE.
#' @param ntree number of trees desired, default is 1000
#' @param replace bootstrap samples or subsamples, default is bootstrap samples. If var.type = "ustat", this is set to FALSE; if var.type = "infjack", this is set to TRUE.
#' @param var.type default is NULL. Type of variance estimate to be computed. 2 options: "ustat" for u-statistic based which needs replace=FALSE or "infjack" for infinitesimal jackknife which needs replace=TRUE
#' @param B default is NULL. Number of unique common observations for u-statistic based variance estimate. Note that L, the number of trees sharing a common observation, typically >> B.
#' @param keepForest output forest in output object or not, default is TRUE
#' @param mtry number of variables randomly sampled at each split, default value is floor(sqrt(p)) for classification, max(floor(p/3),1) for regression, where p is the number of features
#' @param nodeSize minimum size of terminal nodes, default value is 1 for classification, 5 for regression
#' @param sampSize size of sample to draw, default value is n for bootstrap samples, 0.632*n for subsamples, where n is the number of observations
#' @param maxNodes maximum number of terminal nodes a tree can have
#' @return List with the following components
#' \item{forest}{forest object, NULL if keepForest=FALSE}
#' \item{inbag.times}{n by ntree matrix (i,j)th value being number of times observation i was used in tree j}
#' \item{oob.times}{number of times observation was out-of-bag}
#' \item{predictedAll}{only returned if individualTrees=TRUE. n by ntree matrix with (i,j)th value being tree j's prediction for observation i} 
#' \item{predictedOOB}{only returned if individualTrees=TRUE. predictedAll but with in-bag observations NA-ed}
#' \item{predictedProb}{only returned for classification and if individualTrees=TRUE. n by ntree matrix with (i,j)th value being tree j's predicted probability of observation i being of class with key value 1. See forestobject$key for mapping of key value to class} 
#' \item{predictedProbOOB}{only returned for classification and if individualTrees=TRUE. predictedProb but with in-bag observations NA-ed}
#' \item{predicted}{length n vector of predictions based on out-of-bag observations}
#' \item{err.rate}{only returned for classification. Forest error rate}
#' \item{mse}{only returned for regression. Forest mean square error}
#' \item{type}{classification or regression}
#' \item{key}{only returned for classification. Mapping of numbers to classes}
#' \item{varNames}{names of features}
#' \item{ntree}{number of trees in forest}
#' \item{replace}{replace value used}
#' \item{var.type}{var.type value used}
#' \item{B}{B value used}
#' \item{sampSize}{sampSize value used}
#' \item{individualTrees}{individualTrees value used}
#' @author Sarah Tan <\email{ht395@cornell.edu}>, David I. Miller
#' @references Leo Breiman. (2001). Random Forests. Machine Learning 45(1), 5-32. http://link.springer.com/article/10.1023/A:1010933404324
#' @seealso \code{\link{predict.forest}}, \code{\link{forest.varIJ}}, \code{\link{forest.varU}}
#' @keywords random forest
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' forest(x=features,y=response)

forest <- function (x, y, individualTrees = FALSE, ntree = 1000, replace=TRUE, var.type=NULL, B = NULL, keepForest = TRUE,
          mtry     = if (is.factor(y)) floor(sqrt(ncol(x))) else max(floor(ncol(x)/3), 1), 
          nodeSize = if (is.factor(y)) 1                    else 5, 
          sampSize    = if (replace) nrow(x) else ceiling(0.632 * nrow(x)),
          maxNodes = 2 * trunc(sampSize/max(1, nodeSize - 4)) + 1) {
  
  # Ensure correct sampling scheme
  ustat = FALSE
  if (!is.null(var.type)) {
    if (var.type != "ustat" & var.type != "infjack") {
      stop('wrong input method for type of variance estimate')  
    }
    if (var.type=="ustat") {
      if (replace) replace = FALSE
      if (!individualTrees) individualTrees = TRUE
      if (is.null(B)) stop("need to specify B")
      if (B < 0 | ntree %% B != 0) stop("ntree and B values needed such that ntree = B * positive integer")
      ustat=TRUE
    }
    if (var.type == "infjack") {
      if (!replace) replace = TRUE
      if (!individualTrees) individualTrees = TRUE
    }
  }  
  if (is.null(B)) B = 0
  
  # Only run for regression or binary classification
  # multiclass classification requires different splitting, unsupervised learning requires additional code
  if (is.factor(y) & length(levels(y))>2) stop('can only handle regression or binary classification')  
  
  # Only run if no NA's in either response or predictor(s)
  if (sum(is.na(y)) != 0) stop('NA not permitted in response') 
  
  if (sum(is.na(x)) != 0) stop('NA not permitted in predictors') 

  # Determine if classification or regression is to be performed
  if (is.factor(y)) {
    # Find mapping of factor level to number
    key = getKey(y)
    y_numeric = factorToNumber(y,key)
  } else 
  {
    y_numeric = y
  }
  
  # Quick and dirty fix for categorical predictors, needs to be changed
  x = data.matrix(x)

  # Call C++ function
  #yOffset <- if (is.factor(y)) 0 else mean(y)     # consider moving this back in for speed improvements
  #if (!is.factor(y)) y <- y - yOffset
  out <- cppForest(x, y_numeric, sampSize, nodeSize, maxNodes, ntree, mtry, keepForest, replace, is.factor(y), ustat, B)
  #if (!is.factor(y)) y = y + yOffset
  #out$forest$nodePred <- out$forest$nodePred + yOffset
  #out$predictedByTree <- out$predictedByTree + yOffset
  
  # Store meta-data in forest object
  if (is.factor(y)) {
    out$type = "binary classification"
    out$key = key
  } else 
  {
    out$type = "regression"
  }  
  out$varNames = if (is.null(colnames(x))) 1:ncol(x) else colnames(x)
  out$replace = replace
  out$var.type = var.type
  out$B = B
  out$sampSize = sampSize
  out$ntree = ntree
  out$individualTrees = individualTrees
               
  ### Prediction on x and y
  # Determine predictions of individual trees
  predictedRaw = cppPredict(x, 
             out$forest$splitVar, 
             out$forest$split,
             out$forest$leftDaughter, 
             out$forest$rightDaughter,
            out$forest$nodePred) 
    
  # Process and store predictions
  if (out$type == "binary classification") {
    predictedProb = predictedRaw - 1
  	predictedProbOOB = predictedProb
  	predictedProbOOB[out$inbag.times!=0] = NA
    out$predicted = collapseProbPred(predictedProbOOB,out$key)
    if (individualTrees) {
    	out$predictedAll = rawProbToFactor(predictedRaw,out$key)
    	out$predictedProb = predictedProb
    	out$predictedOOB = out$predictedAll
  		out$predictedOOB[out$inbag.times!=0] = NA
  		out$predictedProbOOB = predictedProbOOB
    }	
  } else 
  {
  	predictedOOB = predictedRaw
  	predictedOOB[out$inbag.times!=0] = NA
  	out$predicted = collapseRegPred(predictedOOB)
  	if (individualTrees) {
  		out$predictedAll = predictedRaw
  		out$predictedOOB = predictedOOB	
  	}
  }
  
  # If classification, calculate oob err.rate; if regression, oob mse
  #if (out$type == "binary classification") out$err.rate = mean(predicted!=y)
  #else out$mse = mean((predicted-y)^2)

  if (!keepForest) out$forest <- NULL
  class(out) = "forest"
  return(out)
}