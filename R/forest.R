#' Random forest wrapper function
#'
#' Wrapper for the C++ implementation of random forest
#' @param x matrix
#' @param y vector
#' @param individualTrees whether to return predictions of individual trees, default is FALSE. If var.type is not NULL, this is set to TRUE.
#' @param ntree number of trees desired, default is 1000
#' @param replace bootstrap samples or subsamples, default is bootstrap samples. If var.type = "ustat", this is set to FALSE; if var.type = "infjack", this is set to TRUE.
#' @param var.type default is NULL. Type of variance estimate to be computed. 2 options: "ustat" for u-statistic based which needs replace=FALSE or "infjack" for infinitesimal jackknife which needs replace=TRUE
#' @param B default is NULL. Number of unique common observations for u-statistic based variance estimate. Note that L, the number of trees sharing a common observation, typically >> B.
#' @param keepForest output forest in output object or not, default is TRUE
#' @param mtry number of variables randomly sampled at each split, default value is floor(sqrt(p)) for classification, max(floor(p/3),1) for regression, where p is the number of features
#' @param nodeSize minimum size of terminal nodes, default value is 1 for classification, 5 for regression
#' @param sampSize size of sample to draw. Default value is n for bootstrap samples, 0.632*n for subsamples, where n is the number of observations
#' @param maxNodes maximum number of terminal nodes trees can have
#' @return List with the following components
#' \item{forest}{forest object}
#' \item{err.rate}{forest error rate}
#' \item{predictedAll}{n by ntree matrix with (i,j)th value being tree j's prediction for observation i, only returned if individualTrees=TRUE. If classification, refer to key to convert numbers to categories.} 
#' \item{predictedOOB}{predictedAll but with in-bag observations NA-ed, only returned if individualTrees=TRUE. If classification, refer to key to convert numbers to categories.}
#' \item{predicted}{length n vector of predictions based on out-of-bag observations}
#' \item{inbag.times}{n by ntree matrix (i,j)th value being number of times observation i was used in tree j}
#' \item{oob.times}{number of times observation was out-of-bag}
#' \item{type}{classification or regression}
#' \item{key}{mapping of numbers to categories for classification, NULL for regression}
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
  if (sum(is.na(y)) != 0) {
    stop('NA not permitted in response') 
  }
  
  if (sum(is.na(x)) != 0) {
    stop('NA not permitted in predictors') 
  }
  
  # Call C++ function
  #yOffset <- if (is.factor(y)) 0 else mean(y)     # consider moving this back in for speed improvements
  #if (!is.factor(y)) y <- y - yOffset
  out <- cppForest(data.matrix(x), y, sampSize, nodeSize, maxNodes, ntree, mtry, 
                      keepForest, replace, is.factor(y), ustat, B)
  #if (!is.factor(y)) y = y + yOffset
  #out$forest$nodePred <- out$forest$nodePred + yOffset
  #out$predictedByTree <- out$predictedByTree + yOffset
  
  # Store meta-data in forest object
  if (is.factor(y)) {
    out$type = "binary classification"
  
    # Find mapping of factor level to number
    # first (alphabetically-speaking) level becomes 1, second level becomes 2
    out$key = unique(data.frame(y,as.numeric(y)))
    out$key[,1] = as.character(out$key[,1])
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
               
  # Determine predictions of individual trees
  predictedAll = cppPredict(data.matrix(x), 
             out$forest$splitVar, 
             out$forest$split,
             out$forest$leftDaughter, 
             out$forest$rightDaughter,
            out$forest$nodePred) 
  
  # Convert numbers to characters if binary classification
  if (out$type == "binary classification")
  {
    predictedAll = numberToFactor(predictedAll,out$key)
  }
  
  # Determine OOB prediction
  predictedOOB = predictedAll
  predictedOOB[out$inbag.times!=0] = NA
  
  if (out$type == "binary classification") out$predicted = unlist(apply(predictedOOB,1,function(x) names(sort(table(x),decreasing=T))[1]))
  else out$predicted = rowMeans(predictedOOB,na.rm=T)
  
  # Store predictions of individual trees
  if (individualTrees) {
    out$predictedAll = predictedAll
    out$predictedOOB = predictedOOB
  }
  
  # If classification, calculate oob err.rate; if regression, oob mse
  if (out$type == "binary classification") out$err.rate = apply(predictedOOB,2,function(x) mean(x!=as.character(y),na.rm=TRUE))
  else out$mse = apply(predictedOOB,2,function(x) mean((x-y)^2,na.rm=TRUE))
  
  if (!keepForest) out$forest <- NULL
  
  class(out) = "forest"
  return(out)
}