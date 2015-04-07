#' Random forest wrapper function
#'
#' This function is a wrapper for the C++ implementation of random forest
#' @param x matrix
#' @param y vector
#' @param nTree number of trees desired, default is 500
#' @param replace bootstrap samples or subsamples, default is bootstrap samples
#' @param keepForest keep forest or not, default is TRUE
#' @param mtry tuning
#' @param nodeSize node size
#' @param nSamp number of samples
#' @param maxNodes maximum number of nodes
#' @keywords random forest, causal inference
#' @export
#' @examples
#' fastRF()

fastRF <- function (x, y, nTree = 500, replace = TRUE, keepForest = TRUE,
          mtry     = if (is.factor(y)) floor(sqrt(ncol(x))) else max(floor(ncol(x)/3), 1), 
          nodeSize = if (is.factor(y)) 1                    else 5, 
          nSamp    = if (replace) nrow(x) else ceiling(0.632 * nrow(x)),
          maxNodes = 2 * trunc(nSamp/max(1, nodeSize - 4)) + 1) {
  
  varNames <- if (is.null(colnames(x))) 1:ncol(x) else colnames(x)
  yOffset <- if (is.factor(y)) 0 else mean(y)
  if (!is.factor(y)) y <- y - yOffset
  rfout <- cppForest(data.matrix(x), y, nSamp, nodeSize, maxNodes, nTree, mtry, 
                      keepForest, replace, is.factor(y))
  rfout$forest$nodePred <- rfout$forest$nodePred + yOffset
  rfout$predicted       <- rfout$predicted + yOffset
  rfout$varNames        <- varNames
  if (!keepForest) rfout$forest <- NULL
  return(rfout)
}