#' The infinitesimal jackknife for random forests
#'
#' Calculate the infinitesimal jackknife variance
#' @param rf A random forest trained with replace = TRUE and keep.inbag = TRUE
#' @keywords random forest, variance, infinitesimal jackknife
#' @export
#' @examples
#' features = matrix(rnorm(100),nrow=10)
#' response = runif(10) 
#' forestobject = forest(x=features,y=response)
#' varIJ = forest.varIJ(forestobject)

forest.varIJ <- function (rf) {
  #
  # Extract tree-wise predictions and variable counts from random forest
  #
  
  B = dim(rf$inbag.times)[2]
  n = dim(rf$inbag.times)[1]
  s = sum(rf$inbag.times) / rf$ntree
  
  pred = rf$predictedAll
  # in case of classification, convert character labels to numeric (!)
  #class(pred) = "numeric"
  pred = matrix(as.numeric(factor(pred)),nrow=n)
  y.hat = rowMeans(pred)
  pred.centered = pred - rowMeans(pred)
  
  N = Matrix::Matrix(rf$inbag[, used.trees], sparse = TRUE)
  N.avg = Matrix::rowMeans(N)
  
  #
  # Compute raw infinitesimal jackknife
  #
  
  C = N %*% t(pred.centered) - Matrix::Matrix(N.avg, nrow(N), 1) %*% Matrix::Matrix(rowSums(pred.centered), 1, nrow(pred.centered))
  raw.IJ = Matrix::colSums(C^2) / B^2
  
  #
  # Apply Monte Carlo bias correction
  #
  
  N.var = mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
  boot.var = rowSums(pred.centered^2) / B
  bias.correction = n * N.var * boot.var / B
  vars = raw.IJ - bias.correction
  
  results = data.frame(y.hat=y.hat, var.hat=vars)
}

