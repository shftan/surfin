#'surfin
#'
#'Statistical Inference for Random Forests
#'
#'@section Description:
#'This R package computes variance estimates for random forest predictions using a fast implementation of random forests in C++. Two variance estimates are provided: U-statistic based and infinitesimal jackknife.
#'
#'@section Details:
#'Functions include: forest, forest.varU, forest.varIJ, predict.forest, proximity.forest, impute.forest
#'
#'@section Dependencies:
#'Rcpp and RcppArmadillo
#'
#'@author Sarah Tan, David Miller, Giles Hooker, Lucas Mentch, Stefan Wager
#'@docType package
#'@name surfin
#'@importFrom Rcpp evalCpp
#'@import RcppArmadillo
#'@useDynLib surfin
NULL