#'surfin
#'
#'Statistical Inference for Random Forests
#'
#'@section Description:
#'This package computes standard error estimates for random forest predictions using a fast implementation of random forests in C++.
#'
#'@section Details:
#'Functions include: rf, predict.rf, impute.rf, proximity.rf, rf.varIJ, rf.varU
#'
#'@section Dependencies:
#'Rcpp and RcppArmadillo
#'
#'@author person
#'@docType package
#'@name surfin
#'@importFrom Rcpp evalCpp
#'@import RcppArmadillo
#'@useDynLib surfin
NULL