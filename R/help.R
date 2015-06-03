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
#'Version: 0.0.0.9000
#'
#'Date: 2015-06-08
#'
#'License: GPL-3
#'
#'URL: \url{http://shftan.github.io/surfin}
#'
#'BugReports: \url{http://github.com/shftan/surfin/issues}
#'
#'@section Dependencies:
#'Rcpp, RcppArmadillo, Matrix
#'
#'@author Sarah Tan <ht395 AT cornell.edu>, David Miller, Giles Hooker, Lucas Mentch, Stefan Wager
#'
#'Maintainer: Sarah Tan
#'
#'@section References:
#' Mentch, Lucas, and Hooker, Giles. (2014). Ensemble Trees and CLTs: Statistical Inference for Supervised Learning. Arxiv.
#'
#' Wager, Stefan, Hastie, Trevor, and Efron, Bradley. (2014). Confidence Intervals for Random Forests: The Jackknife and the Infinitesimal Jackknife. Journal of Machine Learning Research. 
#'
#'@section Examples:
#'data(birds)
#'
#'?birds
#'
#'@docType package
#'@name surfin
#'@importFrom Rcpp evalCpp
#'@import RcppArmadillo
#'@useDynLib surfin
NULL