#'surfin
#'
#'surfin: Statistical Inference for Random Forests.
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
#' Mentch, Lucas, and Giles Hooker. (2015). Quantifying Uncertainty in Random Forests via Confidence Intervals and Hypothesis Tests. arXiv preprint arXiv:1404.6473.
#'
#' Wager, Stefan, Hastie, Trevor, and Efron, Bradley. (2014). Confidence Intervals for Random Forests: The Jackknife and the Infinitesimal Jackknife. Journal of Machine Learning Research, 15(May), 1625-1651. \url{http://jmlr.org/papers/v15/wager14a.html}
#'
#'@section Examples:
#'See the vignette for an example:
#'
#'vignette("example",package="surfin")
#'
#'We've also provided a data set:
#'
#'?birds
#'
#'data(birds)
#'
#'@docType package
#'@name surfin
#'@importFrom Rcpp evalCpp
#'@import RcppArmadillo
#'@useDynLib surfin
NULL