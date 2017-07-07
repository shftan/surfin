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
#'@author Sarah Tan <\email{ht395@cornell.edu}>, David Miller, Giles Hooker, Lucas Mentch
#'
#'Maintainer: Sarah Tan
#'
#'@section References:
#'Leo Breiman. (2001). Random Forests. Machine Learning 45(1), 5-32. http://link.springer.com/article/10.1023/A:1010933404324
#'
#'Lucas K. Mentch and Giles J. Hooker. (2016). Quantifying Uncertainty in Random Forests via Confidence Intervals and Hypothesis Tests. Journal of Machine Learning Research, 17(26), 1-41. http://www.jmlr.org/papers/volume17/14-168/14-168.pdf  
#'
#'Stefan Wager, Trevor Hastie, and Bradley Efron. (2014). Confidence Intervals for Random Forests: The Jackknife and the Infinitesimal Jackknife. Journal of Machine Learning Research, 15(May), 1625-1651. http://jmlr.org/papers/v15/wager14a.html
#'
#'@section See Also:
#'\code{\link{forest}}, \code{\link{predict.forest}}, \code{\link{forest.varU}}, \code{\link{forest.varIJ}} 
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