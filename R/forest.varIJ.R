#' Wrapper to infinitesimal jackknife variance estimate code by Stefan Wager (https://github.com/swager/randomForestCI/)
#'
#' Wrapper to infinitesimal jackknife variance estimate code by Stefan Wager (https://github.com/swager/randomForestCI/)
#' @param predictedAll a matrix with ntree rows where each element is the individual tree's prediction for that prediction
#' @param object A random forest trained with replace = TRUE
#' @return predictions for each observation and corresponding variance
#' @references Stefan Wager, Trevor Hastie, and Bradley Efron. (2014). Confidence Intervals for Random Forests: The Jackknife and the Infinitesimal Jackknife. Journal of Machine Learning Research, 15(May), 1625-1651. http://jmlr.org/papers/v15/wager14a.html
#' @seealso \code{\link{forest.varU}} 
#' @keywords random forest, variance, infinitesimal jackknife
#' @export
#' @examples
#' features = birds[,setdiff(names(birds),"detected")]
#' response = birds[,"detected"]
#' object = forest(x=features,y=response,individualTrees=TRUE)
#' varIJ = forest.varIJ(object$predictedAll,object)

forest.varIJ <- function (predictedAll,object) {
  # Ensure correct sampling scheme
  if (!object$replace) stop('infinitesimal jackknife variance estimate requires sampling with replacement')  

  # Ensure predictions have same number of trees as forest
  if (is.null(dim(predictedAll))) stop('predictedAll must be a matrix of individual tree predictions')
  if (ncol(predictedAll)!=object$ntree) stop('predictedAll do not have the same number of columns as the number of trees in the forest object')

  if (class(predictedAll[1,1])%in%c("factor","character")) {
    y.hat = collapseClassPred(predictedAll) 
    pred = factorToNumber(predictedAll,object$key)
  } else 
  {
  	y.hat = collapseRegPred(predictedAll)
  	pred = predictedAll
  }

  results = infJack(pred, object$inbag.times, calibrate=TRUE, used.trees = NULL)
  
  return(results)
}

### Infinitesimal jackknife variance estimate code by Stefan Wager (https://github.com/swager/randomForestCI/)
#'
#' The infinitesimal jackknife for random forests
#'
#' @param pred A nrow(newdata) by no. of trees matrix which contains numeric predictions
#'        from a random forest trained with trees grown on bootstrap samples of the training data
#' @param inbag A number of obs. in the training data by no. of trees matrix giving the
#'        number of times the ith observation in the training data appeared in the bootstrap sample for the jth tree.
#' @param calibrate whether to apply calibration to mitigate Monte Carlo noise
#'        warning: if calibrate = FALSE, some variance estimates may be negative
#'                 due to Monte Carlo effects if the number of trees in rf is too small
#' @param used.trees set of trees to use for variance estimation; uses all tress if NULL
#' @import stats
#' @export
infJack = function(pred, inbag, calibrate = TRUE, used.trees = NULL) {

        if (is.null(used.trees)) {
                used.trees = 1:ncol(inbag)
        }

        pred = pred[, used.trees, drop=FALSE]
        
        # check if sampling without replacement
        no.replacement = (max(inbag) == 1)
        
        #
        # Extract tree-wise predictions and variable counts from random forest
        #
        
        B = length(used.trees)
        n = as.numeric(nrow(inbag))
        s = sum(inbag) / ncol(inbag)

        y.hat = rowMeans(pred)
        pred.centered = pred - rowMeans(pred)
        
        N = Matrix::Matrix(inbag[, used.trees], sparse = TRUE)
        N.avg = Matrix::rowMeans(N)
        
        #
        # Compute raw infinitesimal jackknife
        #
        
        if (B^2 > n * as.numeric(nrow(pred))) {
                
                C = Matrix::tcrossprod(N, pred.centered) -
                      Matrix::Matrix(N.avg, nrow(N), 1) %*%
                        Matrix::Matrix(rowSums(pred.centered), 1, nrow(pred.centered))
                raw.IJ = Matrix::colSums(C^2) / B^2
        
        } else {
        
                # Faster implementation when n is large. Uses the fact that
                # colSums((A - B)^2) = T1 - 2 * T2 + T3,
                # where T1 = diag(A'A), T2 = diag(B'A), and T3 = diag(B'B)
                
                NTN = Matrix::crossprod(N, N)
                NTNPT_T = Matrix::tcrossprod(pred.centered, NTN)
                T1 = Matrix::rowSums(pred.centered * NTNPT_T)
                
                RS = rowSums(pred.centered)
                NbarTN = Matrix::crossprod(N.avg, N)
                T2 = RS * Matrix::tcrossprod(NbarTN, pred.centered)
                
                T3 = sum(N.avg^2) * RS^2
                raw.IJ = as.numeric(T1 - 2 * T2 + T3) / B^2
                
        }
        
        #
        # Apply Monte Carlo bias correction
        #
        
        N.var = mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
        boot.var = rowSums(pred.centered^2) / B
        bias.correction = n * N.var * boot.var / B
        vars = raw.IJ - bias.correction
        
        #
        # Finite sample correction
        #
        
        if (no.replacement) {
                
                variance.inflation = 1 / (1 - mean(inbag))^2
                vars = variance.inflation * vars
        }

        results = data.frame(y.hat=y.hat, var.hat=vars)

        if (nrow(results) <= 20 & calibrate) {
                calibrate = FALSE
                warning("No calibration with n <= 20")
        }
        
        #
        # If appropriate, calibrate variance estimates; this step in particular
        # ensures that all variance estimates wil be positive.
        #

        if (calibrate) {
                # Compute variance estimates using half the trees
                calibration.ratio = 2
                n.sample = ceiling(B / calibration.ratio)
                results.ss = infJack(pred, inbag, calibrate = FALSE, used.trees = sample(used.trees, n.sample))
                
                # Use this second set of variance estimates to estimate scale of Monte Carlo noise
                sigma2.ss = mean((results.ss$var.hat - results$var.hat)^2)
                delta = n.sample / B
                sigma2 = (delta^2 + (1 - delta)^2) / (2 * (1 - delta)^2) * sigma2.ss
                
                # Use Monte Carlo noise scale estimate for empirical Bayes calibration
                vars.calibrated = calibrateEB(vars, sigma2)
                results$var.hat = vars.calibrated
        }
        
        
        return(results)
}

### Infinitesimal jackknife variance estimate code by Stefan Wager (https://github.com/swager/randomForestCI/)
#'
#' Fit an empirical Bayes prior in the hierarchical model
#'     mu ~ G, X ~ N(mu, sigma^2)
#'
#' @param X a vector of observations
#' @param sigma noise estimate
#' @param p tuning parameter -- number of parameters used to fit G
#' @param nbin tuning parameter -- number of bins used for discrete approximation
#' @param unif.fraction tuning parameter -- fraction of G modeled as "slab"
#'
#' @return posterior density estimate g
#'
#' @section References:
#' For more details about "g-estimation", see: B Efron. Two modeling strategies for
#' empirical Bayes estimation. Stat. Sci., 29(2): 285–301, 2014.
#' @export

gfit = function(X, sigma, p = 2, nbin = 1000, unif.fraction = 0.1) {
        
        xvals = seq(min(min(X) - 2 * sd(X), 0), max(max(X) + 2 * sd(X), sd(X)), length.out = nbin)
        binw = xvals[2] - xvals[1]
        
        zero.idx = max(which(xvals <= 0))
        noise.kernel = dnorm(xvals / sigma) * binw / sigma
        
        if (zero.idx > 1) {
                noise.rotate = noise.kernel[c(zero.idx:length(xvals), 1:(zero.idx - 1))]
        } else {
                noise.rotate = noise.kernel
        }
        
        XX = sapply(1:p, function(j) xvals^j * as.numeric(xvals >= 0))
        neg.loglik = function(eta) {
                g.eta.raw = exp(XX %*% eta) * as.numeric(xvals >= 0)
                if ((sum(g.eta.raw) == Inf) | (sum(g.eta.raw) <= 100 * .Machine$double.eps)) {
                        return (1000 * (length(X) + sum(eta^2)))
                }
                g.eta.main = g.eta.raw / sum(g.eta.raw)
                g.eta = (1 - unif.fraction) * g.eta.main +
                        unif.fraction * as.numeric(xvals >= 0) / sum(xvals >= 0)
                f.eta = convolve(g.eta, noise.rotate)
                sum(approx(xvals, -log(pmax(f.eta, 0.0000001)), X)$y)
        }
        
        eta.hat = nlm(neg.loglik, rep(-1, p))$estimate
        g.eta.raw = exp(XX %*% eta.hat) * as.numeric(xvals >= 0)
        g.eta.main = g.eta.raw / sum(g.eta.raw)
        g.eta = (1 - unif.fraction) * g.eta.main +
                unif.fraction * as.numeric(xvals >= 0) / sum(xvals >= 0)
        
        return(data.frame(x=xvals, g=g.eta))
}

#' Bayes posterior estimation with Gaussian noise
#'
#' @param x0 an obsevation
#' @param g.est a prior density, as returned by gfit
#' @param sigma noise estimate
#'
#' @return posterior estimate E[mu | x0]
#' @export

gbayes = function(x0, g.est, sigma) {
        Kx = dnorm((g.est$x - x0) / sigma)
        post = Kx * g.est$g
        post = post / sum(post)
        sum(post * g.est$x)
}

#' Empirical Bayes calibration of noisy variance estimates
#'
#' @param vars list of variance estimates
#' @param sigma2 estimate of the Monte Carlo noise in vars
#'
#' @return calibrated variance estimates
#' @export

calibrateEB = function(vars, sigma2) {
        
        if(sigma2 <= 0 | min(vars) == max(vars)) {
                return(pmax(vars, 0))
        }
        
        sigma = sqrt(sigma2)
        eb.prior = gfit(vars, sigma)
        
        if (length(vars >= 200)) {
                # If there are many test points, use interpolation to speed up computations
                calib.x = quantile(vars, q = seq(0, 1, by = 0.02))
                calib.y = sapply(calib.x, function(xx) gbayes(xx, eb.prior, sigma))
                calib.all = approx(x=calib.x, y=calib.y, xout=vars)$y
        } else {
                calib.all = sapply(vars, function(xx) gbayes(xx, eb.prior, sigma))
        }
        return(calib.all)
}