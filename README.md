
# surfin
Statistical Inference for Random Forests

This R package computes uncertainty for random forest predictions using a fast implementation of random forests in C++. Two variance estimates are provided: U-statistic based (Mentch & Hooker, 2016) and infinitesimal jackknife (Wager, Hastie, Efron, 2014). The latter is also available from the authors' R package <a href="http://www.github.com/swager/randomForestCI">randomForestCI</a>.

References:
Mentch L, Hooker G. <a href="http://jmlr.org/papers/v17/14-168.html">Quantifying uncertainty in random forests via confidence intervals and hypothesis tests.</a> Journal of Machine Learning Research. 2016.

Wager S, Hastie T, Efron B. <a href="http://jmlr.org/papers/v15/wager14a.html">Confidence intervals for random forests: the jackknife and the infinitesimal jackknife.</a> Journal of Machine Learning Research. 2014.

Download from: 
http://shftan.github.io/surfin/

Check out a demo on some real data: <a href="http://shftan.github.io/surfin/example.html">How Uncertain Are Your Random Forest Predictions?</a>

Feedback, bug reports, etc. are very much welcome! <a href="mailto:ht395@cornell.edu">Email me!</a>
