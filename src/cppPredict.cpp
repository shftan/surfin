//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' cppPredict
//' @description
//' Returns predictions from a stored forest.
//' @param x = matrix of covariates for the test data
//' @param splitVar  = best split variables
//' @param split     = best split values
//' @param lDaughter = left daughter node assignments
//' @param rDaughter = right daughter node assignments
//' @param nodePred = terminal node predictions
//' @details
//' All other matrices represent the forest. Trees are columns, and nodes are rows.
//' @export
// [[Rcpp::export]]
NumericMatrix cppPredict(NumericMatrix x, IntegerMatrix splitVar, 
                         NumericMatrix split, IntegerMatrix lDaughter,
                         IntegerMatrix rDaughter, NumericMatrix  nodePred) {

  //get stats
  int nSample = x.nrow();
  int nTree   = splitVar.ncol();
  //NumericVector yPred(nSample);
  NumericMatrix yPred(nSample, nTree); 

  //iterate through the trees and observations
  int t, i, k, var;
  for (t = 0; t < nTree; ++t) {
    for (i = 0; i < nSample; ++i) {
      
      //start at the root node k = 1
      k    = 1;
      var  = splitVar(k-1, t);
      
      //loop until a terminal node...
      while (var > 0) { 
        
        //compare covariate and split values to get next node
        k = (x(i, var-1) <= split(k-1, t)) ? 
          lDaughter(k-1, t) : rDaughter(k-1, t);
        
        //get splitting variable
        var = splitVar(k-1, t);
      }
      
      //update prediction and move to next obs
      yPred(i,t) = nodePred(k-1, t);
      //yPred[i] +=  nodePred(k-1, t);
    }
  }
  
  //normalize by number of trees
  //yPred = yPred / nTree;
  
  return(yPred);
}