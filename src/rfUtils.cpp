//Includes/namespaces
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' cppProx
//' @description
//' Given an n x nTree matrix of terminal nodes, this function will return an n x n proximimity matrix.
//' @param nodes n x nTree matrix of terminal nodes
//' @details
//' The computed proximities are based on only the out-of-bag predictions. Zero-valued entries in nodes correspond to in-bag instances.
//' @export
// [[Rcpp::export]]
arma::mat cppProx(arma::umat nodes) {
  
  // Given an n x nTree matrix of terminal nodes, this function
  // will return an n x n proximimity matrix.
  
  // The computed proximities are based on only the out-of-bag predictions.
  // Zero-valued entries in nodes correspond to in-bag instances.
  
  //declare primitive bookkeeping variables
  int n = nodes.n_rows;
  int nTree = nodes.n_cols;
  int t, nodeCol; 
  arma::uvec::iterator r, c;
  
  //allocate memory for bookkeeping vectors/matrices
  arma::mat prox(n, n, arma::fill::zeros);
  arma::umat nOOB(n, n, arma::fill::zeros);
  arma::uvec oob, tree;
  
  //iterate through the trees
  for (t=0; t<nTree; ++t) {
    
    //get out-of-bag IDs
    tree = nodes.unsafe_col(t);
    oob = arma::find(tree);
    
    //update counts 
    for (c = oob.begin(); c < oob.end()-1; ++c) {
      nodeCol = tree(*c); 
      for (r = c+1; r < oob.end(); ++r) {
        nOOB(*r, *c)++;
        if (nodeCol == tree(*r)) 
          prox(*r, *c)++;
      }
    }
  }
  
  //compute proximities
  prox = symmatl( prox / nOOB);
  prox.diag().ones();
  return prox;
}

//' @title
//' cppPredict
//' @description
//' Returns predictions from a stored forest.
//' @param x = matrix of covariates for the test data
//' @param splitVar  = best split variables
//' @param split     = best split values
//' @param lDaughter = left daughter node assignments
//' @param nnodePred = terminal node predictions
//' @details
//' All other matrices represent the forest. Trees are columns, and nodes are rows.
//' @export
// [[Rcpp::export]]
NumericVector cppPredict(NumericMatrix x, IntegerMatrix splitVar, 
                         NumericMatrix split, IntegerMatrix lDaughter,
                         IntegerMatrix rDaughter, NumericMatrix  nodePred) {
                              
  // Returns predictions from a stored forest.
  //     x = matrix of covariates for the test data
  
  // All other matrices represent the forest. Trees are columns, and nodes are rows.
  //     splitVar  = best split variables 
  //     split     = best split values
  //     lDaughter = left daughter node assignments
  //     rDaughter = right daughter node assignments
  //     nnodePred = terminal node predictions

  //get stats
  int nSample = x.nrow();
  int nTree   = splitVar.ncol();
  NumericVector yPred(nSample);

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
      yPred[i] +=  nodePred(k-1, t);
    }
  }
  
  //normalize by number of trees
  yPred = yPred / nTree;
  return(yPred);
}
