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
