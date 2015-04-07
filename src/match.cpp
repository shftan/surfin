//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' cppMatch
//' @description
//' Nearest neighbor 1:1 matching within and between conditions. 
//' @param tr vector
//' @param dist distance
//' @param type corresponds to R's representation of distance. 'v' = vector of pairwise distances (class "dist"), 'm' = matrix of pairwise distances (coerced as a vector), 'p' = vector of propensity scores
//' @details
//' Ties are broken using the the first match (not randomly).
//' @export
// [[Rcpp::export]]
DataFrame cppMatch(IntegerVector tr, 
                   NumericVector dist, 
                   char type) {
  // Nearest neighbor 1:1 matching within and between conditions. 
  // Ties are broken using the the first match (not randomly).

  // Note: type corresponds to R's representation of dist
  //   type 'v' = vector of pairwise distances (class "dist")
  //   type 'm' = matrix of pairwise distances (coerced as a vector)
  //   type 'p' = vector of propensity scores

  //keep track of best indices and distances
  int n = tr.size();
  IntegerVector dMatch(n, -1);
  IntegerVector sMatch(n, -1);
  NumericVector dBest(n, R_PosInf);
  NumericVector sBest(n, R_PosInf);
            
  double d;
  int row, col, i=0;
  for (row=0; row<n-1; ++row) {
    for (col=row+1; col<n; ++col) {

      //get distance and update counter
      if (type=='v') d = dist[i];
      if (type=='m') d = dist[row + col*n];
      if (type=='p') d = std::abs(dist[row] - dist[col]);
      i++;
      
      //if pair is in different conditions...
      if (tr[col] != tr[row]) {
        if (d < dBest[row]) { dBest[row] = d; dMatch[row] = col; }
        if (d < dBest[col]) { dBest[col] = d; dMatch[col] = row; }
      }

      //if pair is in the same condition...
      if (tr[col] == tr[row]) {
        if (d < sBest[row]) { sBest[row] = d; sMatch[row] = col; }
        if (d < sBest[col]) { sBest[col] = d; sMatch[col] = row; }
      }
    }
  }
  
  //get # of times k each obs was matched between conditions
  IntegerVector k(n, 0);
  for (i=0; i<n; ++i) k[dMatch[i]]++;
  
  //combine matches into a data frame
  DataFrame matches = DataFrame::create(
    Named("tr")    = tr,
    Named("diffM") = dMatch+1,
    Named("sameM") = sMatch+1,
    Named("k")     = k);
            
  return matches;
}

