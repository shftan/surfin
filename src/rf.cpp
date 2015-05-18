//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

///////////////////////////
//      Header file      //
///////////////////////////

//define temporary data structure
struct TempData {
    double wgt;
    double yWgt;
};

//define helper function and value indicating a node terminal
#define swapInt(a, b) ((a ^= b), (b ^= a), (a ^= b))
#define NODE_TERMINAL -1

//define functions that will be used inside of other functions
void resample(int nOrig, int nSamp, int replace, IntegerVector& IDs, 
        std::vector<TempData>& d, double* ySum, const NumericVector& y);

void buildTree(const NumericMatrix& x, double ySum, int mtry, int nodeSize, 
        int nSamp, int maxNodes, int* splitVar, double* split, int* lDaughter, 
        int* rDaughter, double* sumNode, IntegerVector& idMap, 
        std::vector<TempData>& tmp, int classify);

void findBestSplit(int mtry, const NumericMatrix& x, NumericVector& xTry, int nUniq, 
        double sumParent, double nParent, int* idMap, int* idTry, int nodeStart,
        IntegerVector& vars, int* splitVar, double* split, double* sumLeft, 
        double* nLeft, int* nUniqLeft, std::vector<TempData>& tmp);
        
void findBestVal(int var, int* idTry, int nUniq, double nParent, 
        double sumParent, int* nUniqLeft, double* nLeft, double* sumLeft, 
        int* splitVar, double* split, int* idMap, const NumericVector& xTry,
        double* critMax, const std::vector<TempData>& tmp);
                 
void sampNoReplace(int* next, IntegerVector indices, int* nRemain);

void predictTree(const NumericMatrix& x, NumericVector& yPred,
        int* nodes, int* splitVar, double* split, int* lDaughter, 
        int* rDaughter, double* nodePred, NumericVector& nOOB, 
        std::vector<TempData>& d);

///////////////////////
//     Main file     //
///////////////////////

//' @title
//' cppForest
//' @description
//' Construct random forest
//' @param x matrix
//' @param y vector
//' @param nSamp number of samples
//' @param nodeSize node size
//' @param maxNodes maximum number of nodes
//' @param nTree number of trees desired
//' @param mtry tuning
//' @param keepF keep forest or not
//' @param replace bootstrap samples or subsamples
//' @param classify perform classification or regression
//' @details
//' Options available for bootstrap samples or subsamples
//' @export
// [[Rcpp::export]]
List cppForest(NumericMatrix& x, NumericVector& y, int nSamp, int nodeSize, 
    int maxNodes, int nTree, int mtry, int keepF, int replace, int classify) {
    
    //allocate memory for the forest
    //will be 1-column matrices if keepForest = FALSE
    int nt = keepF ? nTree : 1;
    IntegerMatrix splitVar(maxNodes, nt);
    NumericMatrix split(maxNodes, nt);
    IntegerMatrix lDaughter(maxNodes, nt);
    IntegerMatrix rDaughter(maxNodes, nt);
    NumericMatrix nodePred(maxNodes, nt);
    
    //allocate memory for other variables
    int n = x.nrow();
    double ySum = 0.0;
    IntegerMatrix nodes(n, nTree);
    NumericVector yPred(n);    
    NumericVector nOOB(n);
    IntegerVector IDs(n);
    std::vector<TempData> tmp;
    tmp.reserve(n);

    //interface with R's random number generator
    GetRNGstate();
    
    //BIG LOOP HERE
    //iterate over the number of trees
    int i, t;
    for (i = 0; i < nTree; ++i) {
      
      //reset weights and resample
      resample(n, nSamp, replace, IDs, tmp, &ySum, y);
      
      //tree index depends on setting for keepForest
      t = keepF ? i : 0;

      //grow the regression tree 
      buildTree(x, ySum, mtry, nodeSize, nSamp, maxNodes, splitVar(_,t).begin(),
                split(_,t).begin(), lDaughter(_,t).begin(), rDaughter(_,t).begin(),
                nodePred(_,t).begin(),  IDs, tmp, classify);
    
      //update OOB predictions
      predictTree(x, yPred, nodes(_,t).begin(), splitVar(_,t).begin(),
                  split(_,t).begin(), lDaughter(_,t).begin(), rDaughter(_,t).begin(), 
                  nodePred(_,t).begin(), nOOB,  tmp);
    }
    
    //normalize the y predictions
    yPred = yPred / nOOB;
    
    //interface with R's random number generator
    PutRNGstate();
    
    //return as nested list
    return List::create(
            Named("nodes")     = nodes,
            Named("predicted") = yPred,
            Named("forest") =  Rcpp::List::create(
              Named("splitVar")      = splitVar,
              Named("split")         = split,
              Named("leftDaughter")  = lDaughter,
              Named("rightDaughter") = rDaughter,
              Named("nodePred")      = nodePred,
              Named("y") = y));  // remove after testing
}

//build an individual regression tree
void buildTree(const NumericMatrix& x, double ySum, int mtry, int nodeSize, 
              int nSamp, int maxNodes, int* splitVar, double* split,
              int* lDaughter, int* rDaughter, double* sumNode,  
              IntegerVector& idMap, std::vector<TempData>& tmp, int classify) {

    //allocate memory for this function and findBestSplit
    int i, ncur, nUnique=0;
    int n = x.nrow();
    int* idStart;
    NumericVector xTry(n);
    IntegerVector idTry(n);
    IntegerVector vars(x.ncol());
    IntegerVector nodeStart(maxNodes);
    IntegerVector nodeUniq(maxNodes);
    NumericVector nNode(maxNodes);

    //initialize ID map
    for (i = 0; i < x.nrow(); ++i) {
      if (tmp[i].wgt > 0) {
        idMap[nUnique] = i;
        nUnique++;
      }
    }

    //initialize parent node
    ncur = 0;
    sumNode[0] = ySum;
    nNode[0] = nSamp;
    nodeStart[0] = 0;
    nodeUniq[0] = nUnique;
    
    //BIG LOOP HERE
    //iterate through all possible nodes
    for (i = 0; i < maxNodes - 2; ++i) {
      
      //don't exceed the maximum number of nodes
      if (i > ncur || ncur >= maxNodes - 2) break;
      
      //check stopping criteria: nodesize and (if classification) homogeneity in y
      if (nNode[i] <= nodeSize)                                splitVar[i] = NODE_TERMINAL;
      if (classify && (sumNode[i]==0 || sumNode[i]==nNode[i])) splitVar[i] = NODE_TERMINAL;
      
      //skip if at a terminal node
      if (splitVar[i] == NODE_TERMINAL) continue;
      
      //initialize temporary ID vector for next call to findBestSplit
      idStart = idMap.begin() + nodeStart[i];
      memcpy(idTry.begin(), idStart, nodeUniq[i] * sizeof(int));
      
      //find the best split and do "pointer ninja" to directly update values 
      findBestSplit(mtry, x, xTry, nodeUniq[i], sumNode[i], nNode[i], idStart, 
                    idTry.begin(), nodeStart[i], vars, &splitVar[i], &split[i], 
                    &sumNode[ncur + 1], &nNode[ncur + 1], &nodeUniq[ncur + 1], tmp);
          
      //if no valid split was found, move on
      if (splitVar[i] == NODE_TERMINAL) continue;
      
      //build the tree map
      lDaughter[i] = ncur + 1 + 1;
      rDaughter[i] = ncur + 2 + 1;

      //update bookkeeping about daughter nodes 
      nodeStart[ncur + 1] = nodeStart[i];
      nodeStart[ncur + 2] = nodeStart[i] + nodeUniq[ncur + 1];
      nodeUniq[ncur + 2]  = nodeUniq[i]  - nodeUniq[ncur + 1];
      sumNode[ncur + 2]   = sumNode[i]   - sumNode[ncur + 1];
      nNode[ncur + 2]     = nNode[i]     - nNode[ncur + 1];

      //augment the tree by two nodes
      ncur += 2;
    }
    
    //once finished with loop over nodes, get terminal node predictions
    for (i = 0; i < maxNodes - 1; ++i) sumNode[i] /= nNode[i];
}

//find a split at an individual node
void findBestSplit(int mtry, const NumericMatrix& x, NumericVector& xTry, int nUniq, 
        double sumParent, double nParent, int* idMap, int* idTry, int nodeStart,
        IntegerVector& vars, int* splitVar, double* split, double* sumLeft, 
        double* nLeft, int* nUniqLeft, std::vector<TempData>& tmp) {
    
    //assume node is terminal, unless proven otherwise...
    *splitVar = NODE_TERMINAL;
      
    //reset bookkeeping variables
    int i, j, var;
    double critMax = 0.0;
    vars = seq_along( vars );
    int last = vars.size();
    
    //BIG LOOP HERE
    //try up to mtry variables to split the node
    for (i = 0; i < mtry; ++i) {

      //sample without replacement from the variables
      sampNoReplace(&var, vars, &last);
      
      //get x values to sort 
      for (j=0; j<nUniq; ++j) xTry[j] = x( idTry[j], var-1);
      
      //sort the x values (R_qsort_I sorts "in-place")
      R_qsort_I(xTry.begin(), (int*) idTry, 1, nUniq);
     
      //find the best splitting value for this current variable
      findBestVal(var, idTry, nUniq, nParent, sumParent, nUniqLeft, nLeft,
                 sumLeft, splitVar, split, idMap, xTry, &critMax, tmp);
    }
}

//find the best splitting value for this current variable
void findBestVal(int var, int* idTry, int nUniq, double nParent, 
        double sumParent, int* nUniqLeft, double* nLeft, double* sumLeft, 
        int* splitVar, double* split, int* idMap, const NumericVector& xTry,
        double* critMax, const std::vector<TempData>& tmp) {
              
    // start by placing the lowest x value into the left daughter node
    double nL       = tmp[ idTry[0] ].wgt;
    double sumL  = tmp[ idTry[0] ].yWgt;
    double xLeft = xTry[0];
    double xMax  = xTry[nUniq-1];
            
    // BIG LOOP HERE
    double crit, xRight, sumR;
    for (int j = 1; j < nUniq; ++j) {

        //If xLeft = xMax, then all values between them 
        //are the same. Therefore skip to the next variable.
        if (xLeft == xMax) return;

        //If the split is valid...
        xRight = xTry[j];
        if (xLeft != xRight) {
          
          //compute criteria
          sumR = sumParent - sumL;
          crit = (sumL * sumL / nL) + (sumR * sumR / (nParent - nL));
          
          //update bookkeeping if the split is improved
          if (crit > *critMax) {
            *critMax = crit;
            *splitVar = var;
            *split = (xLeft + xRight) / 2.0;
            *nLeft = nL;
            *sumLeft = sumL;
            *nUniqLeft = j;
            memcpy(idMap, idTry, nUniq * sizeof(int));
          }
        }
        
        //move the current obs to the left daughter node
        sumL += tmp[ idTry[j] ].yWgt;
        nL   += tmp[ idTry[j] ].wgt;
        xLeft = xRight;
      }
}

//generate a new sample for the different trees
void resample(int nOrig, int nSamp, int replace, IntegerVector& IDs, 
              std::vector<TempData>& tmp, double* ySum, const NumericVector& y) {
              
  //reset weights
  int i, id, nRemain;
  for (i=0; i<nOrig; ++i) tmp[i].wgt = 0.0;
  
  //sample with replacement
  if (replace) {       
    for (i = 0; i < nSamp; ++i) {
      id = nOrig * unif_rand();
      tmp[id].wgt++;
    }
    
  //sample without replacement
  } else {          
    for (i = 0; i < nOrig; ++i) IDs[i] = i;
    nRemain = nOrig;
    for (i = 0; i < nSamp; ++i) {
      sampNoReplace(&id, IDs, &nRemain);
      tmp[id].wgt++;
    }
  }
  
  //calculate weighted y's (needed later for splitting criteria)
  *ySum = 0.0;
  for (i = 0; i < nOrig; ++i) {
    tmp[i].yWgt = y[i] * tmp[i].wgt; 
    *ySum += tmp[i].yWgt;
  }
}
        
//generate OOB predictions from a built tree
void predictTree(const NumericMatrix& x, NumericVector& yPred, int* nodes, 
        int* splitVar, double* split, int* lDaughter, int* rDaughter, 
        double* nodePred, NumericVector& nOOB, std::vector<TempData>& tmp) {
          
    //iterate through the observations
    int oob, var, nd = 0;
    for (int i = 0; i < x.nrow(); ++i) {
      
      //skip in-bag observations
      if (tmp[i].wgt>0) continue;
      
      //iterate through nodes in the tree
      while (splitVar[nd] > 0) { 
        var = splitVar[nd] - 1;
        nd = (x(i, var) <= split[nd]) ?          
              lDaughter[nd] - 1 : rDaughter[nd] - 1;
      }
      
      //Reached terminal node. Update predictions and reset node
      nOOB[i]++;
      yPred[i] += nodePred[nd];
      nodes[i] = nd+1; 
      nd = 0; 
    }
}

//helper function for sampling without replacement
void sampNoReplace(int* next, IntegerVector indices, int* nRemain) {
  int tmp = (int) (unif_rand() * (*nRemain));
  *next = indices[tmp];
  swapInt(indices[tmp], indices[*nRemain-1]);
  *nRemain -= 1;
}