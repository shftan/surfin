//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

//#include "cppPredict.h"   // use when merging cppPredict and predictTree functions at some later time   //delete this line

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
void resample(int nOrig, int sampSize, int replace, IntegerVector& IDs, 
        std::vector<TempData>& d, double* ySum, const NumericVector& y);

void buildTree(const NumericMatrix& x, double ySum, int mtry, int nodeSize, 
        int sampSize, int maxNodes, int* splitVar, double* split, int* lDaughter, 
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

// NumericMatrix na_matrix(int n, int p);

//void predictTree(const NumericMatrix& x, double* yPred,
//        int* nodes, int* splitVar, double* split, int* lDaughter, 
//        int* rDaughter, double* nodePred, NumericVector& nOOB,
//        std::vector<TempData>& tmp);

///////////////////////
//     Main file     //
///////////////////////

//' @title
//' cppForest
//' @description
//' Construct random forest
//' @param x matrix
//' @param y vector
//' @param sampSize number of samples
//' @param nodeSize node size
//' @param maxNodes maximum number of nodes
//' @param nTree number of trees desired
//' @param mtry tuning
//' @param keepF keep forest or not
//' @param replace bootstrap samples or subsamples
//' @param classify perform classification or regression
//' @param ustat u-statistic based or infinitesimal jackknife
//' @param B number of common observations for u-statistic based variance estimate
//' @details
//' Options available for bootstrap samples or subsamples
//' @export
// [[Rcpp::export]]
List cppForest(NumericMatrix& x, NumericVector& y, int sampSize, int nodeSize, 
    int maxNodes, int nTree, int mtry, int keepF, int replace, int classify, int ustat, int B) {
    
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
//    NumericMatrix yPred(n, nTree);   
    NumericVector nOOB(n);
    IntegerVector IDs(n);
    std::vector<TempData> tmp;
    tmp.reserve(n);
    NumericVector obsB(B);

    //interface with R's random number generator
    GetRNGstate();
    
    int i, t, L;

    //// Sampling for U-statistic based variance estimate
    // Sample without replacement with common observations
    if (ustat) {   
		int b, l, id, nRemain;
		for (i = 0; i < n; ++i) tmp[i].wgt = 0.0;
		for (i = 0; i < n; ++i) IDs[i] = i;
    	nRemain = n;
    	for (i = 0; i < B; ++i) {
      		sampNoReplace(&id, IDs, &nRemain);
      		obsB[i] = id;
    	}
		
		L = double(nTree) / B;
		
		nt = 0;
		for (b = 0; b < B; ++b) {
			for (l = 0; l < L; ++l) {			
				//// For each tree
				for (i=0; i<n; ++i) tmp[i].wgt = 0.0;
				
				// First observation already sampled
				tmp[obsB[b]].wgt++;	
				for (i = 0; i < n; ++i) IDs[i] = i;
				nRemain = n;
				swapInt(IDs[obsB[b]], IDs[nRemain-1]);
    			nRemain -= 1;
  
    			// Sample rest of the observations
    			for (i = 0; i < (sampSize-1); ++i) {
      				sampNoReplace(&id, IDs, &nRemain);
      				tmp[id].wgt++;
    			}
    			
  				ySum = 0.0;
  				for (i = 0; i < n; ++i) {
    				tmp[i].yWgt = y[i] * tmp[i].wgt;   //calculate weighted y's (needed later for splitting criteria) 
    				ySum += tmp[i].yWgt;
    				nodes(i,nt) = tmp[i].wgt;   // in-bag status by tree
    				if (tmp[i].wgt==0) nOOB[i]++;      // number of trees where observation was out of bag
  				}
             				
  				//tree index depends on setting for keepForest
      			t = keepF ? nt : 0;

      			//grow the regression tree 
      			buildTree(x, ySum, mtry, nodeSize, sampSize, maxNodes, splitVar(_,t).begin(),
                	split(_,t).begin(), lDaughter(_,t).begin(), rDaughter(_,t).begin(),
                	nodePred(_,t).begin(),  IDs, tmp, classify);
    
      			//update predictions, both out-of-bag and in-bag
//      			predictTree(x, yPred(_,t).begin(), nodes(_,t).begin(), splitVar(_,t).begin(),
//                	split(_,t).begin(), lDaughter(_,t).begin(), rDaughter(_,t).begin(), 
//                	nodePred(_,t).begin(), nOOB, tmp);

                nt++;	
			}	
		}
	}
	//// Sampling for infinitesimal jackknife or typical sampling without replacement
	else {
		//// For each tree
    	for (nt = 0; nt < nTree; ++nt) {
      		//reset weights and resample
      		resample(n, sampSize, replace, IDs, tmp, &ySum, y);
      
      		for (i = 0; i < n; ++i) {
          		nodes(i,nt) = tmp[i].wgt;   // in-bag status by tree
    			if (tmp[i].wgt==0) nOOB[i]++;      // number of trees where observation was out of bag
    		}
    
      		//tree index depends on setting for keepForest
      		t = keepF ? nt : 0;

      		//grow the regression tree 
      		buildTree(x, ySum, mtry, nodeSize, sampSize, maxNodes, splitVar(_,t).begin(),
                split(_,t).begin(), lDaughter(_,t).begin(), rDaughter(_,t).begin(),
                nodePred(_,t).begin(),  IDs, tmp, classify);
    
      		//update predictions, both out-of-bag and in-bag
//      		predictTree(x, yPred(_,t).begin(), nodes(_,t).begin(), splitVar(_,t).begin(),
//                  split(_,t).begin(), lDaughter(_,t).begin(), rDaughter(_,t).begin(), 
 //                 nodePred(_,t).begin(), nOOB, tmp);
        }
    }
    
    
    ////normalize the y predictions
    ////yPred = yPred / nOOB;
    
    //interface with R's random number generator
    PutRNGstate();
    
    //return as nested list
    return List::create(
            Named("inbag.times")     = nodes,
//            Named("predictedAll") = yPred,
            Named("oob.times") = nOOB,
            Named("forest") =  Rcpp::List::create(
              Named("splitVar")       = splitVar,
              Named("split")         = split,
              Named("leftDaughter")  = lDaughter,
              Named("rightDaughter") = rDaughter,
              Named("nodePred")      = nodePred));
}

//build an individual regression tree
void buildTree(const NumericMatrix& x, double ySum, int mtry, int nodeSize, 
              int sampSize, int maxNodes, int* splitVar, double* split,
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
    nNode[0] = sampSize;
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
void resample(int nOrig, int sampSize, int replace, IntegerVector& IDs, 
              std::vector<TempData>& tmp, double* ySum, const NumericVector& y) {
              
  //reset weights
  int i, id, nRemain;
  for (i=0; i<nOrig; ++i) tmp[i].wgt = 0.0;
  
  // sample with replacement
  if (replace) {       
    for (i = 0; i < sampSize; ++i) {
      id = nOrig * unif_rand();
      tmp[id].wgt++;
    }
  }
  
  //sample without replacement
  else {          
    for (i = 0; i < nOrig; ++i) IDs[i] = i;
    nRemain = nOrig;
    for (i = 0; i < sampSize; ++i) {
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
        
//generate predictions from a built tree
//void predictTree(const NumericMatrix& x, double* yPred, int* nodes, 
//        int* splitVar, double* split, int* lDaughter, int* rDaughter, 
//        double* nodePred, NumericVector& nOOB, std::vector<TempData>& tmp) {
          
    //iterate through the observations
//    int var, nd = 0;
//    for (int i = 0; i < x.nrow(); ++i) {
      
      ////skip in-bag observations
      ////if (tmp[i].wgt>0) continue;
      
      //iterate through nodes in the tree
//      while (splitVar[nd] > 0) { 
//        var = splitVar[nd] - 1;
//        nd = (x(i, var) <= split[nd]) ?          
//              lDaughter[nd] - 1 : rDaughter[nd] - 1;
//      }
      
      //Reached terminal node. Update predictions and reset node
//      if (tmp[i].wgt==0) {
//      nOOB[i]++;
//      }
//      nodes[i] = tmp[i].wgt; //nodes[i] = nd+1;
//      yPred[i] = nodePred[nd]; 
//      nd = 0; 
//    }
//}

//helper function for sampling without replacement
void sampNoReplace(int* next, IntegerVector indices, int* nRemain) {
  int tmp = (int) (unif_rand() * (*nRemain));
  *next = indices[tmp];
  swapInt(indices[tmp], indices[*nRemain-1]);
  *nRemain -= 1;
}

// helper function for initialization of NA matrices
//NumericMatrix na_matrix(int n, int p){
//  NumericMatrix m(n,p) ;
//  std::fill( m.begin(), m.end(), NumericVector::get_na() ) ;
//  return m ;
//}