#ifndef HEXMATRIX_H
#define HEXMATRIX_H


#include <Rcpp.h>


using namespace Rcpp;


#define IS_FINITE(x) (!NumericVector::is_na(x) && !Rcpp::traits::is_nan<REALSXP>(x) && !Rcpp::traits::is_infinite<REALSXP>(x))


// neighbourIndices.cpp
extern int neigh(int dir, int i, int rows, int cols);
extern int whichDir(int cur, int other, int rows, int cols);

// reachability.cpp
extern List shortest(int source, int target, const NumericVector dist);

// region.cpp
extern NumericVector region(const NumericMatrix m, int index);

#endif
