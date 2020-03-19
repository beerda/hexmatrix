#include "hexmatrix.h"


void regionInternal(int index, double val, NumericMatrix &hist, NumericVector &res) {
  if (hist[index] == val) {
    res.push_back(index);
    hist[index] = val - 1;

    for (int dir = 0; dir < 6; ++dir) {
      int other = neigh(dir, index, hist.nrow(), hist.ncol());
      if (other >= 0) {
        regionInternal(other, val, hist, res);
      }
    }
  }
}


// [[Rcpp::export(name=".region")]]
NumericVector region(const NumericMatrix m, int index) {
  NumericVector res = NumericVector();
  NumericMatrix hist = clone(m);
  regionInternal(index, m[index], hist, res);
  return res;
}
