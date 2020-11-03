#include "hexmatrix.h"


void regionInternal(int index, double val, NumericVector &hist, NumericVector &res, int rows, int cols) {
  if (index >= 0 && index < hist.length() && hist[index] == val) {
    res.push_back(index);
    hist[index] = val - 1;

    for (int dir = 0; dir < 6; ++dir) {
      int other = neigh(dir, index, rows, cols);
      if (other >= 0) {
        regionInternal(other, val, hist, res, rows, cols);
      }
    }
  }
}


// [[Rcpp::export(name=".region")]]
NumericVector region(const NumericVector m, int index) {
  NumericVector dim = m.attr("dim");
  int rows = dim[0];
  int cols = dim[1];
  NumericVector res = NumericVector();
  NumericVector hist = clone(m);
  regionInternal(index, m[index], hist, res, rows, cols);
  return res;
}
