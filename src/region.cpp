#include <stack>
#include "hexmatrix.h"


// [[Rcpp::export(name=".region")]]
NumericVector region(const NumericVector m, int index) {
  NumericVector dim = m.attr("dim");
  int rows = dim[0];
  int cols = dim[1];
  NumericVector res = NumericVector();
  NumericVector hist = clone(m);

  double val = m[index];
  std::stack<int> stack;
  stack.push(index);
  while (!stack.empty()) {
    index = stack.top(); stack.pop();
    if (index >= 0 && index < hist.length() && IS_FINITE(hist[index]) && hist[index] == val) {
      res.push_back(index);
      hist[index] = val - 1;
      for (int dir = 0; dir < 6; ++dir) {
        int other = neigh(dir, index, rows, cols);
        if (other >= 0) {
          stack.push(other);
        }
      }
    }
  }

  return res;
}
