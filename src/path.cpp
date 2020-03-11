#include <Rcpp.h>


using namespace Rcpp;


// [[Rcpp::export(name=".path")]]
NumericVector path(int to, NumericMatrix paths) {
  NumericVector res = NumericVector();
  do {
    if (to < 1 || to > paths.length()) {
      NumericVector fail = NumericVector();
      fail.push_back(NA_REAL);
      return fail;
    }
    res.push_back(to);
    to = paths[to - 1];
  } while (to);

  return res;
}
