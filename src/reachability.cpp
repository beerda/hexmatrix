#include "hexmatrix.h"


// [[Rcpp::export(name=".reachability")]]
List reachability(const NumericVector m,
                  const NumericVector dist,
                  const NumericVector trans,
                  int target) {
  NumericVector dim = m.attr("dim");
  int rows = dim[0];
  int cols = dim[1];
  int layers = dim[2];
  int rowcols = rows * cols;
  int rowcolayers = rowcols * layers;
  NumericVector priceMatrix = NumericVector(clone(m));  // cloned m
  NumericVector pathMatrix = NumericVector(Dimension(rows, cols, layers)); // zero-filled
  NumericVector init = NumericVector();
  DijkstraGraph graph(dist, trans, rows, cols, layers);

  for (int i = 0; i < rowcolayers; ++i) {
    double val = m[i];
    if (IS_FINITE(val)) {
      init.push_back(i);
      graph.addStart(i, val);
      pathMatrix[i] = -1;
    } else {
      pathMatrix[i] = NA_REAL;
    }
  }

  graph.process(priceMatrix, pathMatrix, target);

  return List::create(
    _["prices"] = priceMatrix,
    _["paths"] = pathMatrix,
    _["init"] = init);
}


// [[Rcpp::export(name=".shortest")]]
List shortest(int source, int target, const NumericVector dist) {
  NumericVector dim = dist.attr("dim");
  int rows = dim[0];
  int cols = dim[1];
  int rowcols = rows * cols;

  NumericMatrix priceMatrix = NumericMatrix(rows, cols);  // zero-filled
  NumericMatrix pathMatrix = NumericMatrix(rows, cols);
  DijkstraGraph graph(dist, rows, cols, 1);

  for (int i = 0; i < rowcols; ++i) {
    priceMatrix[i] = NA_REAL;
    pathMatrix[i] = NA_REAL;
  }
  priceMatrix[source] = 0;
  pathMatrix[source] = -1;
  graph.addStart(source, 0);
  graph.process(priceMatrix, pathMatrix, target);

  NumericVector prices;
  NumericVector paths;
  int cur = target;
  do {
    if (NumericVector::is_na(pathMatrix[cur]))
      return List::create();
    paths.push_back(cur);
    prices.push_back(priceMatrix[cur]);
    cur = pathMatrix[cur];
  } while (cur >= 0);

  std::reverse(paths.begin(), paths.end());
  std::reverse(prices.begin(), prices.end());

  return List::create(_["prices"] = prices, _["path"] = paths);
}
