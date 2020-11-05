#include "hexmatrix.h"


// [[Rcpp::export(name=".shortest")]]
List shortest(int source,
              int target,
              const NumericVector dist,
              const NumericVector trans) {
  NumericVector dim = dist.attr("dim");
  DijkstraGraph graph(dist, trans, dim[0], dim[1], dim[2]);
  NumericVector priceMatrix = NumericVector(graph.length);  // zero-filled
  NumericVector pathMatrix = NumericVector(graph.length);  // zero-filled

  for (int i = 0; i < graph.length; ++i) {
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
