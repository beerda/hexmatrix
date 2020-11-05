#include "hexmatrix.h"


// [[Rcpp::export(name=".reachability")]]
List reachability(const NumericVector m,
                  const NumericVector dist,
                  const NumericVector trans,
                  int target) {
  NumericVector dim = m.attr("dim");
  DijkstraGraph graph(dist, trans, dim[0], dim[1], dim[2]);
  NumericVector priceMatrix = NumericVector(clone(m));  // cloned m
  NumericVector pathMatrix = NumericVector(Dimension(graph.rows, graph.cols, graph.layers)); // zero-filled
  NumericVector init = NumericVector();

  for (int i = 0; i < graph.length; ++i) {
    double val = m[i];
    if (IS_FINITE(val)) {
      init.push_back(i);
      graph.addStart(i, val);
      pathMatrix[i] = -1;
    } else {
      pathMatrix[i] = NA_REAL;
    }
  }

  bool changed = graph.process(priceMatrix, pathMatrix, target);

  return List::create(
    _["prices"] = priceMatrix,
    _["paths"] = pathMatrix,
    _["init"] = init,
    _["changed"] = changed);
}
