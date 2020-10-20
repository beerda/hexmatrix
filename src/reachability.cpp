#include <queue>
#include "hexmatrix.h"



class Vertex {
public:
  int i;
  double price;
  Vertex(int ai, double aPrice) : i(ai), price(aPrice)  { }
};


bool operator<(const Vertex& v1, const Vertex& v2) {
  if (v1.price == v2.price)
    return v1.i < v2.i; // to behave in the same way on all platforms

  return v1.price > v2.price;
}


bool dijkstraLoop(std::priority_queue<Vertex> &queue,
                  NumericMatrix &priceMatrix,
                  NumericMatrix &pathMatrix,
                  const NumericVector &dist,
                  int target) {
  int rows = priceMatrix.nrow();
  int cols = priceMatrix.ncol();
  int rowcols = rows * cols;
  bool changed = false;

  while (!queue.empty()) {
    Vertex cur = queue.top();
    queue.pop();

    if (cur.i == target)
      break; // path to target found

    for (int dir = 0; dir < 6; ++dir) {
      int other = neigh(dir, cur.i, rows, cols);
      if (other < 0)
        continue;

      int oppositeDir = (dir + 3) % 6;
      double edgePrice = dist[other + rowcols * oppositeDir];
      if (!IS_FINITE(edgePrice))
        continue;

      double otherPrice = priceMatrix[other];
      double newPrice = cur.price + edgePrice;

      if ((!IS_FINITE(otherPrice)) || (otherPrice > newPrice)) {
        changed = true;
        priceMatrix[other] = newPrice;
        pathMatrix[other] = cur.i;
        Vertex v = Vertex(other, newPrice);
        queue.push(v);
      }
    }
  }

  return changed;
}


// [[Rcpp::export(name=".reachability")]]
List reachability(const NumericMatrix m, const NumericVector dist, int target) {
  int rows = m.nrow();
  int cols = m.ncol();
  int rowcols = rows * cols;
  NumericMatrix priceMatrix = NumericMatrix(clone(m));  // cloned m
  NumericMatrix pathMatrix = NumericMatrix(rows, cols); // zero-filled
  NumericVector init = NumericVector();
  std::priority_queue<Vertex> queue;

  for (int i = 0; i < rowcols; ++i) {
    double val = m[i];
    if (IS_FINITE(val)) {
      init.push_back(i);
      Vertex v = Vertex(i, val);
      queue.push(v);
      pathMatrix[i] = -1;
    } else {
      pathMatrix[i] = NA_REAL;
    }
  }

  bool changed = dijkstraLoop(queue, priceMatrix, pathMatrix, dist, target);

  return List::create(
    _["prices"] = priceMatrix,
    _["paths"] = pathMatrix,
    _["init"] = init,
    _["changed"] = changed);
}


// [[Rcpp::export(name=".shortest")]]
List shortest(int source, int target, const NumericVector dist) {
  NumericVector dim = dist.attr("dim");
  int rows = dim[0];
  int cols = dim[1];
  int rowcols = rows * cols;

  NumericMatrix priceMatrix = NumericMatrix(rows, cols);  // zero-filled
  NumericMatrix pathMatrix = NumericMatrix(rows, cols);
  std::priority_queue<Vertex> queue;

  for (int i = 0; i < rowcols; ++i) {
    priceMatrix[i] = NA_REAL;
    pathMatrix[i] = NA_REAL;
  }
  priceMatrix[source] = 0;
  pathMatrix[source] = -1;
  Vertex v = Vertex(source, 0);
  queue.push(v);

  dijkstraLoop(queue, priceMatrix, pathMatrix, dist, target);

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
