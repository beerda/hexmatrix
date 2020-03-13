#include <Rcpp.h>
#include <queue>
#include "neighbourIndices.h"

#define IS_FINITE(x) (!NumericVector::is_na(x) && !Rcpp::traits::is_nan<REALSXP>(x) && !Rcpp::traits::is_infinite<REALSXP>(x))

using namespace Rcpp;


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


// [[Rcpp::export(name=".reachability")]]
List reachability(const NumericMatrix m, const NumericVector dist, int target) {
  int rows = m.nrow();
  int cols = m.ncol();
  int rowcols = rows * cols;
  NumericMatrix priceMatrix = NumericMatrix(clone(m));  // cloned m
  NumericMatrix pathMatrix = NumericMatrix(rows, cols); // zero-filled
  NumericVector init = NumericVector();
  std::priority_queue<Vertex> queue;
  target--;

  // init
  for (int i = 0; i < rowcols; ++i) {
    double val = m[i];
    if (IS_FINITE(val)) {
      init.push_back(i + 1);
      Vertex v = Vertex(i, val);
      queue.push(v);
    } else {
      pathMatrix[i] = NA_REAL;
    }
  }

  // main loop
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
        priceMatrix[other] = newPrice;
        pathMatrix[other] = cur.i + 1;
        Vertex v = Vertex(other, newPrice);
        queue.push(v);
      }
    }
  }

  return List::create(
    _["prices"] = priceMatrix,
    _["paths"] = pathMatrix,
    _["init"] = init);
}
