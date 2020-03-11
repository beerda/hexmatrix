#include <Rcpp.h>
#include <queue>
#include "neighbourIndices.h"

using namespace Rcpp;


class Vertex {
public:
  int i;
  double price;
  Vertex(int ai, double aPrice) : i(ai), price(aPrice)  { }
};

bool operator<(const Vertex& v1, const Vertex& v2) {
  return v1.price > v2.price;
}


// [[Rcpp::export(name=".reachability")]]
List reachability(const NumericMatrix m, const NumericVector dist) {
  int rows = m.nrow();
  int cols = m.ncol();
  int rowcols = rows * cols;
  NumericMatrix priceMatrix = NumericMatrix(clone(m));  // cloned m
  NumericMatrix pathMatrix = NumericMatrix(rows, cols); // zero-filled
  NumericVector init = NumericVector();
  std::priority_queue<Vertex> queue;

  // init
  for (int i = 0; i < rowcols; ++i) {
    double val = m[i];
    if (traits::is_finite<REALSXP>(val)) {
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

    for (int dir = 0; dir < 6; ++dir) {
      int other = neigh(dir, cur.i, rows, cols);
      int oppositeDir = (dir + 3) % 6;
      double edgePrice = dist[other + rowcols * oppositeDir];
      if (!traits::is_finite<REALSXP>(edgePrice))
        continue;

      double otherPrice = priceMatrix[other];
      double newPrice = cur.price + edgePrice;

      if ((!traits::is_finite<REALSXP>(otherPrice)) || (otherPrice > newPrice)) {
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
