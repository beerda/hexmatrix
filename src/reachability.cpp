#include <Rcpp.h>
#include <queue>
#include "neighbourIndices.h"

using namespace Rcpp;


class Vertex {
public:
  int x;
  int y;
  double price;
  Vertex(int ax, int ay, double aPrice) : x(ax), y(ay), price(aPrice)  { }
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
  NumericVector initx = NumericVector();
  NumericVector inity = NumericVector();
  std::priority_queue<Vertex> queue;

  // init
  for (int i = 0; i < rowcols; ++i) {
    double val = m[i];
    if (traits::is_finite<REALSXP>(val)) {
      int x = i % rows;
      int y = i / rows;
      initx.push_back(x + 1);
      inity.push_back(y + 1);
      Vertex v = Vertex(x, y, val);
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
      int otherX = cur.x + xDiff[dir];
      if (otherX < 0 || otherX >= rows)
        continue;

      int otherY = cur.y + ((cur.x % 2 == 0) ? yDiffEven[dir] : yDiffOdd[dir]);
      if (otherY < 0 || otherY >= cols)
        continue;

      int oppositeDir = (dir + 3) % 6;
      double edgePrice = dist[otherX + rows * otherY + rowcols * oppositeDir];
      if (!traits::is_finite<REALSXP>(edgePrice))
        continue;

      double otherPrice = priceMatrix(otherX, otherY);
      double newPrice = cur.price + edgePrice;

      if ((!traits::is_finite<REALSXP>(otherPrice)) || (otherPrice > newPrice)) {
        priceMatrix(otherX, otherY) = newPrice;
        pathMatrix(otherX, otherY) = oppositeDir + 1;
        Vertex other = Vertex(otherX, otherY, newPrice);
        queue.push(other);
      }
    }
  }

  return List::create(
    _["prices"] = priceMatrix,
    _["paths"] = pathMatrix,
    _["initRows"] = initx,
    _["initCols"] = inity);
}
