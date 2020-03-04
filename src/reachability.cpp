#include <Rcpp.h>
#include <queue>


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

int xDiff[6] = {-1, -1, 0, 1, 1, 0};
int yDiffOdd[6] = {-1, 0, 1, 0, -1, -1 };
int yDiffEven[6] = {0, 1, 1, 1, 0, -1};


// [[Rcpp::export(name=".reachability")]]
List reachability(const NumericMatrix m, const NumericVector dist) {
  NumericMatrix res = NumericMatrix(clone(m));
  std::priority_queue<Vertex> queue;
  int rows = m.nrow();
  int cols = m.ncol();
  int rowcols = rows * cols;

  // init queue
  for (int x = 0; x < rows; ++x) {
    for (int y = 0; y < cols; ++y) {
      double val = m(x, y);
      if (traits::is_finite<REALSXP>(val)) {
        Vertex v = Vertex(x, y, val);
        queue.push(v);
      }
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

      double edgePrice = dist[otherX + rows * otherY + rowcols * ((dir + 3) % 6)];
      if (!traits::is_finite<REALSXP>(edgePrice))
        continue;

      double otherPrice = res(otherX, otherY);
      double newPrice = cur.price + edgePrice;

      if ((!traits::is_finite<REALSXP>(otherPrice)) || (otherPrice > newPrice)) {
        res(otherX, otherY) = newPrice;
        Vertex other = Vertex(otherX, otherY, newPrice);
        queue.push(other);
      }
    }
  }

  List result = List::create(_["prices"] = res);
  return result;
}
