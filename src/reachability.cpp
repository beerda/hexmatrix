#include <Rcpp.h>
#include <queue>


using namespace Rcpp;


class Vertex {
public:
  int x;
  int y;
  double price;

  Vertex(int ax, int ay, double aPrice = 0) : x(ax), y(ay), price(aPrice) { }

  Vertex neighbour(int dir) {
    int newX = x + Vertex::xDiff[dir];
    int newY;
    if (x % 2 == 0) {
      newY = y + Vertex::yDiffEven[dir];
    } else {
      newY = y + Vertex::yDiffOdd[dir];
    }
    Vertex res = Vertex(newX, newY);
    return res;
  }

private:
  static int xDiff[6];
  static int yDiffOdd[6];
  static int yDiffEven[6];
};

int Vertex::xDiff[6] = {-1, -1, 0, 1, 1, 0};
int Vertex::yDiffOdd[6] = {-1, 0, 1, 0, -1, -1 };
int Vertex::yDiffEven[6] = {0, 1, 1, 1, 0, -1};

bool operator<(const Vertex& v1, const Vertex& v2) {
  return v1.price > v2.price;
}


inline const int oppositeDirection(const int dir) {
  return (dir + 3) % 6;
}


// [[Rcpp::export(name=".reachability")]]
List reachability(const NumericMatrix m, const NumericVector dist) {
  NumericMatrix res = NumericMatrix(clone(m));
  std::priority_queue<Vertex> queue;
  int rows = m.nrow();
  int cols = m.ncol();
  int rowcol = rows * cols;

  for (int x = 0; x < rows; ++x) {
    for (int y = 0; y < cols; ++y) {
      double val = m(x, y);
      if (traits::is_finite<REALSXP>(val)) {
        Vertex v = Vertex(x, y, val);
        queue.push(v);
      }
    }
  }

  while (!queue.empty()) {
    Vertex cur = queue.top();
    queue.pop();
    for (int dir = 0; dir < 6; ++dir) {
      Vertex other = cur.neighbour(dir);
      if (other.x < 0 || other.y < 0 || other.x >= rows || other.y >= cols)
        continue;
      other.price = res(other.x, other.y);
      double edgePrice = dist(other.x + rows * other.y + rowcol * oppositeDirection(dir));
      if (traits::is_finite<REALSXP>(edgePrice)) {
        if ((!traits::is_finite<REALSXP>(other.price)) || (other.price > cur.price + edgePrice)) {
          res(other.x, other.y) = cur.price + edgePrice;
          other.price = res(other.x, other.y);
          queue.push(other);
        }
      }
    }
  }

  List result = List::create(_["prices"] = res);
  return result;
}
