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


class Graph {
private:
  const NumericVector dist;
  const int rows, cols, layers;

public:
  Graph(const NumericVector aDist, int aRows, int aCols, int aLayers = 1) :
    dist(aDist), rows(aRows), cols(aCols), layers(aLayers)
  { }

  const int getDirections() const
  { return 6 + layers - 1; }

  const int neighbour(const int vertex, const int direction) const
  {
    if (direction < 6) {
      int layer = vertex / (rows * cols);
      int res = neigh(direction, vertex % (rows * cols), rows, cols);
      return res + layer * rows * cols;
    } else {
      int sourceLayer = vertex / (rows * cols);
      int targetLayer = direction - 6;
      if (sourceLayer <= targetLayer) {
        targetLayer++;
      }
      return vertex % (rows * cols) + targetLayer * rows * cols;
    }
  }

  const int oppositeDirection(const int vertex, const int direction) const
  {
    if (direction < 6) {
      return (direction + 3) % 6;
    } else {
      int sourceLayer = vertex / (rows * cols);
      int targetLayer = direction - 6;
      if (sourceLayer <= targetLayer) {
        targetLayer++;
      }
      return (sourceLayer < targetLayer) ? sourceLayer : sourceLayer - 1;
    }
  }

  const double edgePrice(const int vertex, const int direction) const
  {
    return dist[vertex + rows * cols * layers * direction];
  }
};


void dijkstraLoop(std::priority_queue<Vertex> &queue,
                  NumericVector &priceMatrix,
                  NumericVector &pathMatrix,
                  const Graph &graph,
                  int target) {
  while (!queue.empty()) {
    Vertex cur = queue.top();
    queue.pop();

    if (cur.i == target)
      break; // path to target found

    for (int dir = 0; dir < graph.getDirections(); ++dir) {
      //int other = neigh(dir, cur.i, rows, cols);
      int other = graph.neighbour(cur.i, dir);
      if (other < 0)
        continue;

      //int oppositeDir = (dir + 3) % 6;
      //double edgePrice = dist[other + rowcols * oppositeDir];
      int oppositeDir = graph.oppositeDirection(cur.i, dir);
      double edgePrice = graph.edgePrice(other, oppositeDir);
      if (!IS_FINITE(edgePrice))
        continue;

      double otherPrice = priceMatrix[other];
      double newPrice = cur.price + edgePrice;

      if ((!IS_FINITE(otherPrice)) || (otherPrice > newPrice)) {
        priceMatrix[other] = newPrice;
        pathMatrix[other] = cur.i;
        Vertex v = Vertex(other, newPrice);
        queue.push(v);
      }
    }
  }
}


// [[Rcpp::export(name=".reachability")]]
List reachability(const NumericVector m, const NumericVector dist, int target) {
  NumericVector dim = m.attr("dim");
  int rows = dim[0];
  int cols = dim[1];
  int layers = dim[2];
  int rowcols = rows * cols;
  int rowcolayers = rowcols * layers;
  NumericVector priceMatrix = NumericVector(clone(m));  // cloned m
  NumericVector pathMatrix = NumericVector(Dimension(rows, cols, layers)); // zero-filled
  NumericVector init = NumericVector();
  std::priority_queue<Vertex> queue;

  for (int i = 0; i < rowcolayers; ++i) {
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

  const Graph graph(dist, rows, cols, layers);
  dijkstraLoop(queue, priceMatrix, pathMatrix, graph, target);

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
  std::priority_queue<Vertex> queue;

  for (int i = 0; i < rowcols; ++i) {
    priceMatrix[i] = NA_REAL;
    pathMatrix[i] = NA_REAL;
  }
  priceMatrix[source] = 0;
  pathMatrix[source] = -1;
  Vertex v = Vertex(source, 0);
  queue.push(v);

  const Graph graph(dist, rows, cols, 1);
  dijkstraLoop(queue, priceMatrix, pathMatrix, graph, target);

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
