#ifndef DIJKSTRA_H
#define DIJKSTRA_H


#include <queue>
#include "hexmatrix.h"


class DijkstraQueueElement {
public:
  int i;
  double price;
  DijkstraQueueElement(int ai, double aPrice) : i(ai), price(aPrice)  { }

  friend bool operator<(const DijkstraQueueElement& v1, const DijkstraQueueElement& v2) {
    if (v1.price == v2.price)
      return v1.i < v2.i; // to behave in the same way on all platforms
    return v1.price > v2.price;
  }
};


class DijkstraGraph {
private:
  std::priority_queue<DijkstraQueueElement> queue;
  const NumericVector dist;
  const NumericVector trans;
  const int rows;
  const int cols;
  const int layers;
  const int rowcols;

public:
  DijkstraGraph(const NumericVector aDist,
                const NumericVector aTrans,
                int aRows, int aCols, int aLayers = 1) :
  queue(),
  dist(aDist),
  trans(aTrans),
  rows(aRows),
  cols(aCols),
  layers(aLayers),
  rowcols(aRows * aCols)
  { }

  void addStart(const int vertex, const double price)
  {
    DijkstraQueueElement v = DijkstraQueueElement(vertex, price);
    queue.push(v);
  }

  const int getDirections() const
  { return 6 + layers - 1; }

  void layerIndices(const int vertex,
                    const int direction,
                    int& sourceLayer,
                    int& targetLayer) const
  {
    sourceLayer = vertex / rowcols;
    if (direction < 6) {
      targetLayer = sourceLayer;
    } else {
      targetLayer = direction - 6;
      if (sourceLayer <= targetLayer) {
        targetLayer++;
      }
    }
  }

  const int neighbour(const int vertex, const int direction) const
  {
    int sourceLayer, targetLayer = 0;
    layerIndices(vertex, direction, sourceLayer, targetLayer);
    if (sourceLayer == targetLayer) {
      int res = neigh(direction, vertex % rowcols, rows, cols);
      if (res < 0) {
        return -1;
      }
      return res + sourceLayer * rowcols;
    } else {
      return vertex % rowcols + targetLayer * rowcols;
    }
  }

  const int oppositeDirection(const int vertex, const int direction) const
  {
    int sourceLayer, targetLayer = 0;
    layerIndices(vertex, direction, sourceLayer, targetLayer);
    if (sourceLayer == targetLayer) {
      return (direction + 3) % 6;
    } else {
      return (sourceLayer < targetLayer) ? 6 + sourceLayer : 6 + sourceLayer - 1;
    }
  }

  const double edgePrice(const int vertex, const int direction) const
  {
    int sourceLayer, targetLayer = 0;
    layerIndices(vertex, direction, sourceLayer, targetLayer);
    if (sourceLayer == targetLayer) {
      int index = vertex + rowcols * layers * direction;
      return dist[index];
    } else {
      int index = (vertex % rowcols) + rowcols
        * (sourceLayer * (layers - 1)
             + (sourceLayer < targetLayer ? targetLayer - 1 : targetLayer));
      return trans[index];
    }
  }

  void process(NumericVector &priceMatrix,
               NumericVector &pathMatrix,
               int target) {
    while (!queue.empty()) {
      DijkstraQueueElement cur = queue.top();
      queue.pop();

      if (cur.i == target)
        break; // path to target found

      for (int dir = 0; dir < getDirections(); ++dir) {
        int other = neighbour(cur.i, dir);
        if (other < 0)
          continue;

        int oppositeDir = oppositeDirection(cur.i, dir);
        double price = edgePrice(other, oppositeDir);
        if (!IS_FINITE(price))
          continue;

        double otherPrice = priceMatrix[other];
        double newPrice = cur.price + price;

        if ((!IS_FINITE(otherPrice)) || (otherPrice > newPrice)) {
          priceMatrix[other] = newPrice;
          pathMatrix[other] = cur.i;
          DijkstraQueueElement v = DijkstraQueueElement(other, newPrice);
          queue.push(v);
        }
      }
    }
  }
};


#endif
