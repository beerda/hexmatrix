#ifndef DIJKSTRA_H
#define DIJKSTRA_H


#include <queue>
#include <exception>
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


class DijkstraGraph : public Graph {
protected:
  std::priority_queue<DijkstraQueueElement> queue;
  const NumericVector dist;
  const NumericVector trans;

public:
  DijkstraGraph(const NumericVector aDist,
                const NumericVector aTrans,
                int aRows, int aCols, int aLayers = 1) :
  Graph(aRows, aCols, aLayers),
  queue(),
  dist(aDist),
  trans(aTrans)
  { }

  void addStart(const int vertex, const double price)
  {
    if (vertex < 0 || vertex > rowcols * layers) {
      throw std::out_of_range("'vertex' out of range in addStart()");
    }
    DijkstraQueueElement v = DijkstraQueueElement(vertex, price);
    queue.push(v);
  }

  const double edgePrice(const int vertex, const int direction) const
  {
    if (direction >= 6) {
      return trans[transIndex(vertex, direction)];
    } else if (direction >= 0) {
      return dist[distIndex(vertex, direction)];
    } else {
      throw std::out_of_range("Invalid 'direction' in edgePrice()");
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
