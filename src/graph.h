#ifndef HGRAPH_H
#define HGRAPH_H


#include <exception>
#include "hexmatrix.h"


class Graph {
public:
  const int rows;
  const int cols;
  const int layers;
  const int rowcols;

  Graph(int aRows, int aCols, int aLayers) :
  rows(aRows),
  cols(aCols),
  layers(aLayers),
  rowcols(aRows * aCols)
  { }

  const int getDirections() const
  { return 6 + layers - 1; }

  void layerIndices(const int vertex,
                    const int direction,
                    int& sourceLayer,
                    int& targetLayer) const
  {
    if (vertex < 0 || vertex > rowcols * layers) {
      throw std::out_of_range("vertex out of range in layerIndices()");
    }
    if (direction < 0 || direction > getDirections()) {
      throw std::out_of_range("direction out of range in layerIndices()");
    }
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
    if (vertex < 0 || vertex > rowcols * layers) {
      throw std::out_of_range("'vertex' out of range in layerIndices()");
    }
    if (direction < 0 || direction > getDirections()) {
      throw std::out_of_range("'direction' out of range in layerIndices()");
    }
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

  const int whichDirection(const int from, const int to) const
  {
    if (from < 0 || from > rowcols * layers) {
      throw std::out_of_range("'from' out of range in whichDirection()");
    }
    if (to < 0 || to > rowcols * layers) {
      throw std::out_of_range("'to' out of range in whichDirection()");
    }
    int dir = getDirections();
    while (dir >= 0) {
      if (neighbour(from, dir) == to)
        break;
      dir--;
    }
    return dir;
  }

  const int oppositeDirection(const int vertex, const int direction) const
  {
    if (vertex < 0 || vertex > rowcols * layers) {
      throw std::out_of_range("'vertex' out of range in oppositeDirection()");
    }
    if (direction < 0 || direction > getDirections()) {
      throw std::out_of_range("'direction' out of range in oppositeDirection()");
    }
    int sourceLayer, targetLayer = 0;
    layerIndices(vertex, direction, sourceLayer, targetLayer);
    if (sourceLayer == targetLayer) {
      return (direction + 3) % 6;
    } else {
      return (sourceLayer < targetLayer) ? 6 + sourceLayer : 6 + sourceLayer - 1;
    }
  }

  const int distIndex(const int vertex, const int direction) const
  {
    if (vertex < 0 || vertex > rowcols * layers) {
      throw std::out_of_range("'vertex' out of range in distIndex()");
    }
    if (direction < 0 || direction > 5) {
      throw std::out_of_range("'direction' out of range in distIndex()");
    }
    return vertex + rowcols * layers * direction;
  }

  const int transIndex(const int vertex, const int direction) const
  {
    if (vertex < 0 || vertex > rowcols * layers) {
      throw std::out_of_range("'vertex' out of range in transIndex()");
    }
    if (direction < 6 || direction > getDirections()) {
      throw std::out_of_range("'direction' out of range in transIndex()");
    }
    int sourceLayer, targetLayer = 0;
    layerIndices(vertex, direction, sourceLayer, targetLayer);
    return (vertex % rowcols) + rowcols
      * (sourceLayer * (layers - 1)
         + (sourceLayer < targetLayer ? targetLayer - 1 : targetLayer));
  }
};


#endif
