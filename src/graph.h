#ifndef HGRAPH_H
#define HGRAPH_H


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
};


#endif
