#include "hexmatrix.h"


int xDiff[] = {-1, -1, 0, 1, 1, 0};
int yDiffOdd[] = {-1, 0, 1, 0, -1, -1 };
int yDiffEven[] = {0, 1, 1, 1, 0, -1};


int neigh2D(int dir, int i, int rows, int cols)
{
  int curX = i % rows;
  int otherX = curX + xDiff[dir];
  if ((otherX < 0) || (otherX >= rows))
    return -1;

  int curY = i / rows;
  int otherY = curY + ((curX % 2 == 0) ? yDiffEven[dir] : yDiffOdd[dir]);
  if ((otherY < 0) || (otherY >= cols))
    return -1;

  return otherX + otherY * rows;
}


int neigh(int dir, int i, int rows, int cols) {
  if (dir < 6) {
    int layer = i / (rows * cols);
    int res = neigh2D(dir, i % (rows * cols), rows, cols);
    return res + layer * rows * cols;
  } else {
    int sourceLayer = i / (rows * cols);
    int targetLayer = dir - 6;
    if (sourceLayer <= targetLayer) {
      targetLayer++;
    }
    return i % (rows * cols) + targetLayer * rows * cols;
  }
}


// [[Rcpp::export(name=".whichDir")]]
int whichDir(int cur, int other, int rows, int cols, int layers)
{
  int dir = 6 + layers - 2;
  while (dir >= 0) {
    if (neigh(dir, cur, rows, cols) == other)
      break;
    dir--;
  }

  return dir;
}
