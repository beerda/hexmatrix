#include <Rcpp.h>
#include "neighbourIndices.h"

using namespace Rcpp;

int xDiff[] = {-1, -1, 0, 1, 1, 0};
int yDiffOdd[] = {-1, 0, 1, 0, -1, -1 };
int yDiffEven[] = {0, 1, 1, 1, 0, -1};


int neigh(int dir, int i, int rows, int cols)
{
  int curX = i % rows;
  int otherX = curX + xDiff[dir];
  if (otherX < 0 || otherX >= rows)
    return -1;

  int curY = i / rows;
  int otherY = curY + ((curX % 2 == 0) ? yDiffEven[dir] : yDiffOdd[dir]);
  if (otherY < 0 || otherY >= cols)
    return -1;

  return otherX + otherY * rows;
}
