#include "hexmatrix.h"


int xDiff[] = {-1, -1, 0, 1, 1, 0};
int yDiffOdd[] = {-1, 0, 1, 0, -1, -1 };
int yDiffEven[] = {0, 1, 1, 1, 0, -1};


int neigh(int dir, int i, int rows, int cols)
{
  int rowcol = rows * cols;
  int curX = i % rows;
  int otherX = curX + xDiff[dir];
  if ((otherX < 0) || (otherX >= rows))
    return -1;

  int curY = (i % rowcol) / rows;
  int otherY = curY + ((curX % 2 == 0) ? yDiffEven[dir] : yDiffOdd[dir]);
  if ((otherY < 0) || (otherY >= cols))
    return -1;

  int otherZ = i / rowcol;

  return otherX + otherY * rows + otherZ * rowcol;
}


// [[Rcpp::export(name=".whichDir")]]
int whichDir(int cur, int other, int rows, int cols)
{
  int dir = 5;
  while (dir >= 0) {
    if (neigh(dir, cur, rows, cols) == other)
      break;
    dir--;
  }

  return dir;
}
