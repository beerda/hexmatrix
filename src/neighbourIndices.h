#ifndef NEIGHBOUR_INDICES_H
#define NEIGHBOUR_INDICES_H

extern int xDiff[];
extern int yDiffOdd[];
extern int yDiffEven[];

extern int neigh(int dir, int i, int rows, int cols);
int whichDir(int cur, int other, int rows, int cols);

#endif
