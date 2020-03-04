#include <Rcpp.h>
#include "neighbourIndices.h"

using namespace Rcpp;

int xDiff[] = {-1, -1, 0, 1, 1, 0};
int yDiffOdd[] = {-1, 0, 1, 0, -1, -1 };
int yDiffEven[] = {0, 1, 1, 1, 0, -1};

