#include <Rcpp.h>
#include "neighbourIndices.h"


using namespace Rcpp;


class PathReduceContext {
public:
  RObject* cache;
  List data;
  NumericMatrix path;
  Function f;
  int rows;
  int cols;
  int rowcols;

  PathReduceContext(List aData, NumericMatrix aPath, Function aF) :
    cache(NULL), data(aData), path(aPath), f(aF),
    rows(aPath.nrow()), cols(aPath.ncol()), rowcols(aPath.nrow() * aPath.ncol())
  { cache = new RObject[rowcols]; }

  ~PathReduceContext()
  { delete[] cache; }
};


RObject pathReduceInternal(const int i, PathReduceContext &ctx) {
  if (ctx.cache[i] != R_NilValue) {
    return ctx.cache[i];
  }

  int dir = ctx.path[i];
  if (dir == 0) { // initial point (root)
    ctx.cache[i] = ctx.data[i];
    return ctx.cache[i];
  }
  dir--; // switch to C-like direction indexing

  ctx.cache[i] = LogicalVector::create(NA_LOGICAL); // zabrani cyklickym cestam

  int curX = i % ctx.rows;
  int curY = i / ctx.rows;
  int otherX = curX + xDiff[dir];
  int otherY = curY + ((curX % 2 == 0) ? yDiffEven[dir] : yDiffOdd[dir]);
  if (otherX < 0 || otherX >= ctx.rows || otherY < 0 || otherY >= ctx.cols) {
    return ctx.cache[i];
  }

  RObject res = pathReduceInternal(otherX + ctx.rows * otherY, ctx);
  ctx.cache[i] = ctx.f(ctx.data[i], res);
  return ctx.cache[i];
}


// [[Rcpp::export(name=".pathReduce")]]
List pathReduce(const List data, const NumericMatrix path, const Function f) {
  PathReduceContext ctx = PathReduceContext(data, path, f);
  List result;

  for (int i = 0; i < ctx.rowcols; ++i) {
    result.push_back(pathReduceInternal(i, ctx));
  }

  return result;
}
