#include <Rcpp.h>
#include "neighbourIndices.h"


using namespace Rcpp;


class PathReduceContext {
public:
  RObject* cache;
  List data;
  NumericMatrix path;
  RObject origin;
  Function f;
  int rows;
  int cols;
  int rowcols;

  PathReduceContext(List aData, NumericMatrix aPath, RObject aOrigin, Function aF) :
    cache(NULL), data(aData), path(aPath), origin(aOrigin), f(aF),
    rows(aPath.nrow()), cols(aPath.ncol()), rowcols(aPath.nrow() * aPath.ncol())
  { cache = new RObject[rowcols]; }

  ~PathReduceContext()
  { delete[] cache; }
};


LogicalVector naVec = LogicalVector::create(NA_LOGICAL);


RObject pathReduceInternal(const int i, PathReduceContext& ctx) {
  if (ctx.cache[i] != R_NilValue) {
    return ctx.cache[i];
  }

  int next = ctx.path[i];
  if (next == 0) { // initial point (root)
    ctx.cache[i] = ctx.origin;
    return ctx.cache[i];
  }

  next--; // switch to C-like indexing
  ctx.cache[i] = naVec; // avoid infinite loop in cyclic paths

  if (next >= 0 && next <= ctx.rowcols) {
    int dir = whichDir(i, next, ctx.rows, ctx.cols);
    RObject res = pathReduceInternal(next, ctx);
    ctx.cache[i] = ctx.f(ctx.data[i + ctx.rowcols * dir], res);
  }

  return ctx.cache[i];
}


// [[Rcpp::export(name=".pathReduce")]]
List pathReduce(const List data, const NumericMatrix path, const RObject origin, const Function f) {
  PathReduceContext ctx = PathReduceContext(data, path, origin, f);
  List result(ctx.rowcols);

  for (int i = 0; i < ctx.rowcols; ++i) {
    result[i] = pathReduceInternal(i, ctx);
  }

  return result;
}
