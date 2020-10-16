#include "hexmatrix.h"


class PathReduceContext {
public:
  RObject* cache;
  List data;
  NumericVector path;
  RObject origin;
  Function f;
  int rows;
  int cols;
  int layers;
  int length;

  PathReduceContext(List aData, NumericVector aPath, RObject aOrigin, Function aF) :
    cache(NULL), data(aData), path(aPath), origin(aOrigin), f(aF)
  {
    NumericVector dim = aPath.attr("dim");
    rows = dim[0];
    cols = dim[1];
    layers = dim[2];
    length = rows * cols * layers;
    cache = new RObject[length];
  }

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

  if (next >= 0 && next <= ctx.length) {
    int dir = whichDir(i, next, ctx.rows, ctx.cols, ctx.layers);
    RObject res = pathReduceInternal(next, ctx);
    ctx.cache[i] = ctx.f(ctx.data[i + ctx.length * dir], res);
  }

  return ctx.cache[i];
}


// [[Rcpp::export(name=".pathReduce")]]
List pathReduce(const List data, const NumericVector path, const RObject origin, const Function f) {
  PathReduceContext ctx = PathReduceContext(data, path, origin, f);
  List result(ctx.length);

  for (int i = 0; i < ctx.length; ++i) {
    result[i] = pathReduceInternal(i, ctx);
  }

  return result;
}
