#include "hexmatrix.h"


class PathReduceContext {
public:
  RObject* cache;
  List dist;
  List trans;
  NumericVector path;
  RObject origin;
  Function f;
  int rows;
  int cols;
  int rowcols;
  int layers;
  int length;

  PathReduceContext(List aDist,
                    List aTrans,
                    NumericVector aPath,
                    RObject aOrigin,
                    Function aF) :
    cache(NULL),
    dist(aDist),
    trans(aTrans),
    path(aPath),
    origin(aOrigin),
    f(aF)
  {
    NumericVector dim = aPath.attr("dim");
    rows = dim[0];
    cols = dim[1];
    layers = dim[2];
    rowcols = rows * cols;
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
    RObject res = pathReduceInternal(next, ctx);
    RObject price;
    int dir = whichDir(i, next, ctx.rows, ctx.cols);
    if (dir >= 0) {
      // direction within layer
      int index = i + ctx.length * dir;
      price = ctx.dist[index];
    } else {
      // direction between layers
      int sourceLayer = i / ctx.rowcols;
      int targetLayer = next / ctx.rowcols;
      int index = (i % ctx.rowcols) + ctx.rowcols
        * (sourceLayer * (ctx.layers - 1)
             + (sourceLayer < targetLayer ? targetLayer - 1 : targetLayer));
      price = ctx.trans[index];
    }
    ctx.cache[i] = ctx.f(price, res);
  }

  return ctx.cache[i];
}


// [[Rcpp::export(name=".pathReduce")]]
List pathReduce(const List dist,
                const List trans,
                const NumericVector path,
                const RObject origin,
                const Function f) {
  PathReduceContext ctx = PathReduceContext(dist, trans, path, origin, f);
  List result(ctx.length);

  for (int i = 0; i < ctx.length; ++i) {
    result[i] = pathReduceInternal(i, ctx);
  }

  return result;
}
