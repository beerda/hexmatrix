#include "hexmatrix.h"


class PathReduceContext : public Graph {
public:
  RObject* cache;
  List dist;
  List trans;
  NumericVector path;
  RObject origin;
  Function f;
  int length;

  PathReduceContext(List aDist,
                    List aTrans,
                    NumericVector aPath,
                    RObject aOrigin,
                    Function aF,
                    int aRows,
                    int aCols,
                    int aLayers) :
    Graph(aRows, aCols, aLayers),
    cache(NULL),
    dist(aDist),
    trans(aTrans),
    path(aPath),
    origin(aOrigin),
    f(aF)
  {
    length = aRows * aCols * aLayers;
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

  int prev = ctx.path[i];
  if (prev == 0) { // initial point (root)
    ctx.cache[i] = ctx.origin;
    return ctx.cache[i];
  }

  prev--; // switch to C-like indexing
  ctx.cache[i] = naVec; // avoid infinite loop in cyclic paths

  if (prev >= 0 && prev <= ctx.length) {
    RObject res = pathReduceInternal(prev, ctx);
    RObject price;
    int dir = ctx.whichDirection(prev, i);
    if (dir >= 6) {
      // direction between layers
      price = ctx.trans[ctx.transIndex(prev, dir)];
    } else if (dir >= 0) {
      // direction within layer
      price = ctx.dist[ctx.distIndex(prev, dir)];
    } else {
      throw std::out_of_range("Invalid 'dir' in pathReduceInternal()");
    }
      // int index = (i % ctx.rowcols) + ctx.rowcols
    //     * (sourceLayer * (ctx.layers - 1)
    //          + (sourceLayer < targetLayer ? targetLayer - 1 : targetLayer));
    //   price = ctx.trans[index];
    // }
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
  NumericVector dim = path.attr("dim");
  PathReduceContext ctx = PathReduceContext(dist, trans, path, origin, f,
                                            dim[0], dim[1], dim[2]);
  List result(ctx.length);

  for (int i = 0; i < ctx.length; ++i) {
    result[i] = pathReduceInternal(i, ctx);
  }

  return result;
}
