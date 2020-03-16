#include <vector>
#include <queue>
#include "hexmatrix.h"


class Path {
public:
  NumericVector path;
  NumericVector prices;
  double price;


  Path(NumericVector aPath, NumericVector aPrices) :
    path(aPath),
    prices(aPrices),
    price(aPrices[aPrices.length() - 1])
  { }


  Path(List l) : Path(l("path"), l("prices"))
  { }


  Path(Path rootPath, int spurIndex, List l) :
    path(), prices(), price(0)
  {
    NumericVector otherPath = l("path");
    NumericVector otherPrices = l("prices");

    for (int i = 0; i <= spurIndex; ++i) {
      prices.push_back(rootPath.prices[i]);
      path.push_back(rootPath.path[i]);
    }
    price = rootPath.prices[spurIndex];
    for (int i = 0; i < otherPath.size(); ++i) {
      price += otherPrices[i];
      prices.push_back(price);
      path.push_back(otherPath[i]);
    }
  }


  bool commonPrefix(Path other, int spurIndex) {
    if (spurIndex >= path.length() || spurIndex >= other.path.length())
      return false;
    for (int i = 0; i <= spurIndex; ++i) {
      if (path[i] != other.path[i])
        return false;
    }
    return true;
  }
};


bool operator<(const Path& p1, const Path& p2) {
  return p1.price > p2.price;
}


class Distance {
public:
  NumericVector dist;
  int step;
  int dilat;
  int rows;
  int cols;
  int rowcols;
  int offset;

  Distance(const NumericVector aDist, int aStep, int aDilat) :
    dist(clone(aDist)),
    step(aStep),
    dilat(aDilat),
    rows(0),
    cols(0),
    rowcols(0),
    offset(aDilat)
  {
    NumericVector dim = aDist.attr("dim");
    rows = dim[0];
    cols = dim[1];
    rowcols = rows * cols;
  }

  void disableAfter(Path p, int spurIndex) {
    //if (p.path.length() > spurIndex + step + offset) {
      //for (int i = 1; i <= step; ++i) {
        //disableNode(p.path[spurIndex + i + offset]);
      //}
    //}
    int from = spurIndex + offset;
    int to = p.path.length() - offset;
    double amountDiff = 99 / (to - from);
    for (int i = from; i < to; ++i) {
      //disableNode(p.path[i], dilat, (i - from) * amountDiff + 1);
      disableNode(p.path[i], dilat, 100);
    }
  }

  void disableRoot(Path p, int spurIndex) {
    for (int i = 0; i < spurIndex - offset; ++i) {
      disableNode(p.path[i], 0, 100);
    }
  }

private:
  void disableNode(int node, int curDilat, double amount) {
    if (curDilat <= 0) {
      for (int i = 0; i < 6; ++i) {
        int index = node + i * rowcols;
        dist[index] = dist[index] * amount;
      }
    } else {
      disableNode(node, 0, amount);
      for (int d = 0; d < 6; ++d) {
        int n = neigh(d, node, rows, cols);
        disableNode(n, curDilat - 1, amount);
      }
    }
  }
};


// [[Rcpp::export(name=".altpaths")]]
List altpaths(int source, int target, const NumericVector dist, int n, int step, int dilat) {
  std::vector<Path> res;
  std::priority_queue<Path> candidates;

  List sh = shortest(source, target, dist);
  if (!sh.length()) { // path not found
    return List::create();
  }
  Path best = Path(sh);
  res.push_back(best);

  // main loop
  for (int i = 0; i < n; ++i) {
    best = res[i];

    for (int spurIndex = 0; spurIndex < best.path.length() - step - 1; spurIndex += step) {
      int spurNode = best.path[spurIndex];
      Distance modDist = Distance(dist, step, dilat);
      for (int j = 0; j < res.size(); ++j) {
        if (best.commonPrefix(res[j], spurIndex)) {
          modDist.disableAfter(res[j], spurIndex);
        }
      }
      modDist.disableRoot(best, spurIndex);
      sh = shortest(spurNode, target, modDist.dist);
      if (sh.length()) { // path found
        Path foundPath = Path(best, spurIndex, sh);
        candidates.push(foundPath);
      }
    }

    if (candidates.empty())
      break;
    Path best = candidates.top();
    candidates.pop();
    res.push_back(best);
  }

  // gather results
  List result = List();
  for (int i = 0; i < res.size(); ++i) {
    result.push_back(List::create(_["prices"] = res[i].prices,
                                  _["path"] = res[i].path));
  }

  return result;
}
