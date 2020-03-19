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


  bool equalTo(Path other) {
    if (price != other.price) {
      return false;
    }
    if (path.length() != other.path.length()) {
      return false;
    }
    for (int i = 0; i < path.length(); ++i) {
      if (path[i] != other.path[i])
        return false;
    }
    return true;
  }


  bool commonPrefix(Path other, int spurIndex) {
    if (spurIndex >= path.length() || spurIndex >= other.path.length())
      return false;
    for (int i = spurIndex; i >= 0; --i) {
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
  NumericMatrix regions;
  int step;
  int rows;
  int cols;
  int rowcols;

  Distance(const NumericVector aDist, const NumericMatrix aRegions, int aStep) :
    dist(clone(aDist)),
    regions(aRegions),
    step(aStep),
    rows(0),
    cols(0),
    rowcols(0)
  {
    NumericVector dim = aDist.attr("dim");
    rows = dim[0];
    cols = dim[1];
    rowcols = rows * cols;
  }

  void disableAfter(Path p, int spurIndex) {
    int start = step * ((spurIndex + 1) / step + 1);
    for (int i = start; i < p.path.length() && i < start + step; ++i) {
      disableNode(p.path[i]);
    }
  }

  void disableRoot(Path p, int spurIndex) {
    for (int i = 0; i < spurIndex - step; ++i) {
      disableNode(p.path[i]);
    }
  }

private:
  void disableNode(int node) {
    NumericVector reg = region(regions, node);
    for (int j = 0; j < reg.length(); ++j) {
      for (int i = 0; i < 6; ++i) {
        int index = reg[j] + i * rowcols;
        //dist[index] = dist[index] * 100000;
        dist[index] = 1e9;
      }
    }
  }
};


// [[Rcpp::export(name=".altpaths")]]
List altpaths(int source, int target, const NumericVector dist, const NumericMatrix regions, int n, int step, int dilat) {
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
      Distance modDist = Distance(dist, regions, step);
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


    bool pushed = false;
    while (!candidates.empty()) {
      Path best = candidates.top();
      candidates.pop();

      bool equal = false;
      for (int i = 0; i < res.size(); ++i) {
        if (best.equalTo(res[i])) {
          equal = true;
          break;
        }
      }

      if (!equal) {
        res.push_back(best);
        pushed = true;
        break;
      }
    }

    if (!pushed)
      break;
  }

  // gather results
  List result = List();
  for (int i = 0; i < res.size(); ++i) {
    result.push_back(List::create(_["prices"] = res[i].prices,
                                  _["path"] = res[i].path));
  }

  return result;
}
