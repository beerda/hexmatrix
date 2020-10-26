#include "hexmatrix.h"


#define TYPEBASED_CALL(f, rows, cols) {                        \
  switch (TYPEOF(v)) {                                         \
    case INTSXP: {                                             \
      return f(as<IntegerVector>(v), rows);                    \
    }                                                          \
    case REALSXP: {                                            \
      return f(as<NumericVector>(v), rows);                    \
    }                                                          \
    case STRSXP: {                                             \
      return f(as<CharacterVector>(v), rows);                  \
    }                                                          \
    case LGLSXP: {                                             \
      return f(as<LogicalVector>(v), rows);                    \
    }                                                          \
    case CPLXSXP: {                                            \
      return f(as<ComplexVector>(v), rows);                    \
    }                                                          \
    default: {                                                 \
      return NULL;                                             \
    }                                                          \
  }                                                            \
}


template <int RTYPE>
Vector<RTYPE> shiftDownImpl(const Vector<RTYPE> v, const int rows) {
  Vector<RTYPE> res = clone(v);

  for (int i = 0; i < v.length(); i++) {
    if (i % rows == 0) {
      res[i] = res.get_na();
    } else {
      res[i] = v[i-1];
    }
  }
  return res;
}


template <int RTYPE>
Vector<RTYPE> shiftUpImpl(const Vector<RTYPE> v, const int rows) {
  Vector<RTYPE> res = clone(v);

  for (int i = 0; i < v.length(); i++) {
    if (i % rows == rows - 1) {
      res[i] = res.get_na();
    } else {
      res[i] = v[i+1];
    }
  }
  return res;
}


// [[Rcpp::export(name=".shiftDown")]]
SEXP shiftDown(SEXP v, const int rows) {
  TYPEBASED_CALL(shiftDownImpl, rows, NULL)
}


// [[Rcpp::export(name=".shiftUp")]]
SEXP shiftUp(SEXP v, const int rows) {
  TYPEBASED_CALL(shiftUpImpl, rows, NULL)
}
