#include "hexmatrix.h"


#define TYPEBASED_CALL_UPDOWN(f, rows) {                       \
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


#define TYPEBASED_CALL_LEFTRIGHT(f, rows, cols, odd) {         \
  switch (TYPEOF(v)) {                                         \
    case INTSXP: {                                             \
      return f(as<IntegerVector>(v), rows, cols, odd);         \
    }                                                          \
    case REALSXP: {                                            \
      return f(as<NumericVector>(v), rows, cols, odd);         \
    }                                                          \
    case STRSXP: {                                             \
      return f(as<CharacterVector>(v), rows, cols, odd);       \
    }                                                          \
    case LGLSXP: {                                             \
      return f(as<LogicalVector>(v), rows, cols, odd);         \
    }                                                          \
    case CPLXSXP: {                                            \
      return f(as<ComplexVector>(v), rows, cols, odd);         \
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


template <int RTYPE>
Vector<RTYPE> shiftRightImpl(const Vector<RTYPE> v,
                             const int rows, const int cols, const int odd) {
  Vector<RTYPE> res = clone(v);
  int rowcol = rows * cols;

  for (int i = 0; i < v.length(); i++) {
    if ((i % rows) % 2 == odd) {
        res[i] = v[i];
    } else {
      if (i % rowcol < rows) {
        res[i] = res.get_na();
      } else {
        res[i] = v[i - rows];
      }
    }
  }
  return res;
}


template <int RTYPE>
Vector<RTYPE> shiftLeftImpl(const Vector<RTYPE> v,
                             const int rows, const int cols, const int odd) {
  Vector<RTYPE> res = clone(v);
  int rowcol = rows * cols;

  for (int i = 0; i < v.length(); i++) {
    if ((i % rows) % 2 == odd) {
        res[i] = v[i];
    } else {
      if (i % rowcol >= rowcol - rows) {
        res[i] = res.get_na();
      } else {
        res[i] = v[i + rows];
      }
    }
  }
  return res;
}


// [[Rcpp::export(name=".shiftDown")]]
SEXP shiftDown(SEXP v, const int rows) {
  TYPEBASED_CALL_UPDOWN(shiftDownImpl, rows);
}


// [[Rcpp::export(name=".shiftUp")]]
SEXP shiftUp(SEXP v, const int rows) {
  TYPEBASED_CALL_UPDOWN(shiftUpImpl, rows);
}

// [[Rcpp::export(name=".shiftRight")]]
SEXP shiftRight(SEXP v, const int rows, const int cols, const int odd) {
  TYPEBASED_CALL_LEFTRIGHT(shiftRightImpl, rows, cols, odd);
}
// [[Rcpp::export(name=".shiftLeft")]]
SEXP shiftLeft(SEXP v, const int rows, const int cols, const int odd) {
  TYPEBASED_CALL_LEFTRIGHT(shiftLeftImpl, rows, cols, odd);
}
