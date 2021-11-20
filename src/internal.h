#ifndef _PHYLOPOMP_INTERNAL_H_
#define _PHYLOPOMP_INTERNAL_H_

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define err(...) errorcall(R_NilValue,__VA_ARGS__)
#define warn(...) warningcall(R_NilValue,__VA_ARGS__)

inline SEXP trueSEXP (void) {
  SEXP x;
  PROTECT(x = NEW_LOGICAL(1));
  *LOGICAL(x) = 1;
  UNPROTECT(1);
  return x;
}

inline SEXP falseSEXP (void) {
  SEXP x;
  PROTECT(x = NEW_LOGICAL(1));
  *LOGICAL(x) = 0;
  UNPROTECT(1);
  return x;
}

#define OPTIONAL_REAL_PAR(x,X,d) double x;	\
  if (isNull(X)) {				\
    x = d;					\
  } else {					\
    x = *(REAL(AS_NUMERIC(X)));			\
  }						\
  
#define OPTIONAL_INT_PAR(x,X,d) int x;		\
  if (isNull(X)) {				\
    x = d;					\
  } else {					\
    x = *(INTEGER(AS_INTEGER(X)));		\
  }						\
  
#endif
