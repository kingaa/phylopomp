#ifndef _PHYLOPOMP_INTERNAL_H_
#define _PHYLOPOMP_INTERNAL_H_

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#ifndef STANDALONE

#define err(...) errorcall(R_NilValue,__VA_ARGS__)
#define warn(...) warningcall(R_NilValue,__VA_ARGS__)
#define rprint(S) Rprintf("%s\n",(S).c_str())

#else

#include <iostream>
#include <cstdio>
#define err(...) {printf(__VA_ARGS__); printf("\n"); exit(-1);}
#define warn(...) {printf(__VA_ARGS__); printf("\n");}
#define rprint(S) printf("%s\n",(S).c_str())

#endif

typedef Rbyte raw_t; // must match with R's 'Rbyte' (see Rinternals.h)
typedef double slate_t;
typedef size_t name_t;

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

// interface with R's integer RNG
inline int random_integer (int n) {
  return (int) floor(R_unif_index((double) n));
}

// helper function for filling a return list
inline int set_list_elem (SEXP list, SEXP names, SEXP element,
                          const char *name, int pos) {
  SET_ELEMENT(list,pos,element);
  SET_STRING_ELT(names,pos,mkChar(name));
  return ++pos;
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
