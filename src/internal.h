#ifndef _PHYLOPOMP_INTERNAL_H_
#define _PHYLOPOMP_INTERNAL_H_

#define R_NO_REMAP

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>

#ifdef __cplusplus
#include <cassert>
#else
#include <assert.h>
#endif

#ifndef STANDALONE

#define err(...) Rf_errorcall(R_NilValue,__VA_ARGS__)
#define warn(...) Rf_warningcall(R_NilValue,__VA_ARGS__)
#define rprint(S) Rprintf("%s\n",(S).c_str())

#else

#include <iostream>
#include <cstdio>
#define err(...) {printf(__VA_ARGS__); printf("\n"); exit(-1);}
#define warn(...) {printf(__VA_ARGS__); printf("\n");}
#define rprint(S) printf("%s\n",(S).c_str())

#endif

#ifdef __cplusplus

#define mkChar Rf_mkChar
#define mkString Rf_mkString
#define ScalarInteger Rf_ScalarInteger
#define ScalarReal Rf_ScalarReal
#define install Rf_install
#define isNull Rf_isNull

#endif

typedef Rbyte raw_t;
typedef double slate_t;
typedef size_t name_t;

// interface with R's integer RNG
static inline int random_integer (int n) {
  return (int) floor(R_unif_index((double) n));
}

// helper function for filling a return list
static inline int set_list_elem
(
 SEXP list, SEXP names, SEXP element,
 const char *name, int pos
 ) {
  SET_ELEMENT(list,pos,element);
  SET_STRING_ELT(names,pos,Rf_mkChar(name));
  return ++pos;
}

static inline int rcateg (double erate, double *rate, int nrate) {
  double u = erate*unif_rand();
  int e = 0;
  while (u > rate[e] && e < nrate) {
    if (rate[e] < 0)
      err("in '%s': invalid rate rate[%d]=%lg",__func__,e,rate[e]); // #nocov
    u -= rate[e++];
  }
  assert(e!=nrate);             // #nocov
  return e;
}

#endif
