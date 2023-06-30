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
  SET_STRING_ELT(names,pos,mkChar(name));
  return ++pos;
}

static inline int rcateg (double erate, double *rate, int nrate) {
  double u = erate*unif_rand();
  int e = 0;
  while (u > rate[e] && e < nrate) {
    if (rate[e] < 0)
      err("in '%s': invalid rate rate[%ld]=%lg",__func__,e,rate[e]); // #nocov
    u -= rate[e++];
  }
  if (e == nrate)
    err("inconceivable! in '%s'",__func__); // #nocov
  return e;
}

#endif
