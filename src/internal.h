#ifndef _PHYLOPOMP_INTERNAL_H_
#define _PHYLOPOMP_INTERNAL_H_

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

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

// whether an element in an array
inline bool anyof (name_t* arr, size_t len, name_t elem) {
  for (name_t k = 0; k < len; k++) {
    if (elem == arr[k]) return true;
  }
  return false;
}

// sample random numbers; Fisher-Yates algorithm
inline void swap_elem (name_t *a, name_t *b) {
  name_t temp = *a;
  *a = *b;
  *b = temp;
}

inline void random_numbers (name_t* out, size_t len, size_t n) {
  if (n > len)  err("in '%s': exceeding size!",__func__);
  name_t i;
  name_t* arr = R_Calloc(len,name_t);
  for (i = 0; i < len; i++) arr[i] = i;
  srand(time(NULL));
  for (i = len - 1; i > 0; i--)  {
    // Pick a random index from 0 to i
    name_t j = rand() % (i+1);
    // Swap arr[i] with the element at random index
    swap_elem(&arr[i], &arr[j]);
  }
  for (i = 0; i < n; i++) {
    out[i] = arr[i];
  }
  R_Free(arr);
}

// compare integers
inline int compare_int (const void* a, const void* b) {
  return (*(int*)a - *(int*)b);
}

// helper function for filling a return list
inline int set_list_elem (SEXP list, SEXP names, SEXP element,
                          const char *name, int pos) {
  SET_ELEMENT(list,pos,element);
  SET_STRING_ELT(names,pos,mkChar(name));
  return ++pos;
}

#endif
