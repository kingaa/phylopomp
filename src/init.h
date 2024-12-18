#ifndef _INIT_H_
#define _INIT_H_

#include <R_ext/Rdynload.h>
#include "internal.h"

#define DECLARATIONS(X)                                 \
  SEXP make ## X (SEXP Params, SEXP IVPs, SEXP T0);     \
  SEXP revive ## X (SEXP State, SEXP Params);           \
  SEXP run ## X (SEXP State, SEXP Times);               \
  SEXP geneal ## X (SEXP State);                        \
  SEXP yaml ## X (SEXP State)

#define METHODS(X)                              \
  {"make" #X, (DL_FUNC) &make ## X, 3},         \
  {"revive" #X, (DL_FUNC) &revive ## X, 2},     \
  {"run" #X, (DL_FUNC) &run ## X, 2},           \
  {"yaml" #X, (DL_FUNC) &yaml ## X, 1},         \
  {"geneal" #X, (DL_FUNC) &geneal ## X, 1}      \

#endif
