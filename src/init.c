#include "internal.h"
#include "decls.h"

#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
  {"playSIR", (DL_FUNC) &playSIR, 11},
  {"get_SIR_info", (DL_FUNC) &get_SIR_info, 3},
  {NULL, NULL, 0}
};

void R_init_phylopomp (DllInfo *info) {
  // Register routines
  R_registerRoutines(info,NULL,callMethods,NULL,NULL);
  R_useDynamicSymbols(info,TRUE);
}
