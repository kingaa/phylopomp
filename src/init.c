#include "internal.h"
#include "decls.h"

#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
  {"playSIRwS", (DL_FUNC) &playSIRwS, 10},
  {"get_SIRwS_info", (DL_FUNC) &get_SIRwS_info, 3},
  {"playMoran", (DL_FUNC) &playMoran, 9},
  {"playMoranWChain", (DL_FUNC) &playMoranWChain, 8},
  {"get_Moran_info", (DL_FUNC) &get_Moran_info, 3},
  {"playLBDP", (DL_FUNC) &playLBDP, 9},
  {"get_LBDP_info", (DL_FUNC) &get_LBDP_info, 3},
  {NULL, NULL, 0}
};

void R_init_phylopomp (DllInfo *info) {
  // Register routines
  R_registerRoutines(info,NULL,callMethods,NULL,NULL);
  R_useDynamicSymbols(info,TRUE);
}
