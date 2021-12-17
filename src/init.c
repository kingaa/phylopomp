#include "internal.h"
#include "decls.h"

#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
  {"makeSIR", (DL_FUNC) &makeSIR, 3},
  {"reviveSIR", (DL_FUNC) &reviveSIR, 2},
  {"runSIR", (DL_FUNC) &runSIR, 2},
  {"treeSIR", (DL_FUNC) &treeSIR, 3},
  {"structSIR", (DL_FUNC) &structSIR, 2},
  {"infoSIR", (DL_FUNC) &infoSIR, 3},
  {"makeSIIR", (DL_FUNC) &makeSIIR, 3},
  {"reviveSIIR", (DL_FUNC) &reviveSIIR, 2},
  {"runSIIR", (DL_FUNC) &runSIIR, 2},
  {"treeSIIR", (DL_FUNC) &treeSIIR, 3},
  {"structSIIR", (DL_FUNC) &structSIIR, 2},
  {"infoSIIR", (DL_FUNC) &infoSIIR, 3},
  {NULL, NULL, 0}
};

void R_init_phylopomp (DllInfo *info) {
  // Register routines
  R_registerRoutines(info,NULL,callMethods,NULL,NULL);
  R_useDynamicSymbols(info,TRUE);
}
