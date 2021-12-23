#include "internal.h"
#include <R_ext/Rdynload.h>

SEXP makeSIR (SEXP Params, SEXP ICs, SEXP T0);
SEXP reviveSIR (SEXP State, SEXP Params);
SEXP runSIR (SEXP State, SEXP Times);
SEXP infoSIR (SEXP State, SEXP Prune, SEXP Obscure, SEXP T0, SEXP Time, SEXP Descript,
	      SEXP Yaml, SEXP Structure, SEXP Lineages, SEXP Tree, SEXP Compact);

SEXP makeSIIR (SEXP Params, SEXP ICs, SEXP T0);
SEXP reviveSIIR (SEXP State, SEXP Params);
SEXP runSIIR (SEXP State, SEXP Times);
SEXP infoSIIR (SEXP State, SEXP Prune, SEXP Obscure, SEXP T0, SEXP Time, SEXP Descript,
	       SEXP Yaml, SEXP Structure, SEXP Lineages, SEXP Tree, SEXP Compact);

SEXP makeLBDP (SEXP Params, SEXP ICs, SEXP T0);
SEXP reviveLBDP (SEXP State, SEXP Params);
SEXP runLBDP (SEXP State, SEXP Times);
SEXP infoLBDP (SEXP State, SEXP Prune, SEXP Obscure, SEXP T0, SEXP Time, SEXP Descript,
	       SEXP Yaml, SEXP Structure, SEXP Lineages, SEXP Tree, SEXP Compact);

static const R_CallMethodDef callMethods[] = {
  {"makeSIR", (DL_FUNC) &makeSIR, 3},
  {"reviveSIR", (DL_FUNC) &reviveSIR, 2},
  {"runSIR", (DL_FUNC) &runSIR, 2},
  {"infoSIR", (DL_FUNC) &infoSIR, 11},
  {"makeSIIR", (DL_FUNC) &makeSIIR, 3},
  {"reviveSIIR", (DL_FUNC) &reviveSIIR, 2},
  {"runSIIR", (DL_FUNC) &runSIIR, 2},
  {"infoSIIR", (DL_FUNC) &infoSIIR, 11},
  {"makeLBDP", (DL_FUNC) &makeLBDP, 3},
  {"reviveLBDP", (DL_FUNC) &reviveLBDP, 2},
  {"runLBDP", (DL_FUNC) &runLBDP, 2},
  {"infoLBDP", (DL_FUNC) &infoLBDP, 11},
  {NULL, NULL, 0}
};

void R_init_phylopomp (DllInfo *info) {
  // Register routines
  R_registerRoutines(info,NULL,callMethods,NULL,NULL);
  R_useDynamicSymbols(info,TRUE);
}
