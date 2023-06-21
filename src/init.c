#include "init.h"
#include "decls.h"

SEXP parse_newick (SEXP, SEXP, SEXP);
SEXP infoBare (SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP curtailBare (SEXP, SEXP);

// edit this file to register new model routines with R
// for each model, there must be
// one DECLARATIONS line and one METHODS line.

DECLARATIONS(SIR)
DECLARATIONS(SIIR)
DECLARATIONS(LBDP)
DECLARATIONS(Moran)
DECLARATIONS(SI2R)  
DECLARATIONS(SEIR)
  
static const R_CallMethodDef callMethods[] = {
  METHODS(SIR),
  METHODS(SIIR),
  METHODS(LBDP),
  METHODS(Moran),
  METHODS(SI2R),
  METHODS(SEIR),
  {"parse_newick", (DL_FUNC) &parse_newick, 3},
  {"infoBare", (DL_FUNC) &infoBare, 14},
  {"curtailBare", (DL_FUNC) &curtailBare, 2},
  {NULL, NULL, 0}
};

void R_init_phylopomp (DllInfo *info) {
  // Register routines
  R_registerRoutines(info,NULL,callMethods,NULL,NULL);
  R_useDynamicSymbols(info,TRUE);
  //  R_useDynamicSymbols(info,FALSE);
  //  R_forceSymbols(info,TRUE);
}
