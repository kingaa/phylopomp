#include "init.h"

DECLARATIONS(LBDPwr2);
DECLARATIONS(SEI2Rwr);

// bare genealogy functions
SEXP curtail (SEXP State, SEXP Time, SEXP Troot);
SEXP yaml (SEXP State);
SEXP bare_gendat (SEXP State, SEXP Obscure);
SEXP parse_newick (SEXP X, SEXP T0, SEXP Tf);

static const R_CallMethodDef callMethods[] = {
  METHODS(LBDPwr2),
  METHODS(SEI2Rwr),
  {"curtail", (DL_FUNC) &curtail, 3},
  {"yaml", (DL_FUNC) &yaml, 1},
  {"bare_gendat", (DL_FUNC) &bare_gendat, 2},
  {"parse_newick", (DL_FUNC) &parse_newick, 3},
  {NULL, NULL, 0}
};

void R_init_phylopompRe (DllInfo *info) {
  R_registerRoutines(info,NULL,callMethods,NULL,NULL);
  R_useDynamicSymbols(info,FALSE);
}
