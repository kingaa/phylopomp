#include "init.h"
#include "decls.h"
#include "pomplink.h"

get_userdata_t *get_userdata;
get_userdata_double_t *get_userdata_double;
get_userdata_int_t *get_userdata_int;

SEXP parse_newick (SEXP, SEXP, SEXP);
SEXP getInfo (SEXP);
SEXP curtail (SEXP, SEXP);
SEXP yaml (SEXP);
SEXP gendat (SEXP);

// for each model, there must be
// one DECLARATIONS line and one METHODS line.

DECLARATIONS(LBDP)
DECLARATIONS(Moran)
DECLARATIONS(S2I2R2)
DECLARATIONS(SEIR)
DECLARATIONS(SI2R)
DECLARATIONS(SIIR)
DECLARATIONS(SIR)
DECLARATIONS(TIMVA)
DECLARATIONS(TwoSpecies)

static const R_CallMethodDef callMethods[] = {
  METHODS(LBDP),
  METHODS(Moran),
  METHODS(S2I2R2),
  METHODS(SEIR),
  METHODS(SI2R),
  METHODS(SIIR),
  METHODS(SIR),
  METHODS(TIMVA),
  METHODS(TwoSpecies),
  {"parse_newick", (DL_FUNC) &parse_newick, 3},
  {"curtail", (DL_FUNC) &curtail, 2},
  {"yaml", (DL_FUNC) &yaml, 1},
  {"gendat", (DL_FUNC) &gendat, 1},
  {NULL, NULL, 0}
};

static const R_CallMethodDef extMethods[] = {
  {"getInfo", (DL_FUNC) &getInfo, -1},
  {NULL, NULL, 0}
};

void R_init_phylopomp (DllInfo *info) {
  // Register routines
  R_registerRoutines(info,NULL,callMethods,NULL,extMethods);
  R_useDynamicSymbols(info,TRUE);
  //  R_useDynamicSymbols(info,FALSE);
  //  R_forceSymbols(info,TRUE);
  get_userdata = (get_userdata_t*) R_GetCCallable("pomp","get_userdata");
  get_userdata_double = (get_userdata_double_t*) R_GetCCallable("pomp","get_userdata_double");
  get_userdata_int = (get_userdata_int_t*) R_GetCCallable("pomp","get_userdata_int");
}
