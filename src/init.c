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

// edit this file to register new model routines with R
// for each model, there must be
// one DECLARATIONS line and one METHODS line.

DECLARATIONS(SIR)
DECLARATIONS(SIIR)
DECLARATIONS(LBDP)
DECLARATIONS(Moran)
DECLARATIONS(SI2R)
DECLARATIONS(SEIR)
DECLARATIONS(S2I2R2)

static const R_CallMethodDef callMethods[] = {
  METHODS(SIR),
  METHODS(SIIR),
  METHODS(LBDP),
  METHODS(Moran),
  METHODS(SI2R),
  METHODS(SEIR),
  METHODS(S2I2R2),
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
