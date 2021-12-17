#ifndef _PHYLOPOMP_DECLS_H_
#define _PHYLOPOMP_DECLS_H_

#ifdef __cplusplus
extern "C" {
#endif

  SEXP makeSIR (SEXP Params, SEXP ICs, SEXP T0);
  SEXP reviveSIR (SEXP State, SEXP Params);
  SEXP runSIR (SEXP State, SEXP Times);
  SEXP treeSIR (SEXP State, SEXP Prune, SEXP Compact);
  SEXP structSIR (SEXP State, SEXP Prune);
  SEXP infoSIR (SEXP State, SEXP Prune, SEXP Compact);

  SEXP makeSIIR (SEXP Params, SEXP ICs, SEXP T0);
  SEXP reviveSIIR (SEXP State, SEXP Params);
  SEXP runSIIR (SEXP State, SEXP Times);
  SEXP treeSIIR (SEXP State, SEXP Prune, SEXP Compact);
  SEXP structSIIR (SEXP State, SEXP Prune);
  SEXP infoSIIR (SEXP State, SEXP Prune, SEXP Compact);

#ifdef __cplusplus
}
#endif

#endif
