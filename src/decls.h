#ifndef _PHYLOPOMP_DECLS_H_
#define _PHYLOPOMP_DECLS_H_

#ifdef __cplusplus
extern "C" {
#endif
  
  SEXP playSIRwS (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP Iota, SEXP S0, SEXP I0, SEXP Times, SEXP T0, SEXP Tree, SEXP State);
  SEXP get_SIRwS_info (SEXP X, SEXP Prune, SEXP Tree);

  SEXP playMoran (SEXP N, SEXP Mu, SEXP Times, SEXP T0, SEXP Tree, SEXP State);
  SEXP get_Moran_info (SEXP X, SEXP Prune, SEXP Tree);

#ifdef __cplusplus
}
#endif

#endif
