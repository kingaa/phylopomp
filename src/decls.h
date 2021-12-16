#ifndef _PHYLOPOMP_DECLS_H_
#define _PHYLOPOMP_DECLS_H_

#ifdef __cplusplus
extern "C" {
#endif

  SEXP playSIR (SEXP Beta, SEXP Gamma, SEXP Psi,
		SEXP S0, SEXP I0, SEXP R0,
		SEXP Times, SEXP T0, SEXP Tree,
		SEXP Compact, SEXP State);
  SEXP get_SIR_info (SEXP X, SEXP Prune, SEXP Compact);

  SEXP playSIIR (SEXP Beta1, SEXP Beta2, SEXP Gamma, SEXP Psi,
		 SEXP S0, SEXP I1_0, SEXP I2_0,
		 SEXP R0, SEXP Times, SEXP T0, SEXP Tree,
		 SEXP Compact, SEXP State);
  SEXP get_SIIR_info (SEXP X, SEXP Prune, SEXP Compact);
  
#ifdef __cplusplus
}
#endif

#endif
