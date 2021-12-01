#ifndef _PHYLOPOMP_DECLS_H_
#define _PHYLOPOMP_DECLS_H_

#ifdef __cplusplus
extern "C" {
#endif

  SEXP playMoran (SEXP N, SEXP Mu, SEXP Times, SEXP T0, SEXP Sample, SEXP Tree, SEXP Ill, SEXP Stat, SEXP State);
  SEXP playMoranWChain (SEXP N, SEXP Mu, SEXP Ntimes, SEXP T0, SEXP Tree, SEXP Ill, SEXP Stat, SEXP State);
  SEXP get_Moran_info (SEXP X, SEXP Prune, SEXP Compact);

  SEXP playLBDP (SEXP Lambda, SEXP Mu, SEXP Psi, SEXP N0, SEXP Times, SEXP T0, SEXP Tree, SEXP Ill, SEXP State);
  SEXP get_LBDP_info (SEXP X, SEXP Prune, SEXP Compact);

  SEXP playSIRwS (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP S0, SEXP I0, SEXP R0, SEXP Times, SEXP T0, SEXP Tree, SEXP Ill, SEXP State);
  SEXP get_SIRwS_info (SEXP X, SEXP Prune, SEXP Compact);

  SEXP playmultiSIRwS (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP Theta, SEXP S0, SEXP I0, SEXP R0, SEXP Times, SEXP T0, SEXP Tree, SEXP Ill, SEXP State);
  SEXP get_multiSIRwS_info (SEXP X, SEXP Prune, SEXP Compact);
  
  void multisir_rinit (double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars);
  void multisir_dmeas (double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t);
  void birthrates (int indmax, double size, double disp, double *arr);
  void multisir_gill (double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double dt);

#ifdef __cplusplus
}
#endif

#endif
