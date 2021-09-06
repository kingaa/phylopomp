// multisir with Sampling Genealogy Process Simulator (C++)
#include "multisirws_breto.h"
#include "internal.h"

multisirws_tableau_t *makemultiSIRwS (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP Theta, SEXP S0, SEXP I0, SEXP R0, SEXP T0, SEXP State) {
  multisirws_tableau_t *gp;
  
  double beta = R_NaReal;     // transmission rate
  if (!isNull(Beta)) {
    beta = *(REAL(AS_NUMERIC(Beta)));
  }

  double gamma = R_NaReal;    // recovery rate
  if (!isNull(Gamma)) {
    gamma = *(REAL(AS_NUMERIC(Gamma)));
  }

  double psi = R_NaReal;      // sampling rate
  if (!isNull(Psi)) { 
    psi = *(REAL(AS_NUMERIC(Psi)));
  }

  double theta = R_NaReal;       // dispersion param
  if (!isNull(Theta)) {
    theta = *(REAL(AS_NUMERIC(Theta)));
  }

  if (isNull(State)) {        // a fresh multisir

    double t0 = *(REAL(AS_NUMERIC(T0)));

    int s0 = na;                // initial susceptible pool
    if (!isNull(S0)) {
      s0 = *(INTEGER(AS_INTEGER(S0)));
    }
    
    int i0 = na;                // initial number of infections
    if (!isNull(I0)) {
      i0 = *(INTEGER(AS_INTEGER(I0)));
    }

    int r0 = na;                // initial number of recoveries
    if (!isNull(R0)) {
      r0 = *(INTEGER(AS_INTEGER(R0)));
    }

    gp = new multisirws_tableau_t(beta,gamma,psi,theta,s0,i0,r0,t0);

  }  else {              // restart the multisir from the specified state

    gp = new multisirws_tableau_t(RAW(State));
    // optionally override the stored parameters
    if (!isNull(Beta)) gp->transmission_rate(beta);
    if (!isNull(Gamma)) gp->recovery_rate(gamma);
    if (!isNull(Psi)) gp->sample_rate(psi);
    if (!isNull(Theta)) gp->dispersion_param(theta);
      
  }

  gp->valid();
    
  return gp;
}

extern "C" {

  // Sampled overdispersed SIR process.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  SEXP playmultiSIRwS (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP Theta, SEXP S0, SEXP I0, SEXP R0, SEXP Times, SEXP T0, SEXP Tree, SEXP Ill, SEXP State) {
    SEXP out = R_NilValue;
    GetRNGstate();
    multisirws_tableau_t *gp = makemultiSIRwS(Beta,Gamma,Psi,Theta,S0,I0,R0,T0,State);
    PROTECT(out = playGP<multisirws_tableau_t>(gp,Times,Tree,Ill));
    PutRNGstate();
    delete gp;
    UNPROTECT(1);
    return out;
  }

}
