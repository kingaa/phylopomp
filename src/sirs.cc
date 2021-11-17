// SIRS with Sampling Genealogy Process Simulator (C++)

#include "sirs.h"
#include "internal.h"

sirs_tableau_t *makeSIRS (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP Delta,
			  SEXP S0, SEXP I0, SEXP R0, SEXP T0, SEXP State) {
  sirs_tableau_t *gp;
  
  OPTIONAL_REAL_PAR(beta,Beta,0);
  OPTIONAL_REAL_PAR(gamma,Gamma,0);
  OPTIONAL_REAL_PAR(psi,Psi,1);
  OPTIONAL_REAL_PAR(delta,Delta,0);

  if (isNull(State)) {        // a fresh SIR

    double t0 = *(REAL(AS_NUMERIC(T0)));

    OPTIONAL_INT_PAR(s0,S0,0);
    OPTIONAL_INT_PAR(i0,I0,0);
    OPTIONAL_INT_PAR(r0,R0,0);

    gp = new sirs_tableau_t(beta,gamma,psi,delta,s0,i0,r0,t0);

  }  else {              // restart the SIR from the specified state

    gp = new sirs_tableau_t(RAW(State));
    // optionally override the stored parameters
    if (!isNull(Beta)) gp->transmission_rate(beta);
    if (!isNull(Gamma)) gp->recovery_rate(gamma);
    if (!isNull(Psi)) gp->sample_rate(psi);
    if (!isNull(Delta)) gp->waning_rate(delta);
      
  }

  gp->valid();
    
  return gp;
}

extern "C" {

  // Sampled SIR process.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  SEXP playSIRS (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP Delta, SEXP S0, SEXP I0, SEXP R0, SEXP Times, SEXP T0, SEXP Tree, SEXP Ill, SEXP State) {
    SEXP out = R_NilValue;
    GetRNGstate();
    sirs_tableau_t *gp = makeSIRS(Beta,Gamma,Psi,Delta,S0,I0,R0,T0,State);
    PROTECT(out = playGP<sirs_tableau_t>(gp,Times,Tree,Ill));
    PutRNGstate();
    delete gp;
    UNPROTECT(1);
    return out;
  }

}
