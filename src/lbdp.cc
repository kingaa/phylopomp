// Linear birth-death Genealogy Process with Sampling Simulator (C++)

#include "lbdp.h"
#include "internal.h"

lbdp_tableau_t *makeLBDP (SEXP Lambda, SEXP Mu, SEXP Psi, SEXP N0, SEXP T0, SEXP State) {
  lbdp_tableau_t *gp;

  OPTIONAL_REAL_PAR(lambda,Lambda,1);
  OPTIONAL_REAL_PAR(mu,Mu,1);
  OPTIONAL_REAL_PAR(psi,Psi,1);

  if (isNull(State)) {        // a fresh GP

    double t0 = *REAL(AS_NUMERIC(T0));
    
    OPTIONAL_INT_PAR(n0,N0,1);

    gp = new lbdp_tableau_t(lambda,mu,psi,n0,t0);

  }  else {              // restart the GP from the specified state

    gp = new lbdp_tableau_t(RAW(State));
    // optionally override the stored parameters
    if (!isNull(Lambda)) gp->birth_rate(lambda);
    if (!isNull(Mu)) gp->death_rate(mu);
    if (!isNull(Psi)) gp->sampling_rate(psi);

  }

  gp->valid();
    
  return gp;
}

extern "C" {

  // BD process
  // optionally compute genealogies in Newick form ('tree = TRUE').
  SEXP playLBDP (SEXP Lambda, SEXP Mu, SEXP Psi, SEXP N0, SEXP Times, SEXP T0, SEXP Tree, SEXP Ill, SEXP State) {
    SEXP out = R_NilValue;
    GetRNGstate();
    lbdp_tableau_t *gp = makeLBDP(Lambda,Mu,Psi,N0,T0,State);
    PROTECT(out = playGP<lbdp_tableau_t>(gp,Times,Tree,Ill));
    PutRNGstate();
    delete gp;
    UNPROTECT(1);
    return out;
  }

}
