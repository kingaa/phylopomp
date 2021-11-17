// Moran Genealogy Process with Sampling Simulator (C++)

#include "moran.h"
#include "internal.h"

moran_tableau_t *makeGP (SEXP N, SEXP Mu, SEXP T0, SEXP Stat, SEXP State) {
  moran_tableau_t *gp;
  OPTIONAL_INT_PAR(n,N,100);
  OPTIONAL_REAL_PAR(mu,Mu,1);
  if (isNull(State)) {		// a fresh GP
    double t0 = *REAL(AS_NUMERIC(T0));
    int stat = *INTEGER(AS_INTEGER(Stat));
    GetRNGstate();
    gp = new moran_tableau_t(n,mu,t0,stat);
    PutRNGstate();
  }  else {		    // restart the GP from the specified state
    gp = new moran_tableau_t(RAW(State));
    // optionally override the stored parameters
    if (!isNull(N)) gp->popsize(n);
    if (!isNull(Mu)) gp->moran_rate(mu);
  }
  return gp;
}

extern "C" {

  // (Sampled or unsampled) Moran genealogy process.
  // If 'sample = TRUE', one sample is taken at each timepoint.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  // optionally return state in diagrammable form ('ill = TRUE').
  SEXP playMoran (SEXP N, SEXP Mu, SEXP Times, SEXP T0, SEXP Sample, SEXP Tree, SEXP Ill, SEXP Stat, SEXP State) {
    moran_tableau_t *gp;
    SEXP out = R_NilValue;
    GetRNGstate();
    gp = makeGP(N,Mu,T0,Stat,State);
    if (*INTEGER(AS_INTEGER(Sample))) {
      PROTECT(out = playSGP<moran_tableau_t>(gp,Times,Tree,Ill));
    } else {
      PROTECT(out = playGP<moran_tableau_t>(gp,Times,Tree,Ill));
    }
    PutRNGstate();
    delete gp;
    UNPROTECT(1);
    return out;
  }

  // Play unsampled Moran W chain
  // optionally compute genealogies in Newick form ('tree = TRUE').
  // optionally return state in diagrammable form ('ill = TRUE').
  SEXP playMoranWChain (SEXP N, SEXP Mu, SEXP Ntimes, SEXP T0, SEXP Tree, SEXP Ill, SEXP Stat, SEXP State) {
    moran_tableau_t *gp;
    SEXP out = R_NilValue;
    GetRNGstate();
    gp = makeGP(N,Mu,T0,Stat,State);
    PROTECT(out = playWChain<moran_tableau_t>(gp,Ntimes,Tree,Ill));
    PutRNGstate();
    delete gp;
    UNPROTECT(1);
    return out;
  }

}
