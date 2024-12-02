// Bare genealogy

#include "genealogy.h"
#include "generics.h"
#include "internal.h"

extern "C" {

  //! curtail the given genealogy
  SEXP curtail (SEXP State, SEXP Time, SEXP Troot) {
    genealogy_t A = State;
    double t, t0;
    t = *REAL(AS_NUMERIC(Time));
    t0 = *REAL(AS_NUMERIC(Troot));
    if (ISNA(t)) t = A.time();
    if (ISNA(t0)) t0 = A.timezero();
    A.curtail(t,t0);
    SEXP out;
    PROTECT(out = serial(A));
    SET_ATTR(out,install("class"),mkString("gpgen"));
    UNPROTECT(1);
    return out;
  }

  //! extract a YAML description
  SEXP yaml (SEXP State) {
    genealogy_t A = State;
    return mkString(A.yaml().c_str());
  }

  //! data-frame format
  SEXP gendat (SEXP State) {
    genealogy_t A = State;
    A.prune();
    A.obscure();
    A.trace_lineages();
    return A.gendat();
  }

}
