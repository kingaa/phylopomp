#ifndef _GENERICS_H_
#define _GENERICS_H_

#include "internal.h"

template <class TYPE>
SEXP ndeme (TYPE& X) {
  return ScalarInteger(X.ndeme());
}

template <class TYPE>
SEXP nsample (TYPE& X) {
  return ScalarInteger(X.nsample());
}

template <class TYPE>
SEXP timezero (TYPE& X) {
  return ScalarReal(X.timezero());
}

template <class TYPE>
SEXP time (TYPE& X) {
  return ScalarReal(X.time());
}

//! binary serialization
template <class TYPE>
SEXP serial (const TYPE& X) {
  SEXP out;
  PROTECT(out = NEW_RAW(X.bytesize()));
  X >> RAW(out);
  setAttrib(out,install("class"),mkString("gpsim"));
  UNPROTECT(1);
  return out;
}

//! human/machine readable output
template <class TYPE>
SEXP yaml (const TYPE& X) {
  return mkString(X.yaml().c_str());
}

//! human readable output
template <class TYPE>
SEXP describe (const TYPE& X) {
  return mkString(X.describe().c_str());
}

//! structure in R list format
template <class TYPE>
SEXP structure (const TYPE& X) {
  return X.structure();
}

//! tree in newick format
template <class TYPE>
SEXP newick (const TYPE& X) {
  return mkString(X.newick().c_str());
}

//! number of lineages through time
template <class TYPE>
SEXP lineage_count (const TYPE& G) {
  return G.lineage_count();
}

//! initialization
template<class TYPE>
SEXP make (SEXP Params, SEXP IVPs, SEXP T0) {
  SEXP o;
  PROTECT(Params = AS_NUMERIC(Params));
  PROTECT(IVPs = AS_NUMERIC(IVPs));
  PROTECT(T0 = AS_NUMERIC(T0));
  GetRNGstate();
  TYPE X = *REAL(T0);
  X.update_params(REAL(Params),LENGTH(Params));
  X.update_IVPs(REAL(IVPs),LENGTH(IVPs));
  X.rinit();
  X.update_clocks();
  PutRNGstate();
  PROTECT(o = serial(X));
  UNPROTECT(4);
  return o;
}

//! refresh parameters
template<class TYPE>
SEXP revive (SEXP State, SEXP Params) {
  SEXP o;
  TYPE X = State;
  PROTECT(Params = AS_NUMERIC(Params));
  X.update_params(REAL(Params),LENGTH(Params));
  PROTECT(o = serial(X));
  UNPROTECT(2);
  return o;
}

//! run simulations
template<class TYPE>
SEXP run (SEXP State, SEXP Tout) {
  SEXP out;
  TYPE X = State;
  PROTECT(Tout = AS_NUMERIC(Tout));
  GetRNGstate();
  X.valid();
  X.play(*REAL(Tout));
  PutRNGstate();
  PROTECT(out = serial(X));
  UNPROTECT(2);
  return out;
}

//! curtail the given genealogy
template <class TYPE>
SEXP curtail (SEXP State, SEXP Time) {
  TYPE A = State;
  A.curtail(*REAL(AS_NUMERIC(Time)));
  return serial(A);
}

//! extract the bare genealogy
template <class TYPE>
SEXP genealogy (SEXP State) {
  TYPE A = State;
  return serial(A.geneal);
}

#define MAKEFN(X,TYPE) SEXP make ## X (SEXP Params, SEXP IVPs, SEXP T0) { \
    return make<TYPE>(Params,IVPs,T0);                                  \
  }                                                                     \
  
#define REVIVEFN(X,TYPE) SEXP revive ## X (SEXP State, SEXP Params) {   \
    return revive<TYPE>(State,Params);                                  \
  }                                                                     \

#define RUNFN(X,TYPE) SEXP run ## X (SEXP State, SEXP Times) {  \
    return run<TYPE>(State,Times);                              \
  }                                                             \

#define GENEALFN(X,TYPE) SEXP geneal ## X (SEXP State) {		\
    return genealogy<TYPE>(State);					\
  }                                                                     \

#define CURTAILFN(X,TYPE) SEXP curtail ## X (SEXP State, SEXP Time) {   \
    return curtail<TYPE>(State,Time);                                   \
  }                                                                     \

#define GENERICS(X,TYPE)                        \
  extern "C" {                                  \
                                                \
    MAKEFN(X,TYPE)                              \
                                                \
    REVIVEFN(X,TYPE)                            \
                                                \
    RUNFN(X,TYPE)                               \
                                                \
    GENEALFN(X,TYPE)				\
                                                \
    CURTAILFN(X,TYPE)                           \
                                                \
  }						\
  
#endif
