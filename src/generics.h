#ifndef _GENERICS_H_
#define _GENERICS_H_

#include "internal.h"

template <class TYPE>
SEXP timezero (TYPE& X) {
  SEXP o = NEW_NUMERIC(1);
  *REAL(o) = X.timezero();
  return o;
}

template <class TYPE>
SEXP time (TYPE& X) {
  SEXP o = NEW_NUMERIC(1);
  *REAL(o) = X.time();
  return o;
}

// binary serialization
template <class TYPE>
SEXP serial (const TYPE& X) {
  SEXP out;
  PROTECT(out = NEW_RAW(X.bytesize()));
  X >> RAW(out);
  UNPROTECT(1);
  return out;
}

// human/machine readable output
template <class TYPE>
SEXP yaml (const TYPE& X) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(1));
  SET_STRING_ELT(out,0,mkChar(X.yaml().c_str()));
  UNPROTECT(1);
  return out;
}

// human readable output
template <class TYPE>
SEXP describe (const TYPE& X) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(1));
  SET_STRING_ELT(out,0,mkChar(X.describe().c_str()));
  UNPROTECT(1);
  return out;
}

// structure in R list format
template <class TYPE>
SEXP structure (const TYPE& G) {
  return G.structure();
}

// structure in R list format
template <class TYPE>
SEXP newick (const TYPE& X, bool compact = true) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(1));
  SET_STRING_ELT(out,0,mkChar(X.newick(compact).c_str()));
  UNPROTECT(1);
  return out;
}

// initialization
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

// refresh parameters
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

// run simulations
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

template <class TYPE>
SEXP lineage_count (const TYPE& G) {
  return G.lineage_count();
}

// extract information
template <class TYPE>
SEXP info (SEXP State, SEXP Prune, SEXP Obscure,
	   SEXP T0, SEXP Time, SEXP Descript,
	   SEXP Yaml, SEXP Structure, SEXP Lineages,
	   SEXP Tree, SEXP Compact) {
  TYPE A(RAW(State));

  // prune if requested
  bool do_prune = *LOGICAL(AS_LOGICAL(Prune));
  bool do_obscure = *LOGICAL(AS_LOGICAL(Obscure));
  if (do_prune) A.geneal.prune();
  if (do_obscure) A.geneal.obscure();

  size_t nout = 0;

  bool get_t0 = *LOGICAL(AS_LOGICAL(T0));
  if (get_t0) nout++;
  
  bool get_time = *LOGICAL(AS_LOGICAL(Time));
  if (get_time) nout++;

  bool get_desc = *LOGICAL(AS_LOGICAL(Descript));
  if (get_desc) nout++;

  bool get_yaml = *LOGICAL(AS_LOGICAL(Yaml));
  if (get_yaml) nout++;

  bool get_struc = *LOGICAL(AS_LOGICAL(Structure));
  if (get_struc) nout++;

  bool get_lin = *LOGICAL(AS_LOGICAL(Lineages));
  if (get_lin) nout++;

  bool get_tree = *LOGICAL(AS_LOGICAL(Tree));
  if (get_tree) nout++;
  bool do_compact = *LOGICAL(AS_LOGICAL(Compact));

  // pack up return values in a list
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout));
  PROTECT(outnames = NEW_CHARACTER(nout));
  if (get_t0) {
    k = set_list_elem(out,outnames,timezero(A),"t0",k);
  }
  if (get_time) {
    k = set_list_elem(out,outnames,time(A),"time",k);
  }
  if (get_desc) {
    k = set_list_elem(out,outnames,describe(A),"description",k);
  }
  if (get_yaml) {
    k = set_list_elem(out,outnames,yaml(A),"yaml",k);
  }
  if (get_struc) {
    k = set_list_elem(out,outnames,structure(A),"structure",k);
  }
  if (get_lin) {
    k = set_list_elem(out,outnames,lineage_count(A),"lineages",k);
  }
  if (get_tree) {
    k = set_list_elem(out,outnames,newick(A,do_compact),"tree",k);
  }
  SET_NAMES(out,outnames);

  UNPROTECT(2);
  return out;
}

#define MAKEFN(X,TYPE) SEXP make ## X (SEXP Params, SEXP IVPs, SEXP T0) { \
    return make<TYPE>(Params,IVPs,T0);					\
  }									\
  
#define REVIVEFN(X,TYPE) SEXP revive ## X (SEXP State, SEXP Params) {	\
    return revive<TYPE>(State,Params);					\
  }									\

#define RUNFN(X,TYPE) SEXP run ## X (SEXP State, SEXP Times) {	\
    return run<TYPE>(State,Times);				\
  }								\

#define INFOFN(X,TYPE) SEXP info ## X (					\
				       SEXP State, SEXP Prune, SEXP Obscure, \
				       SEXP T0, SEXP Time, SEXP Descript, \
				       SEXP Yaml, SEXP Structure, SEXP Lineages, \
				       SEXP Tree, SEXP Compact) {	\
    return info<TYPE>(State, Prune, Obscure,				\
		      T0, Time, Descript,				\
		      Yaml,Structure, Lineages,				\
		      Tree, Compact);					\
  }									\

#define GENERICS(X,TYPE)						\
  extern "C" {								\
									\
    MAKEFN(X,TYPE)							\
    									\
    REVIVEFN(X,TYPE)							\
									\
    RUNFN(X,TYPE)							\
    									\
    INFOFN(X,TYPE)							\
									\
  }									\
  
#endif
