#ifndef _GENERICS_H_
#define _GENERICS_H_

#include "internal.h"

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
  UNPROTECT(1);
  return out;
}

//! human/machine readable output (per segment)
template <class TYPE>
SEXP yaml (const TYPE& X) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(X.ngeneal));
  for (name_t s = 0; s < X.ngeneal; s++)
    SET_STRING_ELT(out,s,mkChar(X.yaml(" ", s).c_str()));
  UNPROTECT(1);
  return out;
}

//! human readable output (per segment)
template <class TYPE>
SEXP describe (const TYPE& X) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(X.ngeneal));
  for (name_t s = 0; s < X.ngeneal; s++)
    SET_STRING_ELT(out,s,mkChar(X.describe(s).c_str()));
  UNPROTECT(1);
  return out;
}

//! structure in R list format (per segment)
template <class TYPE>
SEXP structure (const TYPE& G) {
  SEXP out;
  PROTECT(out = NEW_LIST(G.ngeneal));
  for (name_t s = 0; s < G.ngeneal; s++)
    SET_VECTOR_ELT(out, s, G.structure(s));
  UNPROTECT(1);
  return out;
}

//! tree in newick format (per segment, compact)
template <class TYPE>
SEXP newick (const TYPE& X, bool compact = true) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(X.ngeneal));
  for (name_t s = 0; s < X.ngeneal; s++) {
    if (compact)
      SET_STRING_ELT(out,s,mkChar(X.compact_newick_str(s).c_str()));
    else
      SET_STRING_ELT(out,s,mkChar(X.newick(s).c_str()));
  }
  UNPROTECT(1);
  return out;
}

//! number of lineages through time (per segment)
template <class TYPE>
SEXP lineage_count (const TYPE& G) {
  SEXP out;
  PROTECT(out = NEW_LIST(G.ngeneal));
  for (name_t s = 0; s < G.ngeneal; s++)
    SET_VECTOR_ELT(out, s, G.lineage_count(s));
  UNPROTECT(1);
  return out;
}

//! genealogy data in data-frame format (per segment)
template <class TYPE>
SEXP gendat (TYPE& G) {
  SEXP out;
  PROTECT(out = NEW_LIST(G.ngeneal));
  for (name_t s = 0; s < G.ngeneal; s++) {
    G.geneal[s].trace_lineages();
    SET_VECTOR_ELT(out, s, G.gendat(s));
  }
  UNPROTECT(1);
  return out;
}

//! extract the bare genealogies (per segment)
template <class TYPE>
SEXP genealogy (SEXP State) {
  TYPE A = State;
  SEXP out;
  PROTECT(out = NEW_LIST(A.ngeneal));
  for (name_t s = 0; s < A.ngeneal; s++) {
    SEXP seg;
    PROTECT(seg = serial(A.geneal[s]));
    SET_ATTR(seg,install("class"),mkString("gpgen"));
    SET_VECTOR_ELT(out, s, seg);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return out;
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

//! prune and/or obscure and/or hide if requested, then extract info
template <class TYPE>
SEXP info (SEXP State, SEXP Prune, SEXP Obscure, SEXP Hide,
           SEXP T0, SEXP Time, SEXP Descript,
           SEXP Yaml, SEXP Structure, SEXP Lineages,
           SEXP Tree, SEXP Compact, SEXP Gendat) {
  TYPE A = State;

  bool do_prune = *LOGICAL(AS_LOGICAL(Prune));
  bool do_obscure = *LOGICAL(AS_LOGICAL(Obscure));
  bool do_hide = *LOGICAL(AS_LOGICAL(Hide));
  if (do_prune) {
    for (name_t s = 0; s < A.ngeneal; s++) A.geneal[s].prune();
  }
  if (do_obscure) {
    for (name_t s = 0; s < A.ngeneal; s++) A.geneal[s].obscure();
  }
  if (do_hide) {
    for (name_t s = 0; s < A.ngeneal; s++) A.geneal[s].hide();
  }
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

  bool get_gendat = *LOGICAL(AS_LOGICAL(Gendat));
  if (get_gendat) nout++;

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
  if (get_gendat) {
    k = set_list_elem(out,outnames,gendat(A),"gendat",k);
  }
  SET_NAMES(out,outnames);

  UNPROTECT(2);
  return out;
}

#define MAKEFN(X,TYPE) SEXP make ## X (SEXP Params, SEXP IVPs, SEXP T0) { \
  return make<TYPE>(Params,IVPs,T0);                                      \
}                                                                         \

#define REVIVEFN(X,TYPE) SEXP revive ## X (SEXP State, SEXP Params) {   \
  return revive<TYPE>(State,Params);                                    \
}                                                                       \

#define RUNFN(X,TYPE) SEXP run ## X (SEXP State, SEXP Times) {  \
  return run<TYPE>(State,Times);                                \
}                                                               \

#define GENEALFN(X,TYPE) SEXP geneal ## X (SEXP State) {        \
  return genealogy<TYPE>(State);                               \
}                                                              \

#define INFOFN(X,TYPE) SEXP info ## X (                                 \
  SEXP State, SEXP Prune, SEXP Obscure, SEXP Hide,                     \
  SEXP T0, SEXP Time, SEXP Descript,                                    \
  SEXP Yaml, SEXP Structure, SEXP Lineages,                             \
  SEXP Tree, SEXP Compact, SEXP Gendat) {                               \
  return info<TYPE>(State, Prune, Obscure, Hide,                        \
                    T0, Time, Descript,                                  \
                    Yaml, Structure, Lineages,                           \
                    Tree, Compact, Gendat);                              \
}                                                                       \

#define GENERICS(X,TYPE)                        \
  extern "C" {                                  \
                                                \
    MAKEFN(X,TYPE)                              \
                                                \
    REVIVEFN(X,TYPE)                            \
                                                \
    RUNFN(X,TYPE)                               \
                                                \
    GENEALFN(X,TYPE)                            \
                                                \
    INFOFN(X,TYPE)                              \
                                                \
  }                                             \

#endif
