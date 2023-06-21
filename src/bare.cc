// Bare genealogy

#include "genealogy.h"
#include "internal.h"

SEXP ndeme (genealogy_t & X) {
  SEXP o = NEW_INTEGER(1);
  *INTEGER(o) = int(X.ndeme());
  return o;
}

SEXP nsample (genealogy_t& X) {
  SEXP o = NEW_INTEGER(1);
  *INTEGER(o) = int(X.nsample());
  return o;
}

SEXP timezero (genealogy_t& X) {
  SEXP o = NEW_NUMERIC(1);
  *REAL(o) = X.timezero();
  return o;
}

SEXP time (genealogy_t& X) {
  SEXP o = NEW_NUMERIC(1);
  *REAL(o) = X.time();
  return o;
}

//! binary serialization
SEXP serial (const genealogy_t& X) {
  SEXP out;
  PROTECT(out = NEW_RAW(X.bytesize()));
  X >> RAW(out);
  UNPROTECT(1);
  return out;
}

//! human/machine readable output
SEXP yaml (const genealogy_t& X) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(1));
  SET_STRING_ELT(out,0,mkChar(X.yaml().c_str()));
  UNPROTECT(1);
  return out;
}

//! human readable output
SEXP describe (const genealogy_t& X) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(1));
  SET_STRING_ELT(out,0,mkChar(X.describe().c_str()));
  UNPROTECT(1);
  return out;
}

//! structure in R list format
SEXP structure (const genealogy_t& G) {
  return G.structure();
}

//! tree in newick format
SEXP newick (const genealogy_t& X) {
  SEXP out;
  PROTECT(out = NEW_CHARACTER(1));
  SET_STRING_ELT(out,0,mkChar(X.newick().c_str()));
  UNPROTECT(1);
  return out;
}

//! number of lineages through time
SEXP lineage_count (const genealogy_t& G) {
  return G.lineage_count();
}

extern "C" {

  //! extract requested information
  //! prune and/or obscure if requested
  SEXP infoBare (SEXP State, SEXP Prune, SEXP Obscure, SEXP Trace,
		 SEXP T0, SEXP Time, SEXP Descript,
		 SEXP Yaml, SEXP Structure, SEXP Ndeme,
		 SEXP Lineages, SEXP Tree,
		 SEXP Nsample, SEXP Genealogy) {
    genealogy_t A = State;

    // prune and/or obscure if requested
    bool do_prune = *LOGICAL(AS_LOGICAL(Prune));
    bool do_obscure = *LOGICAL(AS_LOGICAL(Obscure));
    bool do_trace = *LOGICAL(AS_LOGICAL(Trace));
    if (do_prune) A.prune();
    if (do_obscure) A.obscure();
    if (do_trace) A.trace_lineages();

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

    bool get_ndeme = *LOGICAL(AS_LOGICAL(Ndeme));
    if (get_ndeme) nout++;

    bool get_lin = *LOGICAL(AS_LOGICAL(Lineages));
    if (get_lin) nout++;

    bool get_tree = *LOGICAL(AS_LOGICAL(Tree));
    if (get_tree) nout++;

    bool get_nsamp = *LOGICAL(AS_LOGICAL(Nsample));
    if (get_nsamp) nout++;

    bool get_geneal = *LOGICAL(AS_LOGICAL(Genealogy));
    if (get_geneal) nout++;

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
    if (get_ndeme) {
      k = set_list_elem(out,outnames,ndeme(A),"ndeme",k);
    }
    if (get_lin) {
      k = set_list_elem(out,outnames,lineage_count(A),"lineages",k);
    }
    if (get_tree) {
      k = set_list_elem(out,outnames,newick(A),"newick",k);
    }
    if (get_nsamp) {
      k = set_list_elem(out,outnames,nsample(A),"nsample",k);
    }
    if (get_geneal) {
      k = set_list_elem(out,outnames,serial(A),"genealogy",k);
    }

    SET_NAMES(out,outnames);

    UNPROTECT(2);
    return out;
  }

  //! curtail the given genealogy
  SEXP curtailBare (SEXP State, SEXP Time) {
    genealogy_t A = State;
    A.curtail(*REAL(AS_NUMERIC(Time)));
    return serial(A);
  }

}
