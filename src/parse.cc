#include "genealogy.h"
#include "generics.h"

//! A parser for Newick code.
//! Returns a genealogy in the phylopomp format.

extern "C" {

  SEXP parse_newick
  (
   SEXP X, SEXP T0, SEXP Prune, SEXP Obscure, SEXP Time,
   SEXP Descript, SEXP Yaml, SEXP Structure, SEXP Lineages, SEXP Tree
   ) {

    PROTECT(X = AS_CHARACTER(X));
    PROTECT(T0 = AS_NUMERIC(T0));

    // parse the Newick representation into a genealogy:
    std::string x = (const char *) CHAR(STRING_ELT(X,0));
    genealogy_t G;

    G.parse(x,*REAL(T0));

    // prune and/or obscure if requested
    bool do_prune = *LOGICAL(AS_LOGICAL(Prune));
    bool do_obscure = *LOGICAL(AS_LOGICAL(Obscure));
    if (do_prune) G.prune();
    if (do_obscure) G.obscure();

    size_t nout = 0;

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

    // pack up return values in a list
    int k = 0;
    SEXP out, outnames;
    PROTECT(out = NEW_LIST(nout));
    PROTECT(outnames = NEW_CHARACTER(nout));
    if (get_time) {
      k = set_list_elem(out,outnames,time(G),"time",k);
    }
    if (get_desc) {
      k = set_list_elem(out,outnames,describe(G),"description",k);
    }
    if (get_yaml) {
      k = set_list_elem(out,outnames,yaml(G),"yaml",k);
    }
    if (get_struc) {
      k = set_list_elem(out,outnames,structure(G),"structure",k);
    }
    if (get_lin) {
      k = set_list_elem(out,outnames,lineage_count(G),"lineages",k);
    }
    if (get_tree) {
      k = set_list_elem(out,outnames,newick(G),"tree",k);
    }
    SET_NAMES(out,outnames);

    UNPROTECT(4);
    return out;
  }

}
