#include "genealogy.h"
#include "generics.h"
#include "internal.h"

//! A parser for Newick code.
//! Returns a genealogy in the phylopomp format.

extern "C" {

  SEXP parse_newick (SEXP X, SEXP T0, SEXP Tf) {
    PROTECT(X = AS_CHARACTER(X));
    PROTECT(T0 = AS_NUMERIC(T0));
    PROTECT(Tf = AS_NUMERIC(Tf));
    double t0 = *REAL(T0);
    double tf = *REAL(Tf);
    // parse the Newick representation into a genealogy:
    std::string x = CHAR(STRING_ELT(X,0));
    genealogy_t G(t0);
    G.parse(x,t0);
    if (!ISNA(tf)) {
      if (G.time() > tf) {
        G.curtail(tf);
      } else {
        G.time() = tf;
      }
    }
    G.trace_lineages();
    UNPROTECT(3);
    return serial(G);
  }

}
