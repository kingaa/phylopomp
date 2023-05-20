#include "genealogy.h"

extern "C" {

  SEXP parser (SEXP S) {
    SEXP out;
    PROTECT(S = AS_CHARACTER(S));
    PROTECT(out = NEW_CHARACTER(1));
    std::string s = (const char *) CHAR(STRING_ELT(S,0));
    genealogy_t<1> G;
    G.parse(s,0);
    SET_STRING_ELT(out,0,mkChar(G.newick().c_str()));
    UNPROTECT(2);
    return out;
  }

}
