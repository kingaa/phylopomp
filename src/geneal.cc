#include "genealogy.h"
#include "generics.h"
#include "internal.h"

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>

extern "C" {

  //! extract the bare genealogy
  SEXP geneal (SEXP State) {
    SEXP S;
    PROTECT(S = serial(genealogy_t(State)));
    SET_ATTR(S,install("class"),mkString("gpgen"));
    UNPROTECT(1);
    return S;
  }

}
