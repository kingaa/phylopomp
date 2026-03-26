#include "genealogy.h"
#include "generics.h"
#include "internal.h"
#include <Rinternals.h>

extern "C" {

  //! combine genealogies
  SEXP genealSum (SEXP args) {
    args = CDR(args);
    genealogy_t A(R_PosInf,0,1,R_NegInf); // a "null" genealogy
    while (args != R_NilValue) {
      A += CAR(args);
      args = CDR(args);
    }
    SEXP S;
    PROTECT(S = serial(A));
    SET_ATTR(S,install("class"),mkString("gpgen"));
    UNPROTECT(1);
    return S;
  }

}
