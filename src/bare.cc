// Bare genealogy

#include "genealogy.h"
#include "generics.h"
#include "internal.h"

extern "C" {

  //! curtail the given genealogy
  SEXP curtail (SEXP State, SEXP Time) {
    genealogy_t A = State;
    A.curtail(*REAL(AS_NUMERIC(Time)));
    return serial(A);
  }

  //! curtail the given genealogy
  SEXP yaml (SEXP State) {
    genealogy_t A = State;
    return mkString(A.yaml().c_str());
  }

}
