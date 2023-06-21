// Bare genealogy

#include "genealogy.h"
#include "generics.h"
#include "internal.h"

extern "C" {

  //! curtail the given genealogy
  SEXP curtailBare (SEXP State, SEXP Time) {
    genealogy_t A = State;
    A.curtail(*REAL(AS_NUMERIC(Time)));
    return serial(A);
  }

}
