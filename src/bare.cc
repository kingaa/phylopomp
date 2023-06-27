// Bare genealogy

#include "genealogy.h"
#include "generics.h"
#include "internal.h"

template<>
SEXP serial (const genealogy_t& X) {
  SEXP out;
  PROTECT(out = NEW_RAW(X.bytesize()));
  X >> RAW(out);
  SET_ATTR(out,install("class"),mkString("gpgen"));
  UNPROTECT(1);
  return out;
}

extern "C" {

  //! curtail the given genealogy
  SEXP curtail (SEXP State, SEXP Time) {
    genealogy_t A = State;
    A.curtail(*REAL(AS_NUMERIC(Time)));
    return serial(A);
  }

  //! extract a YAML description
  SEXP yaml (SEXP State) {
    genealogy_t A = State;
    return mkString(A.yaml().c_str());
  }

}
