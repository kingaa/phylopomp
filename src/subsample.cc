#include "genealogy.h"
#include "generics.h"
#include "internal.h"

#include <R.h>
#include <Rdefines.h>

genealogy_t&
genealogy_t::subsample
(double frac)
{
  prune();
  for (node_t *p : *this) {
    for (auto it = p->begin(); it != p->end(); ) {
      if ((*it)->is(blue) && unif_rand() > frac) {
        ball_t *b = *it;
        it = p->erase(it);
        delete b;
      } else {
        ++it;
      }
    }
  }
  comb();
  return *this;
}

extern "C" {

  //! extract the bare genealogy
  SEXP subsample (SEXP State, SEXP Frac) {
    SEXP S;
    genealogy_t A = State;
    PROTECT(S = serial(A.subsample(*REAL(Frac))));
    SET_ATTR(S,install("class"),mkString("gpgen"));
    UNPROTECT(1);
    return S;
  }

}
