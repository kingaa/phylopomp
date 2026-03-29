#include "node.h"
#include "genealogy.h"
#include "generics.h"
#include "internal.h"
#include <Rinternals.h>

//! shifts name to avoid overlap
void
node_t::reuniqify
(name_t shift)
{
  this->uniq += shift;
  for (ball_t *b : *this) {
    b->uniq += shift;
  }
}

//! shifts names to avoid overlap
void
genealogy_t::reuniqify
(name_t shift)
{
  this->_unique += shift;
  for (node_t *p : *this) p->reuniqify(shift);
}

//! merges two genealogies, adjusting time, t0, and ndeme as needed
genealogy_t&
genealogy_t::operator+=
(const genealogy_t& other)
{
  genealogy_t G = other;
  G.reuniqify(_unique);
  merge(G,compare);
  timezero() = (timezero() < G.timezero()) ? timezero() : G.timezero();
  time() = (time() > G.time()) ? time() : G.time();
  ndeme() = (ndeme() > G.ndeme()) ? ndeme() : G.ndeme();
  _unique = G._unique;
  repair_roots();
  return *this;
}

extern "C" {

  //! combine genealogies
  SEXP genealSum (SEXP args) {
    args = CDR(args);
    genealogy_t A(R_PosInf);    // a "null" genealogy
    A.time() = R_NegInf;
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
