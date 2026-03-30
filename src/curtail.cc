// Bare genealogy

#include "genealogy.h"
#include "generics.h"
#include "internal.h"

//! curtail the genealogy by removing nodes
//! with times later than tnew and/or earlier than troot
void
genealogy_t::curtail
(slate_t tnew, slate_t troot)
{
  if (tnew < troot) troot = tnew;
  if (!empty() && tnew < time()) {
    node_t *p = back();
    while (!empty() && p->slate > tnew) {
      ball_t *b;
      while (p->size() > 1) {
        b = p->last_ball();
        switch (b->color) {
        case black:
          p->erase(b); delete b;
          break;
        case green: case blue: // #nocov
          assert(0);           // #nocov
          break;               // #nocov
        }
      }
      b = p->last_ball();
      switch (b->color) {
      case blue:
        b->color = black;
      case black:
        b->deme() = p->deme();
        swap(b,p->green_ball());
      case green:
        destroy_node(p);
        break;
      }
      if (!empty()) p = back();
    }
  }
  time() = tnew;
  if (!empty() && troot > timezero()) {
    node_t *p = front();
    node_t *q;
    while (!empty() && p->slate < troot) {
      ball_t *b;
      assert(p->holds_own());
      while (p->size() > 1) {
        b = p->last_ball();
        switch (b->color) {
        case blue:
          p->erase(b); delete b;
          break;
        case black:
          q = make_node(b->deme());
          q->slate = troot;
          move(b,p,q); push_back(q);
          break;
        case green:
          q = b->child();
          if (q == p) {
            b = p->first_ball();
            q = b->child();
          }
          if (q->slate < troot) {
            move(b,p,q);
          } else {
            node_t *pp = make_node(b->deme());
            pp->slate = troot;
            move(b,p,pp); push_back(pp);
          }
          break;
        }
      }
      destroy_node(p);
      if (!empty()) p = front();
    }
    sort();
  }
  if (troot > timezero()) timezero() = troot;
}

extern "C" {

  //! curtail the given genealogy
  SEXP curtail (SEXP State, SEXP Time, SEXP Troot) {
    genealogy_t A = State;
    double t, t0;
    t = *REAL(AS_NUMERIC(Time));
    t0 = *REAL(AS_NUMERIC(Troot));
    if (ISNA(t)) t = A.time();
    if (ISNA(t0)) t0 = A.timezero();
    A.curtail(t,t0);
    SEXP out;
    PROTECT(out = serial(A));
    SET_ATTR(out,install("class"),mkString("gpgen"));
    UNPROTECT(1);
    return out;
  }

}
