//! lineage count, saturation, and event-type.
//! types are:
//! -  0 = non-event
//! - -1 = root
//! -  1 = sample
//! -  2 = non-sample node

#include "genealogy.h"
#include "internal.h"

void
node_t::lineage_incr
(int *incr, int *sat, int *etype) const
{
  const name_t d = deme();
  incr[d]--;
  for (ball_t *b : *this) {
    switch (b->color) {
    case green: case black:
      incr[b->deme()]++;
      sat[b->deme()]++;
      break;
    default:
      break;
    }
  }
  if (holds_own()) {
    sat[d]--;
    etype[d] = -1;
  } else if (holds(blue)) {
    etype[d] = 1;
  } else {
    etype[d] = 2;
  }
}

void
genealogy_t::lineage_count
(double *tout, int *deme,
 int *ell, int *sat, int *etype) const
{
  size_t nd = ndeme()+1;
  slate_t tcur = timezero();
  for (size_t j = 0; j < nd; j++) {
    tout[j] = tcur;
    deme[j] = j;
    sat[j] = ell[j] = 0;
    etype[j] = 0;
  }
  for (const node_t *p : *this) {
    if (tcur < p->slate) {
      tout += nd; ell += nd; sat += nd;
      deme += nd; etype += nd;
      tcur = p->slate;
      for (size_t j = 0; j < nd; j++) {
        tout[j] = tcur;
        deme[j] = j;
        ell[j] = (ell-nd)[j];
        sat[j] = 0;
        etype[j] = 0;
      }
    }
    p->lineage_incr(ell,sat,etype);
  }
  tout += nd; ell += nd; sat += nd;
  deme += nd; etype += nd;
  tcur = time();
  for (size_t j = 0; j < nd; j++) {
    tout[j] = tcur;
    sat[j] = ell[j] = 0;
    deme[j] = j;
    etype[j] = 3;
  }
}

//! lineage count and saturation
SEXP
genealogy_t::lineage_count
(void) const
{
  SEXP tout, deme, ell, sat, etype, out, outn;
  int nt = ntime(timezero())+1;
  int nl = (ndeme()+1)*nt;
  PROTECT(tout = NEW_NUMERIC(nl));
  PROTECT(deme = NEW_INTEGER(nl));
  PROTECT(ell = NEW_INTEGER(nl));
  PROTECT(sat = NEW_INTEGER(nl));
  PROTECT(etype = NEW_INTEGER(nl));
  PROTECT(out = NEW_LIST(5));
  PROTECT(outn = NEW_CHARACTER(5));
  set_list_elem(out,outn,tout,"time",0);
  set_list_elem(out,outn,deme,"deme",1);
  set_list_elem(out,outn,ell,"lineages",2);
  set_list_elem(out,outn,sat,"saturation",3);
  set_list_elem(out,outn,etype,"event_type",4);
  SET_NAMES(out,outn);
  lineage_count(REAL(tout),INTEGER(deme),INTEGER(ell),
                INTEGER(sat),INTEGER(etype));
  UNPROTECT(7);
  return out;
}
