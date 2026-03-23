// gendat: genealogy information extraction

#include "genealogy.h"
#include "internal.h"

//! genealogy information in list format
void
genealogy_t::gendat
(double *tout, int *anc, int *lin,
 int *sat, int *type, int *deme,
 int *index, int *child) const {
  int m, n, k;
  node_it i, j;
  for (k = 0, n = 0, i = begin(); i != end(); i++, n++) {
    node_t *p = *i;
    assert(!p->holds(black)); // tree should be pruned first
    tout[n] = p->slate;
    deme[n] = p->deme();
    if (p->is_root()) {
      type[n] = 0;            // root node
    } else if (p->holds(blue)) {
      type[n] = 1;            // sample node
      deme[n] = p->ball(blue)->deme();
    } else {
      type[n] = 2;            // internal node
    }
    lin[n] = p->lineage();    // 0-based indexing
    sat[n] = p->nchildren();
    index[n] = k;
    k += sat[n];
    child[n] = NA_INTEGER;
    if (p->is_root()) {
      anc[n] = n;             // 0-based indexing
    } else {
      for (m = 0, j = begin(); j != i; j++, m++) {
        node_t *q = *j;
        if (p->parent()->uniq == q->uniq) {
          anc[n] = m;
          break;
        }
      }
    }
  }
  tout[n] = time();
  for (k = 0, n = 0, i = begin(); i != end(); i++, n++) {
    node_t *p = *i;
    j = i; j++;
    for (m = n+1; j != end(); m++, j++) {
      node_t *q = *j;
      if (p->uniq == q->parent()->uniq) {
        child[k++] = m;
      }
    }
  }
}

//! genealogy information in list format
SEXP
genealogy_t::gendat
(void) const {
  SEXP t0, tout, anc, lin, sat, type, deme, index, child, ns, nr, nn;
  SEXP out, outn;
  size_t n = length();
  PROTECT(t0 = NEW_NUMERIC(1));
  PROTECT(tout = NEW_NUMERIC(n+1));
  PROTECT(type = NEW_INTEGER(n));
  PROTECT(deme = NEW_INTEGER(n));
  PROTECT(lin = NEW_INTEGER(n));
  PROTECT(sat = NEW_INTEGER(n));
  PROTECT(index = NEW_INTEGER(n));
  PROTECT(child = NEW_INTEGER(n));
  PROTECT(anc = NEW_INTEGER(n));
  PROTECT(ns = NEW_INTEGER(1));
  PROTECT(nr = NEW_INTEGER(1));
  PROTECT(nn = NEW_INTEGER(1));
  PROTECT(out = NEW_LIST(12));
  PROTECT(outn = NEW_CHARACTER(12));
  set_list_elem(out,outn,t0,"t0",0);
  set_list_elem(out,outn,tout,"nodetime",1);
  set_list_elem(out,outn,type,"nodetype",2);
  set_list_elem(out,outn,deme,"deme",3);
  set_list_elem(out,outn,lin,"lineage",4);
  set_list_elem(out,outn,sat,"saturation",5);
  set_list_elem(out,outn,index,"index",6);
  set_list_elem(out,outn,child,"child",7);
  set_list_elem(out,outn,anc,"ancestor",8);
  set_list_elem(out,outn,ns,"nsample",9);
  set_list_elem(out,outn,nr,"nroot",10);
  set_list_elem(out,outn,nn,"nnode",11);
  SET_NAMES(out,outn);
  gendat(REAL(tout),INTEGER(anc),INTEGER(lin),INTEGER(sat),
         INTEGER(type),INTEGER(deme),INTEGER(index),INTEGER(child));
  *REAL(t0) = double(timezero()); // zero-time
  *INTEGER(ns) = nsample();       // number of samples
  *INTEGER(nr) = nroot();         // number of roots
  *INTEGER(nn) = length();        // number of nodes
  UNPROTECT(14);
  return out;
}

extern "C" {

  //! data-frame format
  SEXP gendat (SEXP State, SEXP Obscure) {
    genealogy_t A = State;
    A.prune();
    if (*LOGICAL(Obscure)) A.obscure();
    A.trace_lineages();
    return A.gendat();
  }

}
