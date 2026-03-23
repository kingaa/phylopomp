//! R list description

#include "genealogy.h"
#include "internal.h"

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>

SEXP
ball_t::structure
(void) const
{
  SEXP O, On, Name, Color, Deme;
  int size = (is(black)) ? 3 : 2;
  PROTECT(O = NEW_LIST(size));
  PROTECT(On = NEW_CHARACTER(size));
  PROTECT(Name = NEW_INTEGER(1));
  *INTEGER(Name) = int(uniq);
  PROTECT(Color = NEW_CHARACTER(1));
  SET_STRING_ELT(Color,0,mkChar(color_symbol().c_str()));
  set_list_elem(O,On,Name,"name",0);
  set_list_elem(O,On,Color,"color",1);
  if (is(black)) {
    PROTECT(Deme = NEW_INTEGER(1));
    *INTEGER(Deme) = int(deme());
    set_list_elem(O,On,Deme,"deme",2);
    UNPROTECT(1);
  }
  SET_NAMES(O,On);
  UNPROTECT(4);
  return O;
}

SEXP
pocket_t::structure
(void) const
{
  SEXP o;
  PROTECT(o = NEW_LIST(size()));
  int k = 0;
  for (ball_rev_it i = crbegin(); i != crend(); i++) {
    SET_ELEMENT(o,k++,(*i)->structure());
  }
  UNPROTECT(1);
  return o;
}

SEXP
node_t::structure
(void) const
{
  SEXP O, On;
  PROTECT(O = NEW_LIST(4));
  PROTECT(On = NEW_CHARACTER(4));
  set_list_elem(O,On,ScalarInteger(int(uniq)),"name",0);
  set_list_elem(O,On,ScalarReal(double(slate)),"time",1);
  set_list_elem(O,On,ScalarInteger(int(deme())),"deme",2);
  set_list_elem(O,On,pocket_t::structure(),"pocket",3);
  SET_NAMES(O,On);
  UNPROTECT(2);
  return O;
}

SEXP
nodeseq_t::structure
(void) const
{
  SEXP Nodes;
  PROTECT(Nodes = NEW_LIST(size()));
  int k = 0;
  for (node_t *p : *this) {
    SET_ELEMENT(Nodes,k++,p->structure());
  }
  UNPROTECT(1);
  return Nodes;
}

SEXP
genealogy_t::structure
(void) const
{
  SEXP O, On, T0, Time, Nodes;
  PROTECT(O = NEW_LIST(3));
  PROTECT(On = NEW_CHARACTER(3));
  PROTECT(Time = NEW_NUMERIC(1));
  *REAL(Time) = double(time());
  PROTECT(T0 = NEW_NUMERIC(1));
  *REAL(T0) = double(timezero());
  PROTECT(Nodes = nodeseq_t::structure());
  set_list_elem(O,On,Time,"time",0);
  set_list_elem(O,On,T0,"t0",1);
  set_list_elem(O,On,Nodes,"nodes",2);
  SET_NAMES(O,On);
  UNPROTECT(5);
  return O;
}
