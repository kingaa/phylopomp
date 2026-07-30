#include "genealogy.h"
#include "generics.h"
#include "internal.h"
#include <utility>
#include <vector>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

static R_INLINE SEXP
make_matrix
(size_t nrow, size_t ncol, const char **names)
{
  SEXP dim, x;
  SEXP dimnm, nm;
  int *dimp;
  size_t k;
  PROTECT(dim = NEW_INTEGER(2));
  PROTECT(dimnm = Rf_allocVector(VECSXP,2));
  PROTECT(nm = NEW_CHARACTER(ncol));
  for (k = 0; k < ncol; k++)
    SET_STRING_ELT(nm,k,mkChar(names[k]));
  dimp = INTEGER(dim);
  dimp[0] = nrow; dimp[1] = ncol;
  PROTECT(x = Rf_allocArray(REALSXP,dim));
  SET_ELEMENT(dimnm,0,R_NilValue);
  SET_ELEMENT(dimnm,1,nm);
  SET_DIMNAMES(x,dimnm);
  UNPROTECT(4);
  return x;
}

slate_t
node_t::joining_branch_length
(const std::unordered_map<name_t, bool>& memo) const
{
  const node_t *p = parent();
  while (!p->is_root() && !memo.at(p->uniq)) p = p->parent();
  return slate - p->slate;
}

void node_t::cblv
(
 std::vector<slate_t>& x,
 std::vector<slate_t>& y,
 std::unordered_map<name_t, bool>& memo,
 const std::unordered_map<name_t, std::vector<node_t*>>& children,
 slate_t t0
 ) const
{
  assert(!memo[uniq]);
  const std::vector<node_t*>& ch = children.at(uniq);
  if (ch.empty()) {
    // leaf node: push joining branch length to x
    x.push_back(joining_branch_length(memo));
    memo[uniq] = true;
  } else {
    // first child: recurse
    ch[0]->cblv(x, y, memo, children, t0);
    // subsequent children:
    for (size_t i = 1; i < ch.size(); i++) {
      // push the height of current node into y
      y.push_back(slate - t0);
      memo[uniq] = true;
      ch[i]->cblv(x, y, memo, children, t0);
    }
  }
}

std::pair<std::vector<slate_t>, std::vector<slate_t>>
genealogy_t::cblv
(void) const
{
  auto children = children_map();
  auto roots  = ladderize(children);
  std::vector<slate_t> x, y;
  x.reserve(nsample());
  y.reserve(nsample());
  std::unordered_map<name_t, bool> memo;
  memo.reserve(size());
  for (node_t* p : *this) memo[p->uniq] = false;
  slate_t t0 = timezero();
  for (node_t* p : roots) {
    p->cblv(x, y, memo, children, t0);
    y.push_back(slate_t(0));
    memo[p->uniq] = true;
  }
  return {x, y};
}

genealogy_t&
genealogy_t::parse_cblv
(
 const double *x,
 const double *y,
 int nin,
 double tin
 )
{
  if (nin <= 0) err("invalid CBLV");
  size_t n = size_t(nin);
  slate_t t0 = timezero();
  slate_t t = slate_t(tin);
  if (t < t0+slate_t(x[0]))
    err("invalid CBLV: x[0] = %lg > %lg = time-t0", x[0], t-t0);
  time() = t;
  node_t* p = 0;
  for (size_t k = 0; k < n; k++) {
    if (p == 0) {               // new root node at time t0
      p = make_node();
      p->slate = t0;
      push_back(p);
    }
    if (x[k] < 0)
      err("invalid CBLV: negative tip-edge length in position %zu", k+1);
    node_t* q = make_node();    // new tip node
    q->slate = p->slate + slate_t(x[k]);
    attach(p, q);
    push_back(q);
    t = t0 + slate_t(y[k]);     // new internal node time
    if (y[k] < 0)
      err("invalid CBLV: negative internal branch-time in position %zu", k+1);
    if (t > t0) {
      node_t* i = q->parent();  // points to p
      node_t* j = q;
      if (j->slate < t) err("invalid CBLV: node %zu cannot attach.", k+1);
      while (j != i && i->slate > t) {
        j = i;
        i = i->parent();
      }
      assert(j != i);
      // Create new internal node at time t
      node_t* node = make_node();
      node->slate = t;
      attach(i, node);
      move(j->green_ball(), i, node);
      push_back(node);
      p = node;
    } else {
      p = 0;
    }
  }
  if (p != 0) err("invalid CBLV: last value of y is nonzero.");
  sort(); cap_tips(); clip_zlb(); weed();
  return *this;
}

SEXP
cblv
(genealogy_t& A)
{
  const char *colnames[] = {"tip","node"};
  double *x, *y;
  size_t i, n;
  SEXP S;
  std::pair<std::vector<slate_t>, std::vector<slate_t>> rep;
  rep = A.prune().obscure().insert_zlb().cblv();
  n = rep.first.size();
  PROTECT(S = make_matrix(n,2,colnames));
  x = REAL(S);
  y = REAL(S)+n;
  for (i = 0; i < n; i++) {
    x[i] = rep.first[i];
    y[i] = rep.second[i];
  }
  UNPROTECT(1);
  return S;
}

extern "C" {

  //! construct CBLV representation as a matrix
  SEXP cblv (SEXP State) {
    genealogy_t A = State;
    return cblv(A);
  }

  //! parse CBLV representation
  SEXP parse_cblv (SEXP XY, SEXP T0, SEXP Time) {
    int *n = INTEGER(GET_DIM(XY));
    if (n[1] != 2)
      err("in 'parse_cblv': 'xy' must be a two-column matrix.");
    double *xp = REAL(XY);
    double *yp = xp+n[0];
    double *t0 = REAL(T0);
    double *time = REAL(Time);
    genealogy_t A(*t0);
    A.parse_cblv(xp,yp,*n,*time);
    return serial(A);
  }

}
