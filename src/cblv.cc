#include "genealogy.h"
#include "generics.h"
#include "internal.h"
#include <utility>
#include <vector>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

static R_INLINE SEXP make_matrix (size_t nrow, size_t ncol, const char **names) {
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

slate_t node_t::joining_branch_length
(
 const std::unordered_map<name_t, bool>& memo
 ) const {
  const node_t *p = parent();
  while (!p->holds_own() && !memo.at(p->uniq)) p = p->parent();
  return slate - p->slate;
}

void node_t::cblv
(
 std::vector<slate_t>& x,
 std::vector<slate_t>& y,
 std::unordered_map<name_t, bool>& memo,
 const std::unordered_map<name_t, std::vector<node_t*>>& children,
 slate_t t0
 ) const {
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
genealogy_t::cblv (void) const {
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

SEXP cblv (genealogy_t& A) {
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

}
