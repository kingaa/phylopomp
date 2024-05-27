// Get information about a genealogy

#include "genealogy.h"
#include "generics.h"
#include "internal.h"

static size_t matchargs (const char *prov, const char **set, size_t n) {
  size_t i;
  for (i = 0; i < n; i++) {
    if (strcmp(prov,set[i]) == 0) break;
  }
  return i;
}

extern "C" {

  //! extract requested information
  //! prune and/or obscure if requested
  SEXP getInfo (SEXP args) {
    const char *argname[] = {
      "object","prune","obscure",
      "t0","time","nsample","ndeme",
      "description","structure","yaml","newick",
      "lineages","gendat","genealogy"};
    const int narg = sizeof(argname)/sizeof(const char *);
    bool flag[narg];
    SEXP object = R_NilValue;
    size_t nout = 0;
    int k;

    for (k = 0; k < narg; k++) flag[k] = false;
    args = CDR(args);

    while (args != R_NilValue) {
      const char *name = isNull(TAG(args)) ? "" : CHAR(PRINTNAME(TAG(args)));
      SEXP arg = CAR(args);
      size_t j = matchargs(name,argname,narg);
      if (j == 0) {
        object = arg;
        flag[0] = true;
      } else if (j < 3) {
        flag[j] = *LOGICAL(AS_LOGICAL(arg));
      } else if (j < narg) {
        flag[j] = *LOGICAL(AS_LOGICAL(arg));
        if (flag[j]) nout++;
      } else {
        err("unrecognized argument '%s' in '%s'.",name,__func__);
      }
      args = CDR(args);
    }

    if (!flag[0]) err("no genealogy furnished to '%s'",__func__);
    genealogy_t A = object;

    // prune and/or obscure if requested
    const bool *f = flag+1;
    if (*(f++)) A.prune();
    if (*(f++)) A.obscure();
    A.trace_lineages();

    SEXP out, outnames;
    PROTECT(out = NEW_LIST(nout));
    PROTECT(outnames = NEW_CHARACTER(nout));
    k = 0;
    if (*(f++)) {               // t0
      k = set_list_elem(out,outnames,timezero(A),"t0",k);
    }
    if (*(f++)) {               // time
      k = set_list_elem(out,outnames,time(A),"time",k);
    }
    if (*(f++)) {               // nsample
      k = set_list_elem(out,outnames,nsample(A),"nsample",k);
    }
    if (*(f++)) {               // ndeme
      k = set_list_elem(out,outnames,ndeme(A),"ndeme",k);
    }
    if (*(f++)) {               // description
      k = set_list_elem(out,outnames,describe(A),"description",k);
    }
    if (*(f++)) {               // structure
      k = set_list_elem(out,outnames,structure(A),"structure",k);
    }
    if (*(f++)) {               // yaml
      k = set_list_elem(out,outnames,yaml(A),"yaml",k);
    }
    if (*(f++)) {               // newick
      k = set_list_elem(out,outnames,newick(A),"newick",k);
    }
    if (*(f++)) {               // lineages
      k = set_list_elem(out,outnames,lineage_count(A),"lineages",k);
    }
    if (*(f++)) {               // gendat
      k = set_list_elem(out,outnames,gendat(A),"gendat",k);
    }
    if (*(f++)) {               // genealogy
      SEXP S;
      PROTECT(S = serial(A));
      SET_ATTR(S,install("class"),mkString("gpgen"));
      k = set_list_elem(out,outnames,S,"genealogy",k);
      UNPROTECT(1);
    }
    SET_NAMES(out,outnames);
    UNPROTECT(2);
    return out;
  }
}
