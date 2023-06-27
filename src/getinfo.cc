// Bare genealogy

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
      "object","prune","obscure","trace",
      "t0","time","nsample","ndeme",
      "description","structure","yaml","newick",
      "lineages","genealogy"};
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
      } else if (j < 4) {
        flag[j] = *LOGICAL(AS_LOGICAL(arg));
      } else if (j < narg) {
        flag[j] = *LOGICAL(AS_LOGICAL(arg));
        if (flag[j]) nout++;
      } else {
        err("unrecognized argument '%s' in '%s'.",name,__func__);
      }
      args = CDR(args);
    }

    // prune and/or obscure if requested
    if (!flag[0]) err("no genealogy furnished to '%s'",__func__);
    genealogy_t A = object;

    if (flag[1]) A.prune();
    if (flag[2]) A.obscure();
    if (flag[3]) A.trace_lineages();

    SEXP out, outnames;
    PROTECT(out = NEW_LIST(nout));
    PROTECT(outnames = NEW_CHARACTER(nout));
    k = 0;
    if (flag[4]) {              // t0
      k = set_list_elem(out,outnames,timezero(A),argname[4],k);
    }
    if (flag[5]) {              // time
      k = set_list_elem(out,outnames,time(A),argname[5],k);
    }
    if (flag[6]) {              // nsample
      k = set_list_elem(out,outnames,nsample(A),argname[6],k);
    }
    if (flag[7]) {              // ndeme
      k = set_list_elem(out,outnames,ndeme(A),argname[7],k);
    }
    if (flag[8]) {              // description
      k = set_list_elem(out,outnames,describe(A),argname[8],k);
    }
    if (flag[9]) {              // structure
      k = set_list_elem(out,outnames,structure(A),argname[9],k);
    }
    if (flag[10]) {             // yaml
      k = set_list_elem(out,outnames,yaml(A),argname[10],k);
    }
    if (flag[11]) {             // newick
      k = set_list_elem(out,outnames,newick(A),argname[11],k);
    }
    if (flag[12]) {             // lineages
      k = set_list_elem(out,outnames,lineage_count(A),argname[12],k);
    }
    if (flag[13]) {             // genealogy
      SEXP S;
      PROTECT(S = serial(A));
      SET_ATTR(S,install("class"),mkString("gpgen"));
      k = set_list_elem(out,outnames,S,argname[13],k);
      UNPROTECT(1);
    }
    SET_NAMES(out,outnames);
    UNPROTECT(2);
    return out;
  }
}
