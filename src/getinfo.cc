// Extract information from the stored state of a GP process

#include "moran.h"
#include "lbdp.h"
#include "sirws.h"

extern "C" {

  SEXP get_Moran_info (SEXP X, SEXP Prune, SEXP Compact) {
    return get_info<moran_tableau_t>(X,Prune,Compact);
  }

  SEXP get_LBDP_info (SEXP X, SEXP Prune, SEXP Compact) {
    return get_info<lbdp_tableau_t>(X,Prune,Compact);
  }

  SEXP get_SIRwS_info (SEXP X, SEXP Prune, SEXP Compact) {
    return get_info<sirws_tableau_t>(X,Prune,Compact);
  }

}

