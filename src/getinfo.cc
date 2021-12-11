// Extract information from the stored state of a GP process

#include "sir.h"

extern "C" {

  SEXP get_SIR_info (SEXP X, SEXP Prune, SEXP Compact) {
    return get_info<sir_tableau_t>(X,Prune,Compact);
  }

}

