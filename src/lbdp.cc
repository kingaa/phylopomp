// Linear birth-death-sampling genealogy process simulator (C++)

#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
  int n;
} lbdp_state_t;

typedef struct {
  double lambda;                // birth rate
  double mu;                    // death rate
  double psi;                   // sampling rate
  int n0;                       // initial population size
} lbdp_parameters_t;

using lbdp_proc_t = popul_proc_t<lbdp_state_t,lbdp_parameters_t,3>;
using lbdp_genealogy_t = master_t<lbdp_proc_t>;
  
template<>
void lbdp_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(lambda);
  PARAM_SET(mu);
  PARAM_SET(psi);
  if (m != n) err("wrong number of parameters!");
}

template<>
void lbdp_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(n0);
  if (m != n) err("wrong number of initial-value parameters!");
}

// human-readable info
template<>
std::string lbdp_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(lambda)
    + YAML_PARAM(mu)
    + YAML_PARAM(psi)
    + YAML_PARAM(n0);
  std::string s = tab + "state:\n"
    + YAML_STATE(n);
  return p+s;
}

template<>
double lbdp_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.lambda * state.n);	       // birth
  RATE_CALC(params.mu * state.n);	       // death
  RATE_CALC(params.psi * state.n);	       // sample
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void lbdp_proc_t::valid (void) const {
  if (state.n < 0) err("negative population size!");
}

template<>
void lbdp_genealogy_t::rinit (void) {
  state.n = params.n0;
  graft(0,params.n0);
}

template<>
void lbdp_genealogy_t::jump (int event) {
  switch (event) {
  case 0:                     // birth
    state.n += 1;
    birth();
    break;
  case 1:                     // death
    state.n -= 1;
    death();
    break;
  case 2:                     // sample
    sample();
    break;
  default:						    // #nocov
    err("in %s: c'est impossible! (%ld)",__func__,event); // #nocov
    break;
  }
}

GENERICS(LBDP,lbdp_genealogy_t)
