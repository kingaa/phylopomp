// LBDPwr2: Linear birth-death-sampling model with reassortment, but not direct-descent (C++)
// two segments
#include "master.h"
#include "popul_proc.h"
#include "generics.h"

//! LBDP state.
typedef struct {
  int n;
} lbdpwr2_state_t;

//! LBDP with reassortment parameters.
typedef struct {
  double lambda;
  double mu;
  double psi;
  double rhoA;
  double rhoB;
  double frac;
  int n0;
} lbdpwr2_parameters_t;

using lbdpwr2_proc_t = popul_proc_t<lbdpwr2_state_t,lbdpwr2_parameters_t,5>;
using lbdpwr2_genealogy_t = master_t<lbdpwr2_proc_t,1,2,true>;

template<>
std::string lbdpwr2_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
  + YAML_PARAM(lambda)
    + YAML_PARAM(mu)
    + YAML_PARAM(psi)
    + YAML_PARAM(rhoA)
    + YAML_PARAM(rhoB)
    + YAML_PARAM(frac)
    + YAML_PARAM(n0);
    std::string s = tab + "state:\n"
    + YAML_STATE(n);
    return p+s;
}

template<>
void lbdpwr2_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(lambda);
  PARAM_SET(mu);
  PARAM_SET(psi);
  PARAM_SET(rhoA);
  PARAM_SET(rhoB);
  PARAM_SET(frac);
  if (m != n) err("wrong number of parameters!");
}

template<>
void lbdpwr2_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(n0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double lbdpwr2_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.lambda * state.n);
  RATE_CALC(params.mu * state.n);
  RATE_CALC(params.psi * state.n);
  if (state.n > 1)  {
    RATE_CALC(params.rhoA * state.n);
    RATE_CALC(params.rhoB * state.n);
    if (m != n) err("wrong number of events!");
  } else {
    if (m != n-2) err("wrong number of events!");
  }
  return total;
}

template<>
void lbdpwr2_genealogy_t::rinit (void) {
  state.n = params.n0;
  graft(0,params.n0);
}

template<>
void lbdpwr2_genealogy_t::jump (int event) {
  name_t seg[1];
  switch (event) {
  case 0:
    state.n += 1; birth();
    break;
  case 1:
    state.n -= 1; death();
    break;
  case 2:
    state.n -= 1; sample();
    break;
  case 3:
    seg[0] = 0UL;
    reassort(0,0,seg,1);
    break;
  case 4:
    seg[0] = 1UL;
    reassort(0,0,seg,1);
    break;
  default:
    err("in %s: c'est impossible! (%ld)",__func__,event);
  break;
  }
}

template<>
void lbdpwr2_genealogy_t::batch (void) {
  batch_sample(params.frac);
}

GENERICS(LBDPwr2,lbdpwr2_genealogy_t)