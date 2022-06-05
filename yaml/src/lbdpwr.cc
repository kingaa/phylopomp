// LBDPwr: Linear birth-death-sampling model with reassortment (C++)
// two segments
#include "master.h"
#include "popul_proc.h"
#include "generics.h"

//! LBDP state.
typedef struct {
  int n;
} lbdpwr_state_t;

//! LBDP with reassortment parameters.
typedef struct {
  double lambda;
  double mu;
  double psi;
  double rhoA;
  double rhoB;
  double frac;
  int n0;
} lbdpwr_parameters_t;

using lbdpwr_proc_t = popul_proc_t<lbdpwr_state_t,lbdpwr_parameters_t,5>;
using lbdpwr_genealogy_t = master_t<lbdpwr_proc_t,1,2>;

template<>
std::string lbdpwr_proc_t::yaml (std::string tab) const {
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
void lbdpwr_proc_t::update_params (double *p, int n) {
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
void lbdpwr_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(n0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double lbdpwr_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.lambda * state.n);
  RATE_CALC(params.mu * state.n);
  RATE_CALC(params.psi * state.n);
  RATE_CALC(params.rhoA * state.n);
  RATE_CALC(params.rhoB * state.n);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void lbdpwr_genealogy_t::rinit (void) {
  state.n = params.n0;
  graft(0,params.n0);
}

template<>
void lbdpwr_genealogy_t::jump (int event) {
  name_t* seg;
  switch (event) {
  case 0:
    state.n += 1; birth();
    break;
  case 1:
    state.n -= 1; death();
    break;
  case 2:
    sample();
    break;
  case 3:
    seg = (name_t*)malloc(1*sizeof(name_t));
    seg[0] = 0UL;
    reassort(0,0,seg,1);
    free(seg);
    break;
  case 4:
    seg = (name_t*)malloc(1*sizeof(name_t));
    seg[0] = 1UL;
    reassort(0,0,seg,1);
    free(seg);
    break;
  default:
    err("in %s: c'est impossible! (%ld)",__func__,event);
  break;
  }
}

template<>
void lbdpwr_genealogy_t::batch (void) {
  batch_sample(params.frac);
}

GENERICS(LBDPwr,lbdpwr_genealogy_t)