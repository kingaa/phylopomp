// Moranwr: The classical Moran model with reassortment (C++)
// two segments
#include "master.h"
#include "popul_proc.h"
#include "generics.h"

//! Moran process state.
typedef struct {
  int m;
  int g;
} moranwr_state_t;

//! Moran process parameters.
typedef struct {
  double mu;
  double psi;
  double rhoA;
  double rhoB;
  double frac;
  int n;
} moranwr_parameters_t;

using moranwr_proc_t = popul_proc_t<moranwr_state_t,moranwr_parameters_t,4>;
using moranwr_genealogy_t = master_t<moranwr_proc_t,1,2>;

template<>
std::string moranwr_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(mu)
    + YAML_PARAM(psi)
    + YAML_PARAM(rhoA)
    + YAML_PARAM(rhoB)
    + YAML_PARAM(frac)
    + YAML_PARAM(n);
  std::string s = tab + "state:\n"
    + YAML_STATE(m)
    + YAML_STATE(g);
  return p+s;
}

template<>
void moranwr_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(mu);
  PARAM_SET(psi);
  PARAM_SET(rhoA);
  PARAM_SET(rhoB);
  PARAM_SET(frac);
  if (m != n) err("wrong number of parameters!");
}

template<>
void moranwr_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(n);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double moranwr_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.mu * params.n);
  RATE_CALC(params.psi * params.n);
  if (params.n > 1) {
    RATE_CALC(params.rhoA * params.n);
    RATE_CALC(params.rhoB * params.n);
    if (m != n) err("wrong number of events!");
  } else {
    if (m != n - 2) err("wrong number of events!");
  }
  return total;
}

template<>
void moranwr_genealogy_t::rinit (void) {
  state.m = state.g = 0;
  graft(0,params.n);
}

template<>
void moranwr_genealogy_t::jump (int event) {
  name_t seg[1];
  switch (event) {
  case 0:
    state.m += 1; birth(); death();
    break;
  case 1:
    state.g += 1; sample();
    break;
  case 2:
    seg[0] = 0UL;
    reassort(0,0,seg,1);
    break;
  case 3:
    seg[0] = 1UL;
    reassort(0,0,seg,1);
    break;
  default:
    err("in %s: c'est impossible! (%ld)",__func__,event);
    break;
  }
}

template<>
void moranwr_genealogy_t::batch (void) {
  batch_sample(params.frac);
}

GENERICS(Moranwr,moranwr_genealogy_t)
