// SIR with Sampling Genealogy Process Simulator (C++)

#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
  int S;			// number of susceptibles
  int I;			// number of infections
  int R;			// number of recovereds
} sir_state_t;

typedef struct {
  double Beta;                // transmission rate
  double gamma;               // recovery rate
  double psi;                 // sampling rate
  double delta;		      // immunity waning rate
  double N;                   // host population size
  int S0;                     // initial susceptibles
  int I0;                     // initial infecteds
  int R0;                     // initial recoveries
} sir_parameters_t;

using sir_proc_t = popul_proc_t<sir_state_t,sir_parameters_t,4>;
using sir_genealogy_t = master_t<sir_proc_t>;

// human-readable info
template<>
std::string sir_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta)
    + YAML_PARAM(gamma)
    + YAML_PARAM(psi)
    + YAML_PARAM(delta)
    + YAML_PARAM(S0)
    + YAML_PARAM(I0)
    + YAML_PARAM(R0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(I)
    + YAML_STATE(R);
  return p+s;
}

template<>
void sir_proc_t::valid (void) const {
  if (params.N <= 0) err("total population size must be positive!");
  if (state.S < 0 || state.I < 0 || state.R < 0) err("negative state variables!");
  if (params.N != state.S+state.I+state.R) err("population leakage!");
}

template<>
void sir_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta);
  PARAM_SET(gamma);
  PARAM_SET(psi);
  PARAM_SET(delta);
  if (m != n) err("wrong number of parameters!");
}

template<>
void sir_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(I0);
  PARAM_SET(R0);
  params.N = double(params.S0+params.I0+params.R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double sir_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta * state.S * state.I / params.N); // infection
  RATE_CALC(params.gamma * state.I);                     // recovery
  RATE_CALC(params.psi * state.I);                       // sample
  RATE_CALC(params.delta * state.R);			  // waning
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void sir_genealogy_t::rinit (void) {
  state.S = params.S0;
  state.I = params.I0;
  state.R = params.R0;
  graft(0,params.I0);
}

template<>
void sir_genealogy_t::jump (int event) {
  switch (event) {
  case 0:                     // infection
    state.S -= 1;
    state.I += 1;
    birth();
    break;
  case 1:                     // recovery
    state.I -= 1;
    state.R += 1;
    death();
    break;
  case 2:                     // sample
    sample();
    break;
  case 3:			// waning
    state.S += 1;
    state.R -= 1;
    break;
  default:						    // #nocov
    err("in %s: c'est impossible! (%ld)",__func__,event); // #nocov
    break;
  }
}

GENERICS(SIR,sir_genealogy_t)
