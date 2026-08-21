// SIR: Classical susceptible-infected-recovered model (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static const int Infected = 1;

//! SIR process state.
typedef struct {
  int S;
  int I;
  int R;
} sir_state_t;

//! SIR process parameters.
typedef struct {
  double Beta;
  double gamma;
  double psi;
  double chi;
  double omega;
  double pop;
  double S0;
  double I0;
  double R0;
} sir_parameters_t;

using sir_proc_t = popul_proc_t<sir_state_t,sir_parameters_t,5>;
using sir_genealogy_t = master_t<sir_proc_t,1>;

template<>
std::string sir_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta)
    + YAML_PARAM(gamma)
    + YAML_PARAM(psi)
    + YAML_PARAM(chi)
    + YAML_PARAM(omega)
    + YAML_PARAM(pop)
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
void sir_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta);
  PARAM_SET(gamma);
  PARAM_SET(psi);
  PARAM_SET(chi);
  PARAM_SET(omega);
  if (m != n) err("wrong number of parameters!");
}

template<>
void sir_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(pop);
  PARAM_SET(S0);
  PARAM_SET(I0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double sir_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta * state.S * state.I / params.pop);
  RATE_CALC(params.gamma * state.I);
  RATE_CALC(params.psi * state.I);
  RATE_CALC(params.chi * state.I);
  RATE_CALC(params.omega * state.R);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void sir_genealogy_t::rinit (void) {
  double f = params.pop/(params.S0+params.I0+params.R0);
  state.S = nearbyint(f*params.S0);
  state.I = nearbyint(f*params.I0);
  state.R = nearbyint(f*params.R0);
  graft(Infected,state.I);
}

template<>
void sir_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
    state.S -= 1; state.I += 1; birth();
    break;
  case 1:
    state.I -= 1; state.R += 1; death();
    break;
  case 2:
    sample();
    break;
  case 3:
    state.I -= 1; state.R += 1; sample_death();
    break;
  case 4:
    state.R -= 1; state.S += 1;
    break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

GENERICS(SIR,sir_genealogy_t)
