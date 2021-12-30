// SIIR with Sampling Genealogy Process Simulator (C++)

#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
  int S;			// number of susceptibles
  int I1, I2;			// number of infections
  int R;			// number of recovereds
} siir_state_t;

typedef struct {
  double Beta1, Beta2;        // transmission rate
  double gamma;               // recovery rate
  double psi1, psi2;          // sampling rates
  double sigma12, sigma21;    // movement rates
  double N;                   // host population size
  int S0;                     // initial susceptibles
  int I1_0, I2_0;             // initial infecteds
  int R0;                     // initial recoveries
} siir_parameters_t;

using siir_proc_t = popul_proc_t<siir_state_t,siir_parameters_t,8>;
using siir_genealogy_t = master_t<siir_proc_t,2>;

// human-readable info
template<>
std::string siir_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta1)
    + YAML_PARAM(Beta2)
    + YAML_PARAM(gamma)
    + YAML_PARAM(psi1)
    + YAML_PARAM(psi2)
    + YAML_PARAM(sigma12)
    + YAML_PARAM(sigma21)
    + YAML_PARAM(S0)
    + YAML_PARAM(I1_0)
    + YAML_PARAM(I2_0)
    + YAML_PARAM(R0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(I1)
    + YAML_STATE(I2)
    + YAML_STATE(R);
  return p+s;
}

template<>
void siir_proc_t::valid (void) const {
  if (params.N <= 0) err("total population size must be positive!");
  if (state.S < 0 || state.I1 < 0 || state.I2 < 0 || state.R < 0) err("negative state variables!");
  if (params.N != state.S+state.I1+state.I2+state.R) err("population leakage!");
}

template<>
void siir_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta1);
  PARAM_SET(Beta2);
  PARAM_SET(gamma);
  PARAM_SET(psi1);
  PARAM_SET(psi2);
  PARAM_SET(sigma12);
  PARAM_SET(sigma21);
  if (m != n) err("wrong number of parameters!");
}

template<>
void siir_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(I1_0);
  PARAM_SET(I2_0);
  PARAM_SET(R0);
  params.N = double(params.S0+params.I1_0+params.I2_0+params.R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double siir_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta1 * state.S * state.I1 / params.N);
  RATE_CALC(params.Beta2 * state.S * state.I2 / params.N);
  RATE_CALC(params.gamma * state.I1);
  RATE_CALC(params.gamma * state.I2);
  RATE_CALC(params.psi1 * state.I1);
  RATE_CALC(params.psi2 * state.I2);
  RATE_CALC(params.sigma12 * state.I1);
  RATE_CALC(params.sigma21 * state.I2);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void siir_genealogy_t::rinit (void) {
  state.S = params.S0;
  state.I1 = params.I1_0;
  state.I2 = params.I2_0;
  state.R = params.R0;
  graft(0,params.I1_0);
  graft(1,params.I2_0);
}

template<>
void siir_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
    state.S -= 1;
    state.I1 += 1;
    birth(0,0);
    break;
  case 1:
    state.S -= 1;
    state.I2 += 1;
    birth(1,1);
    break;
  case 2:
    state.I1 -= 1;
    state.R += 1;
    death(0);
    break;
  case 3:
    state.I2 -= 1;
    state.R += 1;
    death(1);
    break;
  case 4:                     // sample from deme 1
    sample(0);
    break;
  case 5:                     // sample from deme 2
    sample(1);
    break;
  case 6:                     // move from deme 1 -> 2
    state.I1 -= 1;
    state.I2 += 1;
    migrate(0,1);
    break;
  case 7:                     // move from deme 2 -> 1
    state.I1 += 1;
    state.I2 -= 1;
    migrate(1,0);
    break;
  default:						    // #nocov
    err("in %s: c'est impossible! (%ld)",__func__,event); // #nocov
    break;
  }
}

GENERICS(SIIR,siir_genealogy_t)
