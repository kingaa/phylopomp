// SIIR: Two-strain SIR model (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"

//! SIIR process state.
typedef struct {
  int S;
  int I1;
  int I2;
  int R;
  double N;
} siir_state_t;

//! SIIR process parameters.
typedef struct {
  double Beta1;
  double Beta2;
  double gamma;
  double psi1;
  double psi2;
  double sigma12;
  double sigma21;
  double delta;
  int S0;
  int I1_0;
  int I2_0;
  int R0;
} siir_parameters_t;

using siir_proc_t = popul_proc_t<siir_state_t,siir_parameters_t,9>;
using siir_genealogy_t = master_t<siir_proc_t,2>;

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
    + YAML_PARAM(delta)
    + YAML_PARAM(S0)
    + YAML_PARAM(I1_0)
    + YAML_PARAM(I2_0)
    + YAML_PARAM(R0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(I1)
    + YAML_STATE(I2)
    + YAML_STATE(R)
    + YAML_STATE(N);
  return p+s;
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
  PARAM_SET(delta);
  if (m != n) err("wrong number of parameters!");
}

template<>
void siir_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(I1_0);
  PARAM_SET(I2_0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double siir_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta1 * state.S * state.I1 / state.N);
  RATE_CALC(params.Beta2 * state.S * state.I2 / state.N);
  RATE_CALC(params.gamma * state.I1);
  RATE_CALC(params.gamma * state.I2);
  RATE_CALC(params.psi1 * state.I1);
  RATE_CALC(params.psi2 * state.I2);
  RATE_CALC(params.sigma12 * state.I1);
  RATE_CALC(params.sigma21 * state.I2);
  RATE_CALC(params.delta * state.R);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void siir_genealogy_t::rinit (void) {
  state.S = params.S0;
state.I1 = params.I1_0;
state.I2 = params.I2_0;
state.R = params.R0;
state.N = double(params.S0+params.I1_0+params.I2_0+params.R0);
graft(0,params.I1_0);
graft(1,params.I2_0);
}

template<>
void siir_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
      state.S -= 1; state.I1 += 1; birth(0,0);
      break;
    case 1:
      state.S -= 1; state.I2 += 1; birth(1,1);
      break;
    case 2:
      state.I1 -= 1; state.R += 1; death(0);
      break;
    case 3:
      state.I2 -= 1; state.R += 1; death(1);
      break;
    case 4:
      sample(0);
      break;
    case 5:
      sample(1);
      break;
    case 6:
      state.I1 -= 1; state.I2 += 1; migrate(0,1);
      break;
    case 7:
      state.I1 += 1; state.I2 -= 1; migrate(1,0);
      break;
    case 8:
      state.S += 1; state.R -= 1;
      break;
  default:
    err("in %s: c'est impossible! (%ld)",__func__,event);
    break;
  }
}

GENERICS(SIIR,siir_genealogy_t)
