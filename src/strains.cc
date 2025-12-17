// Strains: Three strains compete for a single susceptible pool. (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static int strain1 = 0;
static int strain2 = 1;
static int strain3 = 2;

//! Strains process state.
typedef struct {
  int S;
  int I1;
  int I2;
  int I3;
  int R;
  double N;
} strains_state_t;

//! Strains process parameters.
typedef struct {
  double Beta1;
  double Beta2;
  double Beta3;
  double gamma;
  double psi1;
  double psi2;
  double psi3;
  int S0;
  int I1_0;
  int I2_0;
  int I3_0;
  int R0;
} strains_parameters_t;

using strains_proc_t = popul_proc_t<strains_state_t,strains_parameters_t,9>;
using strains_genealogy_t = master_t<strains_proc_t,3>;

template<>
std::string strains_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta1)
    + YAML_PARAM(Beta2)
    + YAML_PARAM(Beta3)
    + YAML_PARAM(gamma)
    + YAML_PARAM(psi1)
    + YAML_PARAM(psi2)
    + YAML_PARAM(psi3)
    + YAML_PARAM(S0)
    + YAML_PARAM(I1_0)
    + YAML_PARAM(I2_0)
    + YAML_PARAM(I3_0)
    + YAML_PARAM(R0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(I1)
    + YAML_STATE(I2)
    + YAML_STATE(I3)
    + YAML_STATE(R)
    + YAML_STATE(N);
  return p+s;
}

template<>
void strains_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta1);
  PARAM_SET(Beta2);
  PARAM_SET(Beta3);
  PARAM_SET(gamma);
  PARAM_SET(psi1);
  PARAM_SET(psi2);
  PARAM_SET(psi3);
  if (m != n) err("wrong number of parameters!");
}

template<>
void strains_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(I1_0);
  PARAM_SET(I2_0);
  PARAM_SET(I3_0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double strains_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta1 * state.S * state.I1 / state.N);
  RATE_CALC(params.Beta2 * state.S * state.I2 / state.N);
  RATE_CALC(params.Beta3 * state.S * state.I3 / state.N);
  RATE_CALC(params.gamma * state.I1);
  RATE_CALC(params.gamma * state.I2);
  RATE_CALC(params.gamma * state.I3);
  RATE_CALC(params.psi1 * state.I1);
  RATE_CALC(params.psi2 * state.I2);
  RATE_CALC(params.psi3 * state.I3);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void strains_genealogy_t::rinit (void) {
  state.S = params.S0;
  state.I1 = params.I1_0;
  state.I2 = params.I2_0;
  state.I3 = params.I3_0;
  state.R = params.R0;
  state.N = double(params.S0+params.I1_0+params.I3_0+params.I3_0+params.R0);
  graft(strain1,params.I1_0);
  graft(strain2,params.I2_0);
  graft(strain3,params.I3_0);
}

template<>
void strains_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
    state.S -= 1; state.I1 += 1; birth(strain1,strain1);
    break;
  case 1:
    state.S -= 1; state.I2 += 1; birth(strain2,strain2);
    break;
  case 2:
    state.S -= 1; state.I3 += 1; birth(strain3,strain3);
    break;
  case 3:
    state.I1 -= 1; state.R += 1; death(strain1);
    break;
  case 4:
    state.I2 -= 1; state.R += 1; death(strain2);
    break;
  case 5:
    state.I3 -= 1; state.R += 1; death(strain3);
    break;
  case 6:
    state.I1 -= 1; state.R += 1; sample_death(strain1);
    break;
  case 7:
    state.I2 -= 1; state.R += 1; sample_death(strain2);
    break;
  case 8:
    state.I3 -= 1; state.R += 1; sample_death(strain3);
    break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

GENERICS(Strains,strains_genealogy_t)
