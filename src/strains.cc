// Strains: Three strains compete for a single susceptible pool. (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static const int strain1 = 1;
static const int strain2 = 2;
static const int strain3 = 3;

//! Strains process state.
typedef struct {
  int S;
  int I1;
  int I2;
  int I3;
  int R;
} strains_state_t;

//! Strains process parameters.
typedef struct {
  double Beta1;
  double Beta2;
  double Beta3;
  double gamma;
  double chi1;
  double chi2;
  double chi3;
  double pop;
  double S_0;
  double I1_0;
  double I2_0;
  double I3_0;
  double R_0;
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
    + YAML_PARAM(chi1)
    + YAML_PARAM(chi2)
    + YAML_PARAM(chi3)
    + YAML_PARAM(pop)
    + YAML_PARAM(S_0)
    + YAML_PARAM(I1_0)
    + YAML_PARAM(I2_0)
    + YAML_PARAM(I3_0)
    + YAML_PARAM(R_0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(I1)
    + YAML_STATE(I2)
    + YAML_STATE(I3)
    + YAML_STATE(R);
  return p+s;
}

template<>
void strains_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta1);
  PARAM_SET(Beta2);
  PARAM_SET(Beta3);
  PARAM_SET(gamma);
  PARAM_SET(chi1);
  PARAM_SET(chi2);
  PARAM_SET(chi3);
  if (m != n) err("wrong number of parameters!");
}

template<>
void strains_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(pop);
  PARAM_SET(S_0);
  PARAM_SET(I1_0);
  PARAM_SET(I2_0);
  PARAM_SET(I3_0);
  PARAM_SET(R_0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double strains_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta1 * state.S * state.I1 / params.pop);
  RATE_CALC(params.Beta2 * state.S * state.I2 / params.pop);
  RATE_CALC(params.Beta3 * state.S * state.I3 / params.pop);
  RATE_CALC(params.gamma * state.I1);
  RATE_CALC(params.gamma * state.I2);
  RATE_CALC(params.gamma * state.I3);
  RATE_CALC(params.chi1 * state.I1);
  RATE_CALC(params.chi2 * state.I2);
  RATE_CALC(params.chi3 * state.I3);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void strains_genealogy_t::rinit (void) {
  double f = params.pop/(params.S_0+params.I1_0+params.I2_0+params.I3_0+params.R_0);
  state.S = nearbyint(f*params.S_0);
  state.I1 = nearbyint(f*params.I1_0);
  state.I2 = nearbyint(f*params.I2_0);
  state.I3 = nearbyint(f*params.I3_0);
  state.R = nearbyint(f*params.R_0);
  graft(strain1,state.I1);
  graft(strain2,state.I2);
  graft(strain3,state.I3);
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
