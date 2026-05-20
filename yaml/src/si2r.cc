// SI2R: Two-deme model of superspreading (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static const int L = 1;
static const int H = 2;

//! SI2R process state.
typedef struct {
  int S;
  int IL;
  int IH;
  int R;
} si2r_state_t;

//! SI2R process parameters.
typedef struct {
  double Beta;
  double kappa;
  double gamma;
  double omega;
  double chi;
  double etaL;
  double etaH;
  double pop;
  double S0;
  double IL0;
  double IH0;
  double R0;
} si2r_parameters_t;

using si2r_proc_t = popul_proc_t<si2r_state_t,si2r_parameters_t,9>;
using si2r_genealogy_t = master_t<si2r_proc_t,2>;

template<>
std::string si2r_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta)
    + YAML_PARAM(kappa)
    + YAML_PARAM(gamma)
    + YAML_PARAM(omega)
    + YAML_PARAM(chi)
    + YAML_PARAM(etaL)
    + YAML_PARAM(etaH)
    + YAML_PARAM(pop)
    + YAML_PARAM(S0)
    + YAML_PARAM(IL0)
    + YAML_PARAM(IH0)
    + YAML_PARAM(R0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(IL)
    + YAML_STATE(IH)
    + YAML_STATE(R);
  return p+s;
}

template<>
void si2r_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta);
  PARAM_SET(kappa);
  PARAM_SET(gamma);
  PARAM_SET(omega);
  PARAM_SET(chi);
  PARAM_SET(etaL);
  PARAM_SET(etaH);
  if (m != n) err("wrong number of parameters!");
}

template<>
void si2r_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(pop);
  PARAM_SET(S0);
  PARAM_SET(IL0);
  PARAM_SET(IH0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double si2r_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta * state.S * state.IL / params.pop);
  RATE_CALC(params.kappa * params.Beta * state.S * state.IH / params.pop);
  RATE_CALC(params.etaL * state.IL);
  RATE_CALC(params.etaH * state.IH);
  RATE_CALC(params.gamma * state.IL);
  RATE_CALC(params.gamma * state.IH);
  RATE_CALC(params.omega * state.R);
  RATE_CALC(params.chi * state.IL);
  RATE_CALC(params.chi * state.IH);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void si2r_genealogy_t::rinit (void) {
  double f = params.pop/(params.S0+params.IL0+params.IH0+params.R0);
  state.S = nearbyint(f*params.S0);
  state.IL = nearbyint(f*params.IL0);
  state.IH = nearbyint(f*params.IH0);
  state.R = nearbyint(f*params.R0);
  graft(L,state.IL);
  graft(H,state.IH);
}

template<>
void si2r_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
    state.S -= 1; state.IL += 1; birth(L,L);
    break;
  case 1:
    state.S -= 1; state.IL += 1; birth(H,L);
    break;
  case 2:
    state.IL -= 1; state.IH += 1; migrate(L,H);
    break;
  case 3:
    state.IL += 1; state.IH -= 1; migrate(H,L);
    break;
  case 4:
    state.IL -= 1; state.R += 1; death(L);
    break;
  case 5:
    state.IH -= 1; state.R += 1; death(H);
    break;
  case 6:
    state.R -= 1; state.S += 1;
    break;
  case 7:
    state.IL -= 1; sample_death(L);
    break;
  case 8:
    state.IH -= 1; sample_death(H);
    break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

GENERICS(SI2R,si2r_genealogy_t)
