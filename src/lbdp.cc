// LBDP: Linear birth-death-sampling model (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static const int deme = 1;

//! LBDP process state.
typedef struct {
  int n;
  int nsample;
} lbdp_state_t;

//! LBDP process parameters.
typedef struct {
  double lambda;
  double mu;
  double psi;
  double chi;
  int n0;
  int max_sample;
} lbdp_parameters_t;

using lbdp_proc_t = popul_proc_t<lbdp_state_t,lbdp_parameters_t,4>;
using lbdp_genealogy_t = master_t<lbdp_proc_t,1>;

template<>
std::string lbdp_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(lambda)
    + YAML_PARAM(mu)
    + YAML_PARAM(psi)
    + YAML_PARAM(chi)
    + YAML_PARAM(n0)
    + YAML_PARAM(max_sample);
  std::string s = tab + "state:\n"
    + YAML_STATE(n)
    + YAML_STATE(nsample);
  return p+s;
}

template<>
void lbdp_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(lambda);
  PARAM_SET(mu);
  PARAM_SET(psi);
  PARAM_SET(chi);
  if (m != n) err("wrong number of parameters!");
}

template<>
void lbdp_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(n0);
  PARAM_SET(max_sample);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double lbdp_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC((state.nsample < params.max_sample) ? params.lambda * state.n : 0.0);
  RATE_CALC((state.nsample < params.max_sample) ? params.mu * state.n : 0.0);
  RATE_CALC((state.nsample < params.max_sample) ? params.chi * state.n: 0.0);
  RATE_CALC((state.nsample < params.max_sample) ? params.psi * state.n : 0.0);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void lbdp_genealogy_t::rinit (void) {
  state.n = params.n0;
  state.nsample = 0;
  graft(deme,params.n0);
}

template<>
void lbdp_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
    state.n += 1; birth();
    break;
  case 1:
    state.n -= 1; death();
    break;
  case 2:
    state.n -= 1; state.nsample++; sample_death();
    break;
  case 3:
    state.nsample++; sample();
    break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

GENERICS(LBDP,lbdp_genealogy_t)
