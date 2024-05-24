// SI2R: Two-deme model of superspreading (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

//! SI2R process state.
typedef struct {
  int S;
  int I1;
  int I2;
  int R;
  double N;
} si2r_state_t;

//! SI2R process parameters.
typedef struct {
  double Beta;
  double mu;
  double gamma;
  double omega;
  double psi1;
  double psi2;
  double sigma12;
  double sigma21;
  int S0;
  int I0;
  int R0;
} si2r_parameters_t;

using si2r_proc_t = popul_proc_t<si2r_state_t,si2r_parameters_t,9>;
using si2r_genealogy_t = master_t<si2r_proc_t,2>;

template<>
std::string si2r_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta)
    + YAML_PARAM(mu)
    + YAML_PARAM(gamma)
    + YAML_PARAM(omega)
    + YAML_PARAM(psi1)
    + YAML_PARAM(psi2)
    + YAML_PARAM(sigma12)
    + YAML_PARAM(sigma21)
    + YAML_PARAM(S0)
    + YAML_PARAM(I0)
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
void si2r_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta);
  PARAM_SET(mu);
  PARAM_SET(gamma);
  PARAM_SET(omega);
  PARAM_SET(psi1);
  PARAM_SET(psi2);
  PARAM_SET(sigma12);
  PARAM_SET(sigma21);
  if (m != n) err("wrong number of parameters!");
}

template<>
void si2r_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(I0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double si2r_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta * state.S * state.I1 / state.N);
  RATE_CALC(params.Beta * state.S * state.I2 / state.N);
  RATE_CALC(params.gamma * state.I1);
  RATE_CALC(params.gamma * state.I2);
  RATE_CALC(params.psi1 * state.I1);
  RATE_CALC(params.psi2 * state.I2);
  RATE_CALC(params.sigma12 * state.I1);
  RATE_CALC(params.sigma21 * state.I2);
  RATE_CALC(params.omega * state.R);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void si2r_genealogy_t::rinit (void) {
  state.S = params.S0;
state.I1 = params.I0;
state.I2 = 0;
state.R = params.R0;
state.N = double(params.S0+params.I0+params.R0);
graft(0,params.I0);
}

template<>
void si2r_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
      state.S -= 1; state.I1 += 1; birth(0,0);
      break;
    case 1:
      {
  int n = 1+int(rgeom(1.0/params.mu));
  if (state.S >= n) {
    state.S -= n; state.I1 += n; birth(1,0,n);
  } else {
    birth(1,0,state.S); state.I1 += state.S; state.S = 0;
  }
}
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
      state.R -= 1; state.S += 1;
      break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

GENERICS(SI2R,si2r_genealogy_t)
