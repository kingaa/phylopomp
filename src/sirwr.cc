// SIR with three segments and reassortment: Small, Medium, Large (C++)
#include "masterwr.h"
#include "popul_proc.h"
#include "genericswr.h"

//! SIR process state.
typedef struct {
  int S;
  int I;
  int R;
  double N;
} sirwr_state_t;

//! SIR with reassortment process parameters.
typedef struct {
  double Beta;
  double gamma;
  double psi;
  double delta;
  double rhoS;
  double rhoM;
  double rhoL;
  int S0;
  int I0;
  int R0;
} sirwr_parameters_t;

using sirwr_proc_t = popul_proc_t<sirwr_state_t,sirwr_parameters_t,7>;
using sirwr_genealogy_t = master_t<sirwr_proc_t,1,3>;

template<>
std::string sirwr_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta)
    + YAML_PARAM(gamma)
    + YAML_PARAM(psi)
    + YAML_PARAM(delta)
    + YAML_PARAM(rhoS)
    + YAML_PARAM(rhoM)
    + YAML_PARAM(rhoL)
    + YAML_PARAM(S0)
    + YAML_PARAM(I0)
    + YAML_PARAM(R0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(I)
    + YAML_STATE(R)
    + YAML_STATE(N);
  return p+s;
}

template<>
void sirwr_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta);
  PARAM_SET(gamma);
  PARAM_SET(psi);
  PARAM_SET(delta);
  PARAM_SET(rhoS);
  PARAM_SET(rhoM);
  PARAM_SET(rhoL);
  if (m != n) err("wrong number of parameters!");
}

template<>
void sirwr_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(I0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double sirwr_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta * state.S * state.I / state.N);
  RATE_CALC(params.gamma * state.I);
  RATE_CALC(params.psi * state.I);
  RATE_CALC(params.delta * state.R);
  RATE_CALC(params.rhoS * state.I);
  RATE_CALC(params.rhoM * state.I);
  RATE_CALC(params.rhoL * state.I);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void sirwr_genealogy_t::rinit (void) {
  state.S = params.S0;
  state.I = params.I0;
  state.R = params.R0;
  state.N = double(params.S0+params.I0+params.R0);
  graft(0,params.I0);
}

template<>
void sirwr_genealogy_t::jump (int event) {
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
    state.R -= 1; state.S += 1;
    break;
  case 4:
    reassort(0,0,0);
    break;
  case 5:
    reassort(0,0,1);
    break;
  case 6:
    reassort(0,0,2);
    break;
  default:
    err("in %s: c'est impossible! (%ld)",__func__,event);
    break;
  }
}

GENERICSWR(SIRwr,sirwr_genealogy_t)
