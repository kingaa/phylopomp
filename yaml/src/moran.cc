// Moran: The classical Moran model (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
  int m;
  int g;
} moran_state_t;

typedef struct {
  double mu;
  double psi;
  int n;
} moran_parameters_t;

using moran_proc_t = popul_proc_t<moran_state_t,moran_parameters_t,2>;
using moran_genealogy_t = master_t<moran_proc_t,1>;

template<>
std::string moran_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(mu)
    + YAML_PARAM(psi)
    + YAML_PARAM(n);
  std::string s = tab + "state:\n"
    + YAML_STATE(m)
    + YAML_STATE(g);
  return p+s;
}

template<>
void moran_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(mu);
  PARAM_SET(psi);
  if (m != n) err("wrong number of parameters!");
}

template<>
void moran_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(n);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double moran_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.mu * params.n);
  RATE_CALC(params.psi * params.n);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void moran_genealogy_t::rinit (void) {
  state.m = state.g = 0;
graft(0,params.n);
}

template<>
void moran_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
      state.m += 1; birth(); death();
      break;
    case 1:
      state.g += 1; sample();
      break;
  default:
    err("in %s: c'est impossible! (%ld)",__func__,event);
    break;
  }
}

GENERICS(Moran,moran_genealogy_t)
