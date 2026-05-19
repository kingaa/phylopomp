// MERS: Two-host infection model with spillover and demography. Hosts are culled upon sampling. (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static const int camel = 1;
static const int human = 2;

//! MERS process state.
typedef struct {
  int Sc;
  int Ic;
  int Sh;
  int Ih;
} mers_state_t;

//! MERS process parameters.
typedef struct {
  double Beta_cc;
  double Beta_ch;
  double Beta_hc;
  double Beta_hh;
  double gamma_c;
  double gamma_h;
  double chi_c;
  double chi_h;
  double Bc;
  double Bh;
  double Sc0;
  double Sh0;
  double Ic0;
  double Ih0;
  double Nc;
  double Nh;
} mers_parameters_t;

using mers_proc_t = popul_proc_t<mers_state_t,mers_parameters_t,12>;
using mers_genealogy_t = master_t<mers_proc_t,2>;

template<>
std::string mers_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta_cc)
    + YAML_PARAM(Beta_ch)
    + YAML_PARAM(Beta_hc)
    + YAML_PARAM(Beta_hh)
    + YAML_PARAM(gamma_c)
    + YAML_PARAM(gamma_h)
    + YAML_PARAM(chi_c)
    + YAML_PARAM(chi_h)
    + YAML_PARAM(Bc)
    + YAML_PARAM(Bh)
    + YAML_PARAM(Sc0)
    + YAML_PARAM(Sh0)
    + YAML_PARAM(Ic0)
    + YAML_PARAM(Ih0)
    + YAML_PARAM(Nc)
    + YAML_PARAM(Nh);
  std::string s = tab + "state:\n"
    + YAML_STATE(Sc)
    + YAML_STATE(Ic)
    + YAML_STATE(Sh)
    + YAML_STATE(Ih);
  return p+s;
}

template<>
void mers_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta_cc);
  PARAM_SET(Beta_ch);
  PARAM_SET(Beta_hc);
  PARAM_SET(Beta_hh);
  PARAM_SET(gamma_c);
  PARAM_SET(gamma_h);
  PARAM_SET(chi_c);
  PARAM_SET(chi_h);
  PARAM_SET(Bc);
  PARAM_SET(Bh);
  if (m != n) err("wrong number of parameters!");
}

template<>
void mers_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(Sc0);
  PARAM_SET(Sh0);
  PARAM_SET(Ic0);
  PARAM_SET(Ih0);
  PARAM_SET(Nc);
  PARAM_SET(Nh);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double mers_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta_cc * state.Sc * state.Ic / params.Nc);
  RATE_CALC(params.Beta_hh * state.Sh * state.Ih / params.Nh);
  RATE_CALC(params.Beta_ch * state.Sc * state.Ih / params.Nh);
  RATE_CALC(params.Beta_hc * state.Sh * state.Ic / params.Nc);
  RATE_CALC(params.gamma_c * state.Ic);
  RATE_CALC(params.gamma_h * state.Ih);
  RATE_CALC(params.chi_c * state.Ic);
  RATE_CALC(params.chi_h * state.Ih);
  RATE_CALC(params.Bc);
  RATE_CALC(params.Bh);
  RATE_CALC(params.Bc);
  RATE_CALC(params.Bh/params.Nh * state.Sh);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void mers_genealogy_t::rinit (void) {
  double fc = params.Nc/(params.Sc0+params.Ic0);
  double fh = params.Nh/(params.Sh0+params.Ih0);
  state.Sc = nearbyint(fc*params.Sc0);
  state.Ic = nearbyint(fc*params.Ic0);
  state.Sh = nearbyint(fh*params.Sh0);
  state.Ih = nearbyint(fh*params.Ih0);
  graft(camel,state.Ic);
  graft(human,state.Ih);
}

template<>
void mers_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
    state.Sc -= 1; state.Ic += 1; birth(camel,camel);
    break;
  case 1:
    state.Sh -= 1; state.Ih += 1; birth(human,human);
    break;
  case 2:
    state.Sc -= 1; state.Ic += 1; birth(human,camel);
    break;
  case 3:
    state.Sh -= 1; state.Ih += 1; birth(camel,human);
    break;
  case 4:
    state.Ic -= 1; death(camel);
    break;
  case 5:
    state.Ih -= 1; death(human);
    break;
  case 6:
    state.Ic -= 1; sample_death(camel);
    break;
  case 7:
    state.Ih -= 1; sample_death(human);
    break;
  case 8:
    state.Sc += 1;
    break;
  case 9:
    state.Sh += 1;
    break;
  case 10:
    if (state.Sc > 0) state.Sc -= 1;
    break;
  case 11:
    if (state.Sh > 0) state.Sh -= 1;
    break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

GENERICS(MERS,mers_genealogy_t)
