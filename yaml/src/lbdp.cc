// LBDP: Linear birth-death-sampling model (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static int deme = 0;

//! LBDP process state.
typedef struct {
  int n;
} lbdp_state_t;

//! LBDP process parameters.
typedef struct {
  double lambda;
  double mu;
  double psi;
  double dt;
  int n0;
} lbdp_parameters_t;

using lbdp_proc_t = popul_proc_t<lbdp_state_t,lbdp_parameters_t,3>;
using lbdp_genealogy_t = master_t<lbdp_proc_t,1>;

template<>
std::string lbdp_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(lambda)
    + YAML_PARAM(mu)
    + YAML_PARAM(psi)
    + YAML_PARAM(dt)
    + YAML_PARAM(n0);
  std::string s = tab + "state:\n"
    + YAML_STATE(n);
  return p+s;
}

template<>
void lbdp_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(lambda);
  PARAM_SET(mu);
  PARAM_SET(psi);
  PARAM_SET(dt);
  if (m != n) err("wrong number of parameters!");
}

template<>
void lbdp_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(n0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double lbdp_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.lambda * state.n);
  RATE_CALC(params.mu * state.n);
  RATE_CALC(params.psi * state.n);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void lbdp_genealogy_t::rinit (void) {
  state.n = params.n0;
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
      sample();
      break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

template<>
size_t lbdp_proc_t::n_integer_elements() const {
  return 1;  // Number of integer state variables
}

template<>
size_t lbdp_proc_t::n_double_elements() const {
  return 0;  // Number of double state variables
}

static const char* LBDP_int_names[] = {"n"};
static const char* LBDP_dbl_names[] = {""};

template<>
const char** lbdp_proc_t::integer_names() const {
  return LBDP_int_names;
}

template<>
const char** lbdp_proc_t::double_names() const {
  return LBDP_dbl_names;
}

template<>
void lbdp_proc_t::get_state_elements(size_t i, double *time, int *intg, double *dbl) const {
  *time = time_history[i];
  const lbdp_state_t& s = state_history[i];
    intg[0] = s.n;
  
}

extern "C" {
  SEXP get_states_LBDP (SEXP State) {
    lbdp_genealogy_t x(State);
    return x.get_states();
  }
}

GENERICS(LBDP,lbdp_genealogy_t)
