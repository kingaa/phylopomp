// Moran: The classical Moran model (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static int deme = 0;

//! Moran process state.
typedef struct {
  int m;
  int g;
} moran_state_t;

//! Moran process parameters.
typedef struct {
  double mu;
  double psi;
  double dt;
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
    + YAML_PARAM(dt)
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
  PARAM_SET(dt);
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
graft(deme,params.n);
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
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

template<>
size_t moran_proc_t::n_integer_elements() const {
  return 2;  // Number of integer state variables
}

template<>
size_t moran_proc_t::n_double_elements() const {
  return 0;  // Number of double state variables
}

static const char* Moran_int_names[] = {"m", "g"};
static const char* Moran_dbl_names[] = {""};

template<>
const char** moran_proc_t::integer_names() const {
  return Moran_int_names;
}

template<>
const char** moran_proc_t::double_names() const {
  return Moran_dbl_names;
}

template<>
void moran_proc_t::get_state_elements(size_t i, double *time, int *intg, double *dbl) const {
  *time = time_history[i];
  const moran_state_t& s = state_history[i];
    intg[0] = s.m;
  intg[1] = s.g;
  
}

extern "C" {
  SEXP get_states_Moran (SEXP State) {
    moran_genealogy_t x(State);
    return x.get_states();
  }
}

GENERICS(Moran,moran_genealogy_t)
