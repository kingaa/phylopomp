// SIR: Classical susceptible-infected-recovered model (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static int Infected = 0;

//! SIR process state.
typedef struct {
  int S;
  int I;
  int R;
  double N;
} sir_state_t;

//! SIR process parameters.
typedef struct {
  double Beta;
  double gamma;
  double psi;
  double omega;
  double dt;
  int S0;
  int I0;
  int R0;
} sir_parameters_t;

using sir_proc_t = popul_proc_t<sir_state_t,sir_parameters_t,4>;
using sir_genealogy_t = master_t<sir_proc_t,1>;

template<>
std::string sir_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta)
    + YAML_PARAM(gamma)
    + YAML_PARAM(psi)
    + YAML_PARAM(omega)
    + YAML_PARAM(dt)
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
void sir_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta);
  PARAM_SET(gamma);
  PARAM_SET(psi);
  PARAM_SET(omega);
  PARAM_SET(dt);
  if (m != n) err("wrong number of parameters!");
}

template<>
void sir_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(I0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double sir_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta * state.S * state.I / state.N);
  RATE_CALC(params.gamma * state.I);
  RATE_CALC(params.psi * state.I);
  RATE_CALC(params.omega * state.R);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void sir_genealogy_t::rinit (void) {
  state.S = params.S0;
state.I = params.I0;
state.R = params.R0;
state.N = double(params.S0+params.I0+params.R0);
graft(Infected,params.I0);
}

template<>
void sir_genealogy_t::jump (int event) {
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
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

template<>
size_t sir_proc_t::n_integer_elements() const {
  return 3;  // Number of integer state variables
}

template<>
size_t sir_proc_t::n_double_elements() const {
  return 1;  // Number of double state variables
}

static const char* SIR_int_names[] = {"S", "I", "R"};
static const char* SIR_dbl_names[] = {"N"};

template<>
const char** sir_proc_t::integer_names() const {
  return SIR_int_names;
}

template<>
const char** sir_proc_t::double_names() const {
  return SIR_dbl_names;
}

template<>
void sir_proc_t::get_state_elements(size_t i, double *time, int *intg, double *dbl) const {
  *time = time_history[i];
  const sir_state_t& s = state_history[i];
    intg[0] = s.S;
  intg[1] = s.I;
  intg[2] = s.R;
    dbl[0] = s.N;
}

extern "C" {
  SEXP get_states_SIR (SEXP State) {
    sir_genealogy_t x(State);
    return x.get_states();
  }
}

GENERICS(SIR,sir_genealogy_t)
