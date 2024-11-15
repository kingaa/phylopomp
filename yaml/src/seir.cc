// SEIR: Classical susceptible-exposed-infected-recovered model (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static int Exposed = 0;
static int Infectious = 1;

//! SEIR process state.
typedef struct {
  int S;
  int E;
  int I;
  int R;
  double N;
} seir_state_t;

//! SEIR process parameters.
typedef struct {
  double Beta;
  double sigma;
  double gamma;
  double psi;
  double omega;
  double dt;
  int S0;
  int E0;
  int I0;
  int R0;
} seir_parameters_t;

using seir_proc_t = popul_proc_t<seir_state_t,seir_parameters_t,5>;
using seir_genealogy_t = master_t<seir_proc_t,2>;

template<>
std::string seir_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta)
    + YAML_PARAM(sigma)
    + YAML_PARAM(gamma)
    + YAML_PARAM(psi)
    + YAML_PARAM(omega)
    + YAML_PARAM(dt)
    + YAML_PARAM(S0)
    + YAML_PARAM(E0)
    + YAML_PARAM(I0)
    + YAML_PARAM(R0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(E)
    + YAML_STATE(I)
    + YAML_STATE(R)
    + YAML_STATE(N);
  return p+s;
}

template<>
void seir_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta);
  PARAM_SET(sigma);
  PARAM_SET(gamma);
  PARAM_SET(psi);
  PARAM_SET(omega);
  PARAM_SET(dt);
  if (m != n) err("wrong number of parameters!");
}

template<>
void seir_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(E0);
  PARAM_SET(I0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double seir_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta * state.S * state.I / state.N);
  RATE_CALC(params.sigma * state.E);
  RATE_CALC(params.gamma * state.I);
  RATE_CALC(params.psi * state.I);
  RATE_CALC(params.omega * state.R);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void seir_genealogy_t::rinit (void) {
  state.S = params.S0;
state.E = params.E0;
state.I = params.I0;
state.R = params.R0;
state.N = double(params.S0+params.E0+params.I0+params.R0);
graft(0,params.E0);
graft(1,params.I0);
}

template<>
void seir_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
      state.S -= 1; state.E += 1; birth(Infectious,Exposed);
      break;
    case 1:
      state.E -= 1; state.I += 1; migrate(Exposed,Infectious);
      break;
    case 2:
      state.I -= 1; state.R += 1; death(Infectious);
      break;
    case 3:
      sample(Infectious);
      break;
    case 4:
      state.R -= 1; state.S += 1;
      break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

template<>
size_t seir_proc_t::n_integer_elements() const {
  return 4;  // Number of integer state variables
}

template<>
size_t seir_proc_t::n_double_elements() const {
  return 1;  // Number of double state variables
}

static const char* SEIR_int_names[] = {"S", "E", "I", "R"};
static const char* SEIR_dbl_names[] = {"N"};

template<>
const char** seir_proc_t::integer_names() const {
  return SEIR_int_names;
}

template<>
const char** seir_proc_t::double_names() const {
  return SEIR_dbl_names;
}

template<>
void seir_proc_t::get_state_elements(size_t i, double *time, int *intg, double *dbl) const {
  *time = time_history[i];
  const seir_state_t& s = state_history[i];
    intg[0] = s.S;
  intg[1] = s.E;
  intg[2] = s.I;
  intg[3] = s.R;
    dbl[0] = s.N;
}

extern "C" {
  SEXP get_states_SEIR (SEXP State) {
    seir_genealogy_t x(State);
    return x.get_states();
  }
}

GENERICS(SEIR,seir_genealogy_t)
