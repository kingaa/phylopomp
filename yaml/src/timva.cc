// TIMVA: Within-host viral dynamic model with ALT (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static int virus = 0;
static int infected = 1;

//! TIMVA process state.
typedef struct {
  int T;
  int I;
  int M;
  double V;
  double A;
} timva_state_t;

//! TIMVA process parameters.
typedef struct {
  double lambda;
  double d;
  double Beta;
  double delta;
  double p;
  double a;
  double kappa;
  double c;
  double mu;
  double omega;
  double s;
  double sigma;
  double psi;
  double f;
  double dt;
  int T0;
  int I0;
  int M0;
  double V0;
  double A0;
} timva_parameters_t;

using timva_proc_t = popul_proc_t<timva_state_t,timva_parameters_t,9>;
using timva_genealogy_t = master_t<timva_proc_t,2>;

template<>
std::string timva_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(lambda)
    + YAML_PARAM(d)
    + YAML_PARAM(Beta)
    + YAML_PARAM(delta)
    + YAML_PARAM(p)
    + YAML_PARAM(a)
    + YAML_PARAM(kappa)
    + YAML_PARAM(c)
    + YAML_PARAM(mu)
    + YAML_PARAM(omega)
    + YAML_PARAM(s)
    + YAML_PARAM(sigma)
    + YAML_PARAM(psi)
    + YAML_PARAM(f)
    + YAML_PARAM(dt)
    + YAML_PARAM(T0)
    + YAML_PARAM(I0)
    + YAML_PARAM(M0)
    + YAML_PARAM(V0)
    + YAML_PARAM(A0);
  std::string s = tab + "state:\n"
    + YAML_STATE(T)
    + YAML_STATE(I)
    + YAML_STATE(M)
    + YAML_STATE(V)
    + YAML_STATE(A);
  return p+s;
}

template<>
void timva_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(lambda);
  PARAM_SET(d);
  PARAM_SET(Beta);
  PARAM_SET(delta);
  PARAM_SET(p);
  PARAM_SET(a);
  PARAM_SET(kappa);
  PARAM_SET(c);
  PARAM_SET(mu);
  PARAM_SET(omega);
  PARAM_SET(s);
  PARAM_SET(sigma);
  PARAM_SET(psi);
  PARAM_SET(f);
  PARAM_SET(dt);
  if (m != n) err("wrong number of parameters!");
}

template<>
void timva_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(T0);
  PARAM_SET(I0);
  PARAM_SET(M0);
  PARAM_SET(V0);
  PARAM_SET(A0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double timva_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta * state.T * state.V);
  RATE_CALC(params.delta * state.I + params.kappa * state.I * state.M);
  RATE_CALC(params.p * state.I);
  RATE_CALC(params.c * state.V);
  RATE_CALC(params.a * state.I);
  RATE_CALC(params.mu * state.M);
  RATE_CALC(params.s);
  RATE_CALC(params.sigma * state.A);
  RATE_CALC(params.psi * state.V);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void timva_genealogy_t::rinit (void) {
  state.T = params.T0;
state.I = params.I0;
state.M = params.M0;
state.V = params.V0;
state.A = params.A0;
graft(virus,params.V0);
}

template<>
void timva_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
      state.V -= 1; state.I += 1; migrate(virus,infected);
      break;
    case 1:
      state.I -= 1; state.A += params.omega;
      break;
    case 2:
      {
  state.V += 1;
  birth(infected,virus);
}
      break;
    case 3:
      state.V -= 1;
      break;
    case 4:
      state.M += 1;
      break;
    case 5:
      state.M -= 1;
      break;
    case 6:
      state.A += 1;
      break;
    case 7:
      state.A -= 1;
      break;
    case 8:
      {
  double n = rbinom(state.V,params.f);
  sample_death(virus, (int)n);
  state.V -= (int)n;
}
      break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

template<>
size_t timva_proc_t::n_integer_elements() const {
  return 3;  // Number of integer state variables
}

template<>
size_t timva_proc_t::n_double_elements() const {
  return 2;  // Number of double state variables
}

static const char* TIMVA_int_names[] = {"T", "I", "M"};
static const char* TIMVA_dbl_names[] = {"V", "A"};

template<>
const char** timva_proc_t::integer_names() const {
  return TIMVA_int_names;
}

template<>
const char** timva_proc_t::double_names() const {
  return TIMVA_dbl_names;
}

template<>
void timva_proc_t::get_state_elements(size_t i, double *time, int *intg, double *dbl) const {
  *time = time_history[i];
  const timva_state_t& s = state_history[i];
    intg[0] = s.T;
  intg[1] = s.I;
  intg[2] = s.M;
    dbl[0] = s.V;
  dbl[1] = s.A;
}

extern "C" {
  SEXP get_states_TIMVA (SEXP State) {
    timva_genealogy_t x(State);
    return x.get_states();
  }
}

GENERICS(TIMVA,timva_genealogy_t)
