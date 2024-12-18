// S2I2R2: Two-host infection model with waning, immigration, and demography. (C++)
#include "master.h"
#include "popul_proc.h"
#include "generics.h"
#include "internal.h"

static int host1 = 0;
static int host2 = 1;
static int outside = 2;

//! S2I2R2 process state.
typedef struct {
  int S1;
  int I1;
  int R1;
  int S2;
  int I2;
  int R2;
  double N1;
  double N2;
} s2i2r2_state_t;

//! S2I2R2 process parameters.
typedef struct {
  double Beta11;
  double Beta12;
  double Beta22;
  double gamma1;
  double gamma2;
  double psi1;
  double psi2;
  double omega1;
  double omega2;
  double b1;
  double b2;
  double d1;
  double d2;
  double iota1;
  double iota2;
  int S1_0;
  int S2_0;
  int I1_0;
  int I2_0;
  int R1_0;
  int R2_0;
} s2i2r2_parameters_t;

using s2i2r2_proc_t = popul_proc_t<s2i2r2_state_t,s2i2r2_parameters_t,19>;
using s2i2r2_genealogy_t = master_t<s2i2r2_proc_t,3>;

template<>
std::string s2i2r2_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta11)
    + YAML_PARAM(Beta12)
    + YAML_PARAM(Beta22)
    + YAML_PARAM(gamma1)
    + YAML_PARAM(gamma2)
    + YAML_PARAM(psi1)
    + YAML_PARAM(psi2)
    + YAML_PARAM(omega1)
    + YAML_PARAM(omega2)
    + YAML_PARAM(b1)
    + YAML_PARAM(b2)
    + YAML_PARAM(d1)
    + YAML_PARAM(d2)
    + YAML_PARAM(iota1)
    + YAML_PARAM(iota2)
    + YAML_PARAM(S1_0)
    + YAML_PARAM(S2_0)
    + YAML_PARAM(I1_0)
    + YAML_PARAM(I2_0)
    + YAML_PARAM(R1_0)
    + YAML_PARAM(R2_0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S1)
    + YAML_STATE(I1)
    + YAML_STATE(R1)
    + YAML_STATE(S2)
    + YAML_STATE(I2)
    + YAML_STATE(R2)
    + YAML_STATE(N1)
    + YAML_STATE(N2);
  return p+s;
}

template<>
void s2i2r2_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta11);
  PARAM_SET(Beta12);
  PARAM_SET(Beta22);
  PARAM_SET(gamma1);
  PARAM_SET(gamma2);
  PARAM_SET(psi1);
  PARAM_SET(psi2);
  PARAM_SET(omega1);
  PARAM_SET(omega2);
  PARAM_SET(b1);
  PARAM_SET(b2);
  PARAM_SET(d1);
  PARAM_SET(d2);
  PARAM_SET(iota1);
  PARAM_SET(iota2);
  if (m != n) err("wrong number of parameters!");
}

template<>
void s2i2r2_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S1_0);
  PARAM_SET(S2_0);
  PARAM_SET(I1_0);
  PARAM_SET(I2_0);
  PARAM_SET(R1_0);
  PARAM_SET(R2_0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double s2i2r2_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  RATE_CALC(params.Beta11 * state.I1 / state.N1 * state.S1);
  RATE_CALC(params.Beta22 * state.I2 / state.N2 * state.S2);
  RATE_CALC(params.Beta12 * state.I2 / state.N2 * state.S1);
  RATE_CALC(params.gamma1 * state.I1);
  RATE_CALC(params.gamma2 * state.I2);
  RATE_CALC(params.omega1 * state.R1);
  RATE_CALC(params.omega2 * state.R2);
  RATE_CALC(params.psi1 * state.I1);
  RATE_CALC(params.psi2 * state.I2);
  RATE_CALC(params.iota1 * state.S1);
  RATE_CALC(params.iota2 * state.S2);
  RATE_CALC(params.d1 * state.S1);
  RATE_CALC(params.d2 * state.S2);
  RATE_CALC(params.d1 * state.I1);
  RATE_CALC(params.d2 * state.I2);
  RATE_CALC(params.d1 * state.R1);
  RATE_CALC(params.d2 * state.R2);
  RATE_CALC(params.b1 * state.N1);
  RATE_CALC(params.b2 * state.N2);
  if (m != n) err("wrong number of events!");
  return total;
}

template<>
void s2i2r2_genealogy_t::rinit (void) {
  state.S1 = params.S1_0;
  state.I1 = params.I1_0;
  state.R1 = params.R1_0;
  state.S2 = params.S2_0;
  state.I2 = params.I2_0;
  state.R2 = params.R2_0;
  state.N1 = double(params.S1_0+params.I1_0+params.R1_0);
  state.N2 = double(params.S2_0+params.I2_0+params.R2_0);
  graft(host1,params.I1_0);
  graft(host2,params.I2_0);
}

template<>
void s2i2r2_genealogy_t::jump (int event) {
  switch (event) {
  case 0:
    state.S1 -= 1; state.I1 += 1; birth(host1,host1);
    break;
  case 1:
    state.S2 -= 1; state.I2 += 1; birth(host2,host2);
    break;
  case 2:
    state.S1 -= 1; state.I1 += 1; birth(host2,host1);
    break;
  case 3:
    state.I1 -= 1; state.R1 += 1; death(host1);
    break;
  case 4:
    state.I2 -= 1; state.R2 += 1; death(host2);
    break;
  case 5:
    state.R1 -= 1; state.S1 += 1;
    break;
  case 6:
    state.R2 -= 1; state.S2 += 1;
    break;
  case 7:
    sample(host1);
    break;
  case 8:
    sample(host2);
    break;
  case 9:
    state.S1 -= 1; state.I1 += 1; graft(outside); migrate(outside,host1);
    break;
  case 10:
    state.S2 -= 1; state.I2 += 1; graft(outside); migrate(outside,host2);
    break;
  case 11:
    state.S1 -= 1; state.N1 -= 1;
    break;
  case 12:
    state.S2 -= 1; state.N2 -= 1;
    break;
  case 13:
    state.I1 -= 1; state.N1 -= 1; death(host1);
    break;
  case 14:
    state.I2 -= 1; state.N2 -= 1; death(host2);
    break;
  case 15:
    state.R1 -= 1; state.N1 -= 1;
    break;
  case 16:
    state.R2 -= 1; state.N2 -= 1;
    break;
  case 17:
    state.S1 += 1; state.N1 += 1;
    break;
  case 18:
    state.S2 += 1; state.N2 += 1;
    break;
  default:                      // #nocov
    assert(0);                  // #nocov
    break;                      // #nocov
  }
}

GENERICS(S2I2R2,s2i2r2_genealogy_t)
