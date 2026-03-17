// sei2rwr: two-class SEIRS model with reassortment (C++)
// two segments: HA and NA for influenza
#include "master.h"
#include "popul_proc.h"
#include "generics.h"

static const int Eordinary = 0;
static const int Iordinary = 1;
static const int Ehigh = 2;
static const int Ihigh = 3;

//! SEI2R state.
typedef struct {
  int S;
  int E1;     // ordinary exposed
  int E2;     // superspreading exposed
  int I1;     // ordinary infectious
  int I2;     // superspreading infectious
  int R;
  int N;
} sei2rwr_state_t;

//! SEI2R with reassortment parameters.
typedef struct {
  double Beta0;   // ordinary transmission rate
  double sigma;   // latency
  double gamma;   // recovery
  double psi;     // sampling rate
  double omega;   // immunity decay
  double rhoA;    // reassortment rate in Seg A
  double rhoB;    // reassortment rate in Seg B
  double pr12;    // probability of migration I1 -> I2 when reassortment
  double pr21;    // probability of migration I2 -> I1 when reassortment
  double rr;      // Beta1 = rr * Beta0; [1,]
  double p;       // probability of sample termination
  int S0;
  int E0;         // initial ordinary exposed
  int I0;         // initial ordinary infectious
  int R0;
} sei2rwr_parameters_t;

// 4 demes: E1(0), I1(1), E2(2), I2(3)
// 9 base events + up to 8 reassortment events = 17 max
using sei2rwr_proc_t = popul_proc_t<sei2rwr_state_t,sei2rwr_parameters_t,17,4>;
using sei2rwr_genealogy_t = master_t<sei2rwr_proc_t,4,2>;

template<>
std::string sei2rwr_proc_t::yaml (std::string tab) const {
  std::string t = tab + "  ";
  std::string p = tab + "parameter:\n"
    + YAML_PARAM(Beta0)
    + YAML_PARAM(sigma)
    + YAML_PARAM(gamma)
    + YAML_PARAM(psi)
    + YAML_PARAM(omega)
    + YAML_PARAM(rhoA)
    + YAML_PARAM(rhoB)
    + YAML_PARAM(pr12)
    + YAML_PARAM(pr21)
    + YAML_PARAM(rr)
    + YAML_PARAM(p)
    + YAML_PARAM(S0)
    + YAML_PARAM(E0)
    + YAML_PARAM(I0)
    + YAML_PARAM(R0);
  std::string s = tab + "state:\n"
    + YAML_STATE(S)
    + YAML_STATE(E1)
    + YAML_STATE(E2)
    + YAML_STATE(I1)
    + YAML_STATE(I2)
    + YAML_STATE(R)
    + YAML_STATE(N);
  return p+s;
}

template<>
void sei2rwr_proc_t::update_params (double *p, int n) {
  int m = 0;
  PARAM_SET(Beta0);
  PARAM_SET(sigma);
  PARAM_SET(gamma);
  PARAM_SET(psi);
  PARAM_SET(omega);
  PARAM_SET(rhoA);
  PARAM_SET(rhoB);
  PARAM_SET(pr12);
  PARAM_SET(pr21);
  PARAM_SET(rr);
  PARAM_SET(p);
  if (m != n) err("wrong number of parameters!");
}

template<>
void sei2rwr_proc_t::update_IVPs (double *p, int n) {
  int m = 0;
  PARAM_SET(S0);
  PARAM_SET(E0);
  PARAM_SET(I0);
  PARAM_SET(R0);
  if (m != n) err("wrong number of initial-value parameters!");
}

template<>
double sei2rwr_proc_t::event_rates (double *rate, int n) const {
  int m = 0;
  double total = 0;
  // Transmission events (use double cast to avoid integer division)
  double dN = (state.N > 0) ? (double)state.N : 1.0;
  RATE_CALC(params.Beta0 * (double)state.S * (double)state.I1 / dN); // 0: S -> E1 by I1
  RATE_CALC(params.Beta0 * params.rr * (double)state.S * (double)state.I2 / dN); // 1: S -> E2 by I2
  // Progression
  RATE_CALC(params.sigma * state.E1);                                // 2: E1 -> I1
  RATE_CALC(params.sigma * state.E2);                                // 3: E2 -> I2
  // Recovery
  RATE_CALC(params.gamma * state.I1);                                // 4: I1 -> R
  RATE_CALC(params.gamma * state.I2);                                // 5: I2 -> R
  // Sampling
  RATE_CALC(params.psi * state.I1);                                  // 6: sample I1
  RATE_CALC(params.psi * state.I2);                                  // 7: sample I2
  // Waning immunity
  RATE_CALC(params.omega * state.R);                                  // 8: R -> S
  // Reassortment events (only when exposed individuals exist)
  // E1 reassortment
  if (state.E1 > 0 && state.I1 > 0) {
    RATE_CALC(params.rhoA * (1 - params.pr12) * state.I1);          // 9: seg A re, E1 stays E1
    RATE_CALC(params.rhoA * params.pr12 * state.I1);                // 10: seg A re, E1 -> E2
    RATE_CALC(params.rhoB * (1 - params.pr12) * state.I1);          // 11: seg B re, E1 stays E1
    RATE_CALC(params.rhoB * params.pr12 * state.I1);                // 12: seg B re, E1 -> E2
  } else {
    for (int i = 0; i < 4; i++) rate[m++] = 0.0;
  }
  // E2 reassortment
  if (state.E2 > 0 && state.I2 > 0) {
    RATE_CALC(params.rhoA * (1 - params.pr21) * state.I2);          // 13: seg A re, E2 stays E2
    RATE_CALC(params.rhoA * params.pr21 * state.I2);                // 14: seg A re, E2 -> E1
    RATE_CALC(params.rhoB * (1 - params.pr21) * state.I2);          // 15: seg B re, E2 stays E2
    RATE_CALC(params.rhoB * params.pr21 * state.I2);                // 16: seg B re, E2 -> E1
  } else {
    for (int i = 0; i < 4; i++) rate[m++] = 0.0;
  }
  if (m != n) err("wrong number of events! (m=%d, n=%d)",m,n);
  return total;
}

template<>
void sei2rwr_genealogy_t::rinit (void) {
  state.S = params.S0;
  state.E1 = params.E0;
  state.E2 = 0;
  state.I1 = params.I0;
  state.I2 = 0;
  state.R = params.R0;
  state.N = params.S0+params.E0+params.I0+params.R0;
  graft(Eordinary,params.E0);
  graft(Iordinary,params.I0);
}

template<>
void sei2rwr_genealogy_t::jump (int event) {
  name_t seg[1];
  switch (event) {
  case 0:                         // S -> E1 by I1
    state.S -= 1; state.E1 += 1; birth(Iordinary,Eordinary);
    break;
  case 1:                         // S -> E2 by I2
    state.S -= 1; state.E2 += 1; birth(Ihigh,Ehigh);
    break;
  case 2:                         // E1 -> I1
    state.E1 -= 1; state.I1 += 1; migrate(Eordinary, Iordinary);
    break;
  case 3:                         // E2 -> I2
    state.E2 -= 1; state.I2 += 1; migrate(Ehigh, Ihigh);
    break;
  case 4:                         // I1 -> R
    state.I1 -= 1; state.R += 1; death(Iordinary);
    break;
  case 5:                         // I2 -> R
    state.I2 -= 1; state.R += 1; death(Ihigh);
    break;
  case 6:                         // sample I1
    if (sample(Iordinary, params.p)) {
      state.I1 -= 1; state.R += 1;
    }
    break;
  case 7:                         // sample I2
    if (sample(Ihigh, params.p)) {
      state.I2 -= 1; state.R += 1;
    }
    break;
  case 8:                         // R -> S
    state.R -= 1; state.S += 1;
    break;
  case 9:                         // seg A reassort, E1 stays E1
    seg[0] = 0UL;
    reassort(Eordinary,Iordinary,seg,1);
    break;
  case 10:                        // seg A reassort, E1 -> E2
    seg[0] = 0UL;
    state.E1 -= 1; state.E2 += 1;
    reassort(Eordinary,Iordinary,seg,1,Ehigh,true);
    break;
  case 11:                        // seg B reassort, E1 stays E1
    seg[0] = 1UL;
    reassort(Eordinary,Iordinary,seg,1);
    break;
  case 12:                        // seg B reassort, E1 -> E2
    seg[0] = 1UL;
    state.E1 -= 1; state.E2 += 1;
    reassort(Eordinary,Iordinary,seg,1,Ehigh,true);
    break;
  case 13:                        // seg A reassort, E2 stays E2
    seg[0] = 0UL;
    reassort(Ehigh,Ihigh,seg,1);
    break;
  case 14:                        // seg A reassort, E2 -> E1
    seg[0] = 0UL;
    state.E2 -= 1; state.E1 += 1;
    reassort(Ehigh,Ihigh,seg,1,Eordinary,true);
    break;
  case 15:                        // seg B reassort, E2 stays E2
    seg[0] = 1UL;
    reassort(Ehigh,Ihigh,seg,1);
    break;
  case 16:                        // seg B reassort, E2 -> E1
    seg[0] = 1UL;
    state.E2 -= 1; state.E1 += 1;
    reassort(Ehigh,Ihigh,seg,1,Eordinary,true);
    break;
  default:
    err("in %s: c'est impossible! (%d)",__func__,event);
    break;
  }
}

GENERICS(SEI2Rwr,sei2rwr_genealogy_t)
