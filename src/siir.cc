// SIIR with Sampling Genealogy Process Simulator (C++)

#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
  int S;			// number of susceptibles
  int I1, I2;			// number of infections
  int R;			// number of recovereds
} siir_state_t;

typedef struct {
  double Beta1, Beta2;        // transmission rate
  double gamma;               // recovery rate
  double psi1, psi2;          // sampling rates
  double sigma12, sigma21;    // movement rates
  double N;                   // host population size
  int S0;                     // initial susceptibles
  int I1_0, I2_0;             // initial infecteds
  int R0;                     // initial recoveries
} siir_parameters_t;

class siir_genealogy_t : public master_t<popul_proc_t<siir_state_t,siir_parameters_t,8>, 2> {

public:

  // basic constructor
  siir_genealogy_t (double t0 = 0) : master_t(t0) { };
  // constructor from serialized binary form
  siir_genealogy_t (raw_t *o) : master_t(o) {};
  // copy constructor
  siir_genealogy_t (const siir_genealogy_t &G) : master_t(G) {};

  void valid (void) {
    master_t::valid();
    if (params.N <= 0) err("total population size must be positive!");
    if (state.S < 0 || state.I1 < 0 || state.I2 < 0 || state.R < 0) err("negative state variables!");
    if (params.N != state.S+state.I1+state.I2+state.R) err("population leakage!");
  };

  void update_params (double *p, int n) {
    int m = 0;
    PARAM_SET(Beta1);
    PARAM_SET(Beta2);
    PARAM_SET(gamma);
    PARAM_SET(psi1);
    PARAM_SET(psi2);
    PARAM_SET(sigma12);
    PARAM_SET(sigma21);
    if (m != n) err("wrong number of parameters!");
  };

  void update_ICs (double *p, int n) {
    int m = 0;
    PARAM_SET(S0);
    PARAM_SET(I1_0);
    PARAM_SET(I2_0);
    PARAM_SET(R0);
    params.N = double(params.S0+params.I1_0+params.I2_0+params.R0);
    if (m != n) err("wrong number of initial conditions!");
  };

  void rinit (void) {
    state.S = params.S0;
    state.I1 = params.I1_0;
    state.I2 = params.I2_0;
    state.R = params.R0;
    graft(0,params.I1_0);
    graft(1,params.I2_0);
  };

  double event_rates (double *rate, int n) const {
    int m = 0;
    double total = 0;
    RATE_CALC(params.Beta1 * state.S * state.I1 / params.N);
    RATE_CALC(params.Beta2 * state.S * state.I2 / params.N);
    RATE_CALC(params.gamma * state.I1);
    RATE_CALC(params.gamma * state.I2);
    RATE_CALC(params.psi1 * state.I1);
    RATE_CALC(params.psi2 * state.I2);
    RATE_CALC(params.sigma12 * state.I1);
    RATE_CALC(params.sigma21 * state.I2);
    if (m != n) err("wrong number of events!");
    return total;
  };

  void jump (int event) {
    switch (event) {
    case 0:
      state.S -= 1;
      state.I1 += 1;
      birth(0,0);
      break;
    case 1:
      state.S -= 1;
      state.I2 += 1;
      birth(1,1);
      break;
    case 2:
      state.I1 -= 1;
      state.R += 1;
      death(0);
      break;
    case 3:
      state.I2 -= 1;
      state.R += 1;
      death(1);
      break;
    case 4:                     // sample from deme 1
      sample(0);
      break;
    case 5:                     // sample from deme 2
      sample(1);
      break;
    case 6:                     // move from deme 1 -> 2
      state.I1 -= 1;
      state.I2 += 1;
      migrate(0,1);
      break;
    case 7:                     // move from deme 2 -> 1
      state.I1 += 1;
      state.I2 -= 1;
      migrate(1,0);
      break;
    default:
      err("in SIIR 'jump': c'est impossible! (%ld)",event); // # nocov
      break;
    }
  };

  // human-readable info
  std::string yaml (std::string tab = "") const {
    std::string t = tab + "  ";
    std::string p = tab + "parameter:\n"
      + t + "Beta1: " + std::to_string(params.Beta1) + "\n"
      + t + "Beta2: " + std::to_string(params.Beta2) + "\n"
      + t + "gamma: " + std::to_string(params.gamma) + "\n"
      + t + "psi1: " + std::to_string(params.psi1) + "\n"
      + t + "psi2: " + std::to_string(params.psi2) + "\n"
      + t + "sigma12: " + std::to_string(params.sigma12) + "\n"
      + t + "sigma21: " + std::to_string(params.sigma21) + "\n"
      + t + "S0: " + std::to_string(params.S0) + "\n"
      + t + "I1_0: " + std::to_string(params.I1_0) + "\n"
      + t + "I2_0: " + std::to_string(params.I2_0) + "\n"
      + t + "R0: " + std::to_string(params.R0) + "\n";
    std::string s = tab + "state:\n"
      + t + "S: " + std::to_string(state.S) + "\n"
      + t + "I1: " + std::to_string(state.I1) + "\n"
      + t + "I2: " + std::to_string(state.I2) + "\n"
      + t + "R: " + std::to_string(state.R) + "\n";
    std::string g = tab + "genealogy:\n" + geneal.yaml(t);
    return p+s+g;
  };

};

GENERICS(SIIR,siir_genealogy_t)
