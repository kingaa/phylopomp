// SIIR with Sampling Genealogy Process Simulator (C++)

#include "gp.h"
#include "internal.h"

typedef struct {
  double S;             // number of susceptibles
  double I1, I2;        // number of infections
  double R;             // number of recovereds
} siir_state_t;

typedef struct {
  double beta1, beta2;        // transmission rate
  double gamma;               // recovery rate
  double psi;                 // sampling rate
  double N;                   // host population size
  int S0;                     // initial susceptibles
  int I1_0, I2_0;             // initial infecteds
  int R0;                     // initial recoveries
} siir_parameters_t;

class siir_genealogy_t : public genealogy_t<siir_state_t,siir_parameters_t,6,2> {

public:

  // basic constructor
  siir_genealogy_t (double t0 = 0) : genealogy_t(t0) { };
  // constructor from serialized binary form
  siir_genealogy_t (raw_t *o) {
    o >> *this;
  };
  // copy constructor
  siir_genealogy_t (const siir_genealogy_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T; o >> *this;
    delete[] o;
  };

  void valid (void) {
    this->genealogy_t::valid();
    if (params.N <= 0) err("total population size must be positive!");
    if (state.S < 0 || state.I1 < 0 || state.I2 < 0 || state.R < 0) err("negative state variables!");
    if (params.N != state.S+state.I1+state.I2+state.R) err("population leakage!");
    if (size_t(state.I1) != inventory[0].size()) err("inventory misaccounting!");
    if (size_t(state.I2) != inventory[1].size()) err("inventory misaccounting!");
  };

  void rinit (void) {
    state.S = double(params.S0);
    state.I1 = double(params.I1_0);
    state.I2 = double(params.I2_0);
    state.R = double(params.R0);
    for (int j = 0; j < params.I1_0; j++) graft(0);
    for (int j = 0; j < params.I2_0; j++) graft(1);
  };

  double event_rates (double *rate) const {
    rate[0] = params.beta1 * state.S * state.I1 / params.N;
    rate[1] = params.beta2 * state.S * state.I2 / params.N;
    rate[2] = params.gamma * state.I1;
    rate[3] = params.gamma * state.I2;
    rate[4] = params.psi * state.I1;
    rate[5] = params.psi * state.I2;
    return rate[0] + rate[1] + rate[2] + rate[3] + rate[4] + rate[5];
  };

  void jump (name_t event) {
    switch (event) {
    case 0:
      state.S -= 1.0;
      state.I1 += 1.0;
      birth(0,0);
      break;
    case 1:
      state.S -= 1.0;
      state.I2 += 1.0;
      birth(1,1);
      break;
    case 2:
      state.I1 -= 1.0;
      state.R += 1.0;
      death(0);
      break;
    case 3:
      state.I2 -= 1.0;
      state.R += 1.0;
      death(1);
      break;
    case 4:
      sample(0);
      break;
    case 5:
      sample(1);
      break;
    default:
      err("in SIIR 'jump': c'est impossible! (%ld)",event); // # nocov
      break;
    }
  };

  void update_params (double *p) {
    if (!ISNA(p[0])) params.beta1 = p[0];
    if (!ISNA(p[1])) params.beta2 = p[1];
    if (!ISNA(p[2])) params.gamma = p[2];
    if (!ISNA(p[3])) params.psi = p[3];
  };

  void update_ICs (double *p) {
    if (!ISNA(p[0])) params.S0 = int(p[0]);
    if (!ISNA(p[1])) params.I1_0 = int(p[1]);
    if (!ISNA(p[2])) params.I2_0 = int(p[2]);
    if (!ISNA(p[3])) params.R0 = int(p[3]);
    params.N = double(params.S0+params.I1_0+params.I2_0+params.R0);
  }

};

extern "C" {

  SEXP makeSIIR (SEXP Params, SEXP ICs, SEXP T0) {
    return make_gp<siir_genealogy_t>(Params,ICs,T0);
  }

  SEXP reviveSIIR (SEXP State, SEXP Params) {
    return revive_gp<siir_genealogy_t>(State,Params);
  }

  SEXP runSIIR (SEXP State, SEXP Times) {
    return run_gp<siir_genealogy_t>(State,Times);
  }

  SEXP treeSIIR (SEXP State, SEXP Prune, SEXP Compact) {
    return tree_gp<siir_genealogy_t>(State,Prune,Compact);
  }

  SEXP structSIIR (SEXP State, SEXP Prune) {
    return structure_gp<siir_genealogy_t>(State,Prune);
  }

  SEXP infoSIIR (SEXP State, SEXP Prune, SEXP Compact) {
    return info_gp<siir_genealogy_t>(State,Prune,Compact);
  }

}
