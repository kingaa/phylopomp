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

class siir_tableau_t : public tableau_t<siir_state_t,siir_parameters_t,2> {

private:

  typedef enum {inf1, inf2, recov1, recov2, sample1, sample2} event_t;
  // clock
  slate_t next;
  event_t event;

public:

  siir_tableau_t (void) = default;
  // basic constructor
  siir_tableau_t (double t0 = 0) : tableau_t(t0) { };
  // constructor from serialized binary form
  siir_tableau_t (raw_t *o) {
    o >> *this;
  };
  // copy constructor
  siir_tableau_t (const siir_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T; o >> *this;
    delete[] o;
  };
  // move constructor
  siir_tableau_t (siir_tableau_t &&) = delete;
  // copy assignment operator
  siir_tableau_t & operator= (const siir_tableau_t &) = delete;
  // move assignment operator
  siir_tableau_t & operator= (siir_tableau_t &&) = delete;
  // destructor
  ~siir_tableau_t (void) = default;

  void valid (void) {
    this->tableau_t::valid();
    if (params.N <= 0) err("total population size must be positive!");
    if (state.S < 0 || state.I1 < 0 || state.I2 < 0 || state.R < 0) err("negative state variables!");
    if (params.N != state.S+state.I1+state.I2+state.R) err("population leakage!");
    if (size_t(state.I1) != inventory[0].size()) err("inventory misaccounting!");
    if (size_t(state.I2) != inventory[1].size()) err("inventory misaccounting!");
    if (clock() < time()) err("invalid clock");
  };
  
  bool live (void) const {
    return (state.I1+state.I2 > 0);
  };

  void update_clocks (void) {
    double rate[6];
    double total_rate;
    rate[0] = params.beta1 * state.S * state.I1 / params.N;
    rate[1] = params.beta2 * state.S * state.I2 / params.N;
    rate[2] = params.gamma*state.I1;
    rate[3] = params.gamma*state.I2;
    rate[4] = params.psi*state.I1;
    rate[5] = params.psi*state.I2;
    total_rate = rate[0] + rate[1] + rate[2] + rate[3] + rate[4] + rate[5];
    if (total_rate > 0) {
      next = time()+rexp(1/total_rate);
    } else {
      next = R_PosInf;
    }
    double u = runif(0,total_rate);
    int k = 0;
    while (u > rate[k] && k < 5) {
      u -= rate[k];
      k++;
    }
    event = static_cast<event_t>(k);
  };

  // time to next event
  double clock (void) const {
    return next;
  };
    
  void jump (void) {
    switch (event) {
    case inf1:
      state.S -= 1.0;
      state.I1 += 1.0;
      birth(0,0);
      break;
    case inf2:
      state.S -= 1.0;
      state.I2 += 1.0;
      birth(1,1);
      break;
    case sample1:
      sample(0);
      break;
    case sample2:
      sample(1);
      break;
    case recov1:
      state.I1 -= 1.0;
      state.R += 1.0;
      death(0);
      break;
    case recov2:
      state.I2 -= 1.0;
      state.R += 1.0;
      death(1);
      break;
    }
    update_clocks();
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

  void rinit (void) {
    state.S = double(params.S0);
    state.I1 = double(params.I1_0);
    state.I2 = double(params.I2_0);
    state.R = double(params.R0);
    for (name_t j = 0; j < name_t(params.I1_0); j++) graft(0);
    for (name_t j = 0; j < name_t(params.I2_0); j++) graft(1);
  };

};

extern "C" {

  SEXP makeSIIR (SEXP Params, SEXP ICs, SEXP T0) {
    return make_tableau<siir_tableau_t>(Params,ICs,T0);
  }

  SEXP reviveSIIR (SEXP State, SEXP Params) {
    return revive_tableau<siir_tableau_t>(State,Params);
  }

  SEXP runSIIR (SEXP State, SEXP Times) {
    return run_gp<siir_tableau_t>(State,Times);
  }

  SEXP infoSIIR (SEXP State, SEXP Prune, SEXP Compact) {
    return get_info<siir_tableau_t>(State,Prune,Compact);
  }

}
