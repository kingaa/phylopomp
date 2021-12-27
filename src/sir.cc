// SIR with Sampling Genealogy Process Simulator (C++)

#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
  int S;			// number of susceptibles
  int I;			// number of infections
  int R;			// number of recovereds
} sir_state_t;

typedef struct {
  double Beta;                // transmission rate
  double gamma;               // recovery rate
  double psi;                 // sampling rate
  double delta;		      // immunity waning rate
  double N;                   // host population size
  int S0;                     // initial susceptibles
  int I0;                     // initial infecteds
  int R0;                     // initial recoveries
} sir_parameters_t;

class sir_genealogy_t : public master_t<popul_proc_t<sir_state_t,sir_parameters_t,4>, 1> {

public:

  // basic constructor
  sir_genealogy_t (double t0 = 0) : master_t(t0) {};
  // constructor from serialized binary form
  sir_genealogy_t (raw_t *o) : master_t(o) {};
  // copy constructor
  sir_genealogy_t (const sir_genealogy_t &G) : master_t(G) {};
  
  void rinit (void) {
    state.S = params.S0;
    state.I = params.I0;
    state.R = params.R0;
    for (int j = 0; j < params.I0; j++) graft();
  };

  double event_rates (double *rate, int n) const {
    if (n != 4) err("wrong number of events!");
    rate[0] = params.Beta * state.S * state.I / params.N; // infection
    rate[1] = params.gamma * state.I;                     // recovery
    rate[2] = params.psi * state.I;                       // sample
    rate[3] = params.delta * state.R;			  // waning
    return rate[0] + rate[1] + rate[2] + rate[3];
  };

  void jump (int event) {
    switch (event) {
    case 0:                     // infection
      state.S -= 1;
      state.I += 1;
      birth();
      break;
    case 1:                     // recovery
      state.I -= 1;
      state.R += 1;
      death();
      break;
    case 2:                     // sample
      sample();
      break;
    case 3:			// waning
      state.S += 1;
      state.R -= 1;
      break;
    default:
      err("in SIR 'jump': c'est impossible! (%ld)",event); // # nocov
      break;
    }
  };

  void update_params (double *p, int n) {
    if (n != 4) err("wrong number of parameters!");
    if (!ISNA(p[0])) params.Beta = p[0];
    if (!ISNA(p[1])) params.gamma = p[1];
    if (!ISNA(p[2])) params.psi = p[2];
    if (!ISNA(p[3])) params.delta = p[3];
  };

  void update_ICs (double *p, int n) {
    if (n != 3) err("wrong number of initial conditions!");
    if (!ISNA(p[0])) params.S0 = int(p[0]);
    if (!ISNA(p[1])) params.I0 = int(p[1]);
    if (!ISNA(p[2])) params.R0 = int(p[2]);    
    params.N = double(params.S0+params.I0+params.R0);
  };

  // human-readable info
  std::string yaml (std::string tab = "") const {
    std::string t = tab + "  ";
    std::string p = tab + "parameter:\n"
      + t + "Beta: " + std::to_string(params.Beta) + "\n"
      + t + "gamma: " + std::to_string(params.gamma) + "\n"
      + t + "psi: " + std::to_string(params.psi) + "\n"
      + t + "delta: " + std::to_string(params.delta) + "\n"
      + t + "S0: " + std::to_string(params.S0) + "\n"
      + t + "I0: " + std::to_string(params.I0) + "\n"
      + t + "R0: " + std::to_string(params.R0) + "\n";
    std::string s = tab + "state:\n"
      + t + "S: " + std::to_string(state.S) + "\n"
      + t + "I: " + std::to_string(state.I) + "\n"
      + t + "R: " + std::to_string(state.R) + "\n";
    std::string g = tab + "genealogy:\n" + geneal.yaml(t);
    return p+s+g;
  };

};

GENERICS(SIR,sir_genealogy_t)
