// Moran genealogy process simulator (C++)

#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
  int m, g;
} moran_state_t;

typedef struct {
  double mu;			// event rate
  double psi;                   // sampling rate
  int n;			// population size
} moran_parameters_t;

class mgp_t : public master_t<popul_proc_t<moran_state_t,moran_parameters_t,2> > {

public:

  // basic constructor
  mgp_t (double t0 = 0) : master_t(t0) { };
  // constructor from serialized binary form
  mgp_t (raw_t *o) : master_t(o) {};
  // copy constructor
  mgp_t (const mgp_t &G) : master_t(G) {};
  
  void rinit (void) {
    state.m = 0; state.g = 0;
    for (int j = 0; j < params.n; j++) graft();
  };

  double event_rates (double *rate, int n) const {
    if (n != nevent) err("wrong number of events!");
    rate[0] = params.mu * params.n; // birth/death
    rate[1] = params.psi * params.n; // sample
    return rate[0] + rate[1];
  };

  void jump (int event) {
    switch (event) {
    case 0:                     // birth
      state.m += 1;
      birth();
      death();
      break;
    case 1:                     // sample
      state.g += 1;
      sample();
      break;
    default:
      err("in moran 'jump': c'est impossible! (%ld)",event); // # nocov
      break;
    }
  };

  void update_params (double *p, int n) {
    if (n != 2) err("wrong number of parameters!");
    if (!ISNA(p[0])) params.mu = p[0];
    if (!ISNA(p[1])) params.psi = p[1];
  };

  void update_ICs (double *p, int n) {
    if (n != 1) err("wrong number of initial conditions!");
    if (!ISNA(p[0])) params.n = int(p[0]);
  };

  // human-readable info
  std::string yaml (std::string tab = "") const {
    std::string t = tab + "  ";
    std::string p = tab + "parameter:\n"
      + t + "mu: " + std::to_string(params.mu) + "\n"
      + t + "psi: " + std::to_string(params.psi) + "\n"
      + t + "n: " + std::to_string(params.n) + "\n";
    std::string s = tab + "state:\n"
      + t + "m: " + std::to_string(state.m) + "\n"
      + t + "g: " + std::to_string(state.g) + "\n";
    std::string g = tab + "genealogy:\n"
      + t + geneal.yaml();
    return p+s+g;
  };

};

GENERICS(Moran,mgp_t)
