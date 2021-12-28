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

  void update_params (double *p, int n) {
    int m = 0;
    PARAM_SET(mu);
    PARAM_SET(psi);
    if (m != n) err("wrong number of parameters!");
  };

  void update_ICs (double *p, int n) {
    int m = 0;
    PARAM_SET(n);
    if (m != n) err("wrong number of parameters!");
  };

  void rinit (void) {
    state.m = 0; state.g = 0;
    graft(0,params.n);
  };

  double event_rates (double *rate, int n) const {
    int m = 0;
    double total = 0;
    RATE_CALC(params.mu * params.n);		// birth/death
    RATE_CALC(params.psi * params.n);		// sample
    if (m != n) err("wrong number of events!");
    return total;
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
    default:						    // #nocov
      err("in %s: c'est impossible! (%ld)",__func__,event); // #nocov
      break;
    }
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
    std::string g = tab + "genealogy:\n" + geneal.yaml(t);
    return p+s+g;
  };

};

GENERICS(Moran,mgp_t)
