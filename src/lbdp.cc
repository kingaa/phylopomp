// Linear birth-death-sampling genealogy process simulator (C++)

#include "master.h"
#include "popul_proc.h"
#include "generics.h"

typedef struct {
  int n;
} lbdp_state_t;

typedef struct {
  double lambda;                // birth rate
  double mu;                    // death rate
  double psi;                   // sampling rate
  int n0;                       // initial population size
} lbdp_parameters_t;

class lbdp_genealogy_t : public master_t<popul_proc_t<lbdp_state_t,lbdp_parameters_t,3> > {

public:

  // basic constructor
  lbdp_genealogy_t (double t0 = 0) : master_t(t0) { };
  // constructor from serialized binary form
  lbdp_genealogy_t (raw_t *o) : master_t(o) {};
  // copy constructor
  lbdp_genealogy_t (const lbdp_genealogy_t &G) : master_t(G) {};
  
  void valid (void) const {
    master_t::valid();
    if (state.n < 0) err("negative population size!");
  };

  void rinit (void) {
    state.n = params.n0;
    for (int j = 0; j < params.n0; j++) graft();
  };

  double event_rates (double *rate, int n) const {
    if (n != 3) err("wrong number of events!");
    rate[0] = params.lambda * state.n; // birth
    rate[1] = params.mu * state.n;     // death
    rate[2] = params.psi * state.n;    // sample
    return rate[0] + rate[1] + rate[2];
  };

  void jump (int event) {
    switch (event) {
    case 0:                     // birth
      state.n += 1;
      birth();
      break;
    case 1:                     // death
      state.n -= 1;
      death();
      break;
    case 2:                     // sample
      sample();
      break;
    default:
      err("in LBDP 'jump': c'est impossible! (%ld)",event); // # nocov
      break;
    }
  };

  void update_params (double *p, int n) {
    if (n != 3) err("wrong number of parameters!");
    if (!ISNA(p[0])) params.lambda = p[0];
    if (!ISNA(p[1])) params.mu = p[1];
    if (!ISNA(p[2])) params.psi = p[2];
  };

  void update_ICs (double *p, int n) {
    if (n != 1) err("wrong number of initial conditions!");
    if (!ISNA(p[0])) params.n0 = int(p[0]);
  };

  // human-readable info
  std::string yaml (std::string tab = "") const {
    std::string t = tab + "  ";
    std::string p = tab + "parameter:\n"
      + t + "lambda: " + std::to_string(params.lambda) + "\n"
      + t + "mu: " + std::to_string(params.mu) + "\n"
      + t + "psi: " + std::to_string(params.psi) + "\n"
      + t + "n0: " + std::to_string(params.n0) + "\n";
    std::string s = tab + "state:\n"
      + t + "n: " + std::to_string(state.n) + "\n";
    std::string g = tab + "genealogy:\n" + geneal.yaml(t);
    return p+s+g;
  };

};

GENERICS(LBDP,lbdp_genealogy_t)
