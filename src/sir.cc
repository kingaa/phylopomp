// SIR with Sampling Genealogy Process Simulator (C++)

#include "gp.h"
#include "internal.h"

typedef struct {
  double S;             // number of susceptibles
  double I;             // number of infections
  double R;             // number of recovereds
} sir_state_t;

typedef struct {
  double Beta;                // transmission rate
  double gamma;               // recovery rate
  double psi;                 // sampling rate
  double N;                   // host population size
  int S0;                     // initial susceptibles
  int I0;                     // initial infecteds
  int R0;                     // initial recoveries
} sir_parameters_t;

class sir_genealogy_t : public genealogy_t<sir_state_t,sir_parameters_t,3> {

public:

  // basic constructor
  sir_genealogy_t (double t0 = 0) : genealogy_t(t0) { };
  // constructor from serialized binary form
  sir_genealogy_t (raw_t *o) {
    o >> *this;
    valid();
  };
  // copy constructor
  sir_genealogy_t (const sir_genealogy_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T; o >> *this;
    delete[] o;
    valid();
  };
  
  void valid (void) const {
    this->genealogy_t::valid();
    if (params.N <= 0) err("total population size must be positive!");
    if (state.S < 0 || state.I < 0 || state.R < 0) err("negative state variables!");
  };

  void rinit (void) {
    state.S = double(params.S0);
    state.I = double(params.I0);
    state.R = double(params.R0);
    for (int j = 0; j < params.I0; j++) graft();
  };

  double event_rates (double *rate) const {
    rate[0] = params.Beta * state.S * state.I / params.N; // infection
    rate[1] = params.gamma * state.I;			  // recovery
    rate[2] = params.psi * state.I;			  // sample
    return rate[0] + rate[1] + rate[2];
  };

  void jump (name_t event) {
    switch (event) {
    case 0:			// infection
      state.S -= 1.0;
      state.I += 1.0;
      birth();
      break;
    case 1:			// recovery
      state.I -= 1.0;
      state.R += 1.0;
      death();
      break;
    case 2:			// sample
      sample();
      break;
    default:
      err("in SIR 'jump': c'est impossible! (%ld)",event); // # nocov
      break;
    }
  };

  void update_params (double *p, int n) {
    if (n != 3) err("wrong number of parameters!");
    if (!ISNA(p[0])) params.Beta = p[0];
    if (!ISNA(p[1])) params.gamma = p[1];
    if (!ISNA(p[2])) params.psi = p[2];
  };

  void update_ICs (double *p, int n) {
    if (n != 3) err("wrong number of initial conditions!");
    if (!ISNA(p[0])) params.S0 = int(p[0]);
    if (!ISNA(p[1])) params.I0 = int(p[1]);
    if (!ISNA(p[2])) params.R0 = int(p[2]);    
    params.N = double(params.S0+params.I0+params.R0);
  };

};

extern "C" {

  SEXP makeSIR (SEXP Params, SEXP ICs, SEXP T0) {
    return make_gp<sir_genealogy_t>(Params,ICs,T0);
  }

  SEXP reviveSIR (SEXP State, SEXP Params) {
    return revive_gp<sir_genealogy_t>(State,Params);
  }

  SEXP runSIR (SEXP State, SEXP Times) {
    return run_gp<sir_genealogy_t>(State,Times);
  }

  SEXP infoSIR (SEXP State, SEXP Prune, 
		 SEXP T0, SEXP Time, SEXP Descript,
		 SEXP Yaml, SEXP Structure, SEXP Lineages,
		 SEXP Tree, SEXP Compact) {
    return info_gp<sir_genealogy_t>(State, Prune, T0, Time, Descript,
				    Yaml,Structure, Lineages, Tree, Compact);
  }

}
