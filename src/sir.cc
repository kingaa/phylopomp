// SIR with Sampling Genealogy Process Simulator (C++)

#include "gp.h"
#include "internal.h"

typedef struct {
  double S;             // number of susceptibles
  double I;             // number of infections
  double R;             // number of recovereds
} sir_state_t;

typedef struct {
  double N;                   // host population size
  double beta;                // transmission rate
  double gamma;               // recovery rate
  double psi;                 // sampling rate
  int S0;                     // initial susceptibles
  int I0;                     // initial infecteds
  int R0;                     // initial recoveries
} sir_parameters_t;

class sir_tableau_t : public tableau_t<sir_state_t,sir_parameters_t> {

private:

  // clocks: times to next...
  double nextI;                 // ...infection
  double nextR;                 // ...recovery
  double nextS;                 // ...sample

public:

  sir_tableau_t (void) = default;
  // basic constructor
  sir_tableau_t (double t0 = 0) : tableau_t(t0) { };
  // constructor from serialized binary form
  sir_tableau_t (raw_t *o) {
    o >> *this;
  };
  // copy constructor
  sir_tableau_t (const sir_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T; o >> *this;
    delete[] o;
  };
  // move constructor
  sir_tableau_t (sir_tableau_t &&) = delete;
  // copy assignment operator
  sir_tableau_t & operator= (const sir_tableau_t &) = delete;
  // move assignment operator
  sir_tableau_t & operator= (sir_tableau_t &&) = delete;
  // destructor
  ~sir_tableau_t (void) = default;

  void valid (void) const {
    this->tableau_t::valid();
    if (params.N <= 0) err("total population size must be positive!");
    if (state.S < 0 || state.I < 0 || state.R < 0) err("negative state variables!");
    if (params.N != state.S+state.I+state.R) err("population leakage!");
    if (clock() < time()) err("invalid clock");
  };

  bool live (void) const {
    return (state.I > 0);
  };

  void update_clocks (void) {
    double rate;
    rate = params.beta * state.S * state.I / params.N;
    if (rate > 0) {
      nextI = time()+rexp(1/rate);
    } else {
      nextI = R_PosInf;
    }
    rate = params.gamma*state.I;
    if (rate > 0) {
      nextR = time()+rexp(1/rate);
    } else {
      nextR = R_PosInf;
    }
    rate = params.psi*state.I;
    if (rate > 0) {
      nextS = time()+rexp(1/rate);
    } else {
      nextS = R_PosInf;
    }
  };

  // time to next event
  double clock (void) const {
    double next;
    if (nextI < nextR && nextI < nextS) {
      next = nextI;
    } else if (nextS < nextI && nextS < nextR) {
      next = nextS;
    } else if (nextR < nextI && nextR < nextS) {
      next = nextR;
    } else {
      next = R_PosInf;
    }
    return next;
  };
    
  void jump (void) {
    if (nextI < nextR && nextI < nextS) {
      state.S -= 1.0;
      state.I += 1.0;
      birth();
    } else if (nextS < nextI && nextS < nextR) {
      sample();
    } else if (nextR < nextI && nextR < nextS) {
      state.I -= 1.0;
      state.R += 1.0;
      death();
    }
    update_clocks();
  };

  void update_params (double *p) {
    if (!ISNA(p[0])) params.beta = p[0];
    if (!ISNA(p[1])) params.gamma = p[1];
    if (!ISNA(p[2])) params.psi = p[2];
  };

  void update_ICs (double *p) {
    if (!ISNA(p[0])) params.S0 = int(p[0]);
    if (!ISNA(p[1])) params.I0 = int(p[1]);
    if (!ISNA(p[2])) params.R0 = int(p[2]);    
    params.N = double(params.S0+params.I0+params.R0);
  }

  void rinit (void) {
    state.S = double(params.S0);
    state.I = double(params.I0);
    state.R = double(params.R0);
    for (name_t j = 0; j < name_t(params.I0); j++) graft();
  };

};

extern "C" {

  SEXP makeSIR (SEXP Params, SEXP ICs, SEXP T0) {
    return make_tableau<sir_tableau_t>(Params,ICs,T0);
  }

  SEXP reviveSIR (SEXP State, SEXP Params) {
    return revive_tableau<sir_tableau_t>(State,Params);
  }

  SEXP runSIR (SEXP State, SEXP Times) {
    return run_gp<sir_tableau_t>(State,Times);
  }

  SEXP infoSIR (SEXP State, SEXP Prune, SEXP Compact) {
    return get_info<sir_tableau_t>(State,Prune,Compact);
  }

}
