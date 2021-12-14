// SIR with Sampling Genealogy Process Simulator (C++)

#include "gp.h"

typedef struct {
  double S;             // number of susceptibles
  double I;             // number of infections
  double R;             // number of recovereds
} sir_state_t;

class sir_tableau_t : public tableau_t<sir_state_t> {

private:

  const sir_state_t default_state = {R_NaReal,R_NaReal,R_NaReal};

  typedef struct {
    double N;                   // host population size
    double beta;                // transmission rate
    double gamma;               // recovery rate
    double psi;                 // sampling rate
    int S0;                     // initial susceptibles
    int I0;                     // initial infecteds
    int R0;                     // initial recoveries
  } parameters_t;

  parameters_t params;
  
  // clocks: times to next...
  double nextI;                 // ...infection
  double nextR;                 // ...recovery
  double nextS;                 // ...sample

public:

  size_t size (void) const {
    return sizeof(parameters_t) + this->tableau_t::size();
  };

  friend raw_t* operator<< (raw_t *o, const sir_tableau_t &T) {
    memcpy(o,&T.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o << *dynamic_cast<const tableau_t*>(&T);
  };

  friend raw_t* operator>> (raw_t *o, sir_tableau_t &T) {
    memcpy(&T.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o >> *dynamic_cast<tableau_t*>(&T);
  };

public:

  sir_tableau_t (void) = default;
  // basic constructor
  sir_tableau_t (double beta, double gamma, double psi,
                   int S0, int I0, int R0,
                   double t0 = 0) : tableau_t(t0) {
    params.N = double(S0+I0+R0);
    params.beta = beta;
    params.gamma = gamma;
    params.psi = psi;
    params.S0 = S0;
    params.I0 = I0;
    params.R0 = R0;
    state.S = double(S0);
    state.I = double(I0);
    state.R = double(R0);
    for (name_t j = 0; j < name_t(I0); j++) graft();
    update_clocks();
    valid();
  };
  // constructor from serialized binary form
  sir_tableau_t (raw_t *o) {
    o >> *this;
    update_clocks();
    valid();
  };
  // copy constructor
  sir_tableau_t (const sir_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T; o >> *this;
    delete[] o;
    update_clocks();
    valid();
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
  
  // get transmission rate
  double transmission_rate (void) const {
    return params.beta;
  };

  // set transmission rate
  void transmission_rate (double &beta) {
    params.beta = beta;
    update_clocks();
  };

  // get recovery rate
  double recovery_rate (void) const {
    return params.gamma;
  };

  // set recovery rate
  void recovery_rate (double &gamma) {
    params.gamma = gamma;
    update_clocks();
  };

  // get sample rate
  double sample_rate (void) const {
    return params.psi;
  };

  // set sample rate
  void sample_rate (double &psi) {
    params.psi = psi;
    update_clocks();
  };

  bool live (void) const {
    return (state.I > 0 && !(this->tableau_t::max_size_exceeded()));
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

};
#include "internal.h"

sir_tableau_t *makeSIR (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP S0, SEXP I0, SEXP R0, SEXP T0, SEXP State) {
  sir_tableau_t *A;
  
  OPTIONAL_REAL_PAR(beta,Beta,1);
  OPTIONAL_REAL_PAR(gamma,Gamma,1);
  OPTIONAL_REAL_PAR(psi,Psi,1);

  if (isNull(State)) {        // a fresh SIR

    double t0 = *(REAL(AS_NUMERIC(T0)));

    OPTIONAL_INT_PAR(s0,S0,100);
    OPTIONAL_INT_PAR(i0,I0,1);
    OPTIONAL_INT_PAR(r0,R0,0);

    A = new sir_tableau_t(beta,gamma,psi,s0,i0,r0,t0);

  }  else {              // restart the SIR from the specified state

    A = new sir_tableau_t(RAW(State));
    // optionally override the stored parameters
    if (!isNull(Beta)) A->transmission_rate(beta);
    if (!isNull(Gamma)) A->recovery_rate(gamma);
    if (!isNull(Psi)) A->sample_rate(psi);

  }

  A->valid();
    
  return A;
}

extern "C" {

  // Sampled SIR process.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  SEXP playSIR (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP S0, SEXP I0, SEXP R0, SEXP Times, SEXP T0, SEXP Tree, SEXP Ill, SEXP State) {
    SEXP out = R_NilValue;
    GetRNGstate();
    sir_tableau_t *A = makeSIR(Beta,Gamma,Psi,S0,I0,R0,T0,State);
    PROTECT(out = playGP<sir_tableau_t>(A,Times,Tree,Ill));
    PutRNGstate();
    delete A;
    UNPROTECT(1);
    return out;
  }

  // Extract information from the stored state of a GP process

  SEXP get_SIR_info (SEXP X, SEXP Prune, SEXP Compact) {
    return get_info<sir_tableau_t>(X,Prune,Compact);
  }

}
