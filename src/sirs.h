// SIRS with Sampling Genealogy Process Simulator (C++)

#include "gp.h"

typedef struct {
  double S;             // number of susceptibles
  double I;             // number of infections
  double R;             // number of recovereds
} sirs_state_t;

class sirs_tableau_t : public gp_tableau_t<sirs_state_t> {

private:

  const sirs_state_t default_state = {R_NaReal,R_NaReal,R_NaReal};

  typedef struct {
    double N;                   // host population size
    double beta;                // transmission rate
    double gamma;               // recovery rate
    double delta;               // loss of immunity rate
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
  double nextW;                 // ...waning

  double branch_rate (state_t &s) const {
    return params.beta * (s.S+1) * (s.I-1) / params.N;
  };

  double pop (state_t &s) const {
    return s.I;
  };

public:

  size_t size (void) const {
    return sizeof(parameters_t) + this->gp_tableau_t::size();
  };

  friend raw_t* operator<< (raw_t *o, const sirs_tableau_t &T) {
    memcpy(o,&T.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o << *dynamic_cast<const gp_tableau_t*>(&T);
  };

  friend raw_t* operator>> (raw_t *o, sirs_tableau_t &T) {
    memcpy(&T.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o >> *dynamic_cast<gp_tableau_t*>(&T);
  };

public:

  sirs_tableau_t (void) = default;
  // basic constructor
  sirs_tableau_t (double beta, double gamma, double psi, double delta,
		  int S0, int I0, int R0, double t0 = 0) : gp_tableau_t(t0) {
    params.N = double(S0+I0+R0);
    params.beta = beta;
    params.gamma = gamma;
    params.psi = psi;
    params.delta = delta;
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
  sirs_tableau_t (raw_t *o) {
    o >> *this;
    update_clocks();
    valid();
  };
  // copy constructor
  sirs_tableau_t (const sirs_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T;
    o >> *this;
    delete[] o;
    update_clocks();
    valid();
  };
  // move constructor
  sirs_tableau_t (sirs_tableau_t &&) = delete;
  // copy assignment operator
  sirs_tableau_t & operator= (const sirs_tableau_t &) = delete;
  // move assignment operator
  sirs_tableau_t & operator= (sirs_tableau_t &&) = delete;
  // destructor
  ~sirs_tableau_t (void) = default;

  void valid (void) const {
    this->gp_tableau_t::valid();
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

  // get waning rate
  double waning_rate (void) const {
    return params.delta;
  };

  // set sample rate
  void waning_rate (double &delta) {
    params.delta = delta;
    update_clocks();
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
    rate = params.delta*state.R;
    if (rate > 0) {
      nextW = time()+rexp(1/rate);
    } else {
      nextW = R_PosInf;
    }
  };

  // time to next event
  double clock (void) const {
    double next;
    if (nextI < nextR && nextI < nextS && nextI < nextW) {
      next = nextI;
    } else if (nextS < nextI && nextS < nextR && nextS < nextW) {
      next = nextS;
    } else if (nextR < nextI && nextR < nextS && nextR < nextW) {
      next = nextR;
    } else if (nextW < nextI && nextW < nextS && nextW < nextR) {
      next = nextW;
    } else {
      next = inf;
    }
    return next;
  };
    
  void move (void) {
    if (nextI < nextR && nextI < nextS && nextI < nextW) {
      state.S -= 1.0;
      state.I += 1.0;
      birth();
    } else if (nextS < nextI && nextS < nextR && nextS < nextW) {
      sample();
    } else if (nextR < nextI && nextR < nextS && nextR < nextW) {
      state.I -= 1.0;
      state.R += 1.0;
      death();
    } else if (nextW < nextI && nextW < nextS && nextW < nextR) {
      state.S += 1.0;
      state.R -= 1.0;
    }
    update_clocks();
  };

};
