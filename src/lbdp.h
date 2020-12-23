// Linear birth-death Genealogy Process with Sampling Simulator (C++)

#include "gp.h"

typedef struct {double n; } lbdp_state_t;

class lbdp_tableau_t : public gp_tableau_t<lbdp_state_t> {
  
private:

  typedef struct {
    double n0;                  // initial population size
    double lambda;              // birth rate
    double mu;                  // death rate
    double psi;                 // sampling rate
  } parameters_t;

  parameters_t params;

  // clock: times to next event
  double nextB;
  double nextD;
  double nextS;

  double branch_rate (state_t &s) const {
    return params.lambda * (s.n-1);
  };

  double pop (state_t &s) const {
    return s.n;
  };

public:

  size_t size (void) const {
    return sizeof(parameters_t) + this->gp_tableau_t::size();
  };

  friend raw_t* operator<< (raw_t *o, const lbdp_tableau_t &T) {
    memcpy(o,&T.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o << *dynamic_cast<const gp_tableau_t*>(&T);
  };

  friend raw_t* operator>> (raw_t *o, lbdp_tableau_t &T) {
    memcpy(&T.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o >> *dynamic_cast<gp_tableau_t*>(&T);
  };

public:

  lbdp_tableau_t (void) = default;
  // basic constructor
  lbdp_tableau_t (double lambda, double mu, double psi, int n0,
                  double t0 = 0) : gp_tableau_t(t0) {
    params = {
      double(n0), lambda, mu, psi
    };
    state.n = double(n0);
    for (int j = 0; j < n0; j++) graft();
    update_clocks();
    valid();
  };
  // constructor from serialized binary form
  lbdp_tableau_t (raw_t *o) {
    o >> *this;
    update_clocks();
    valid();
  };
  // copy constructor
  lbdp_tableau_t (const lbdp_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T;
    o >> *this;
    delete[] o;
    update_clocks();
    valid();
  };
  // move constructor
  lbdp_tableau_t (lbdp_tableau_t &&) = delete;
  // copy assignment operator
  lbdp_tableau_t & operator= (const lbdp_tableau_t &) = delete;
  // move assignment operator
  lbdp_tableau_t & operator= (lbdp_tableau_t &&) = delete;
  // destructor
  ~lbdp_tableau_t (void) = default;

  void valid (void) const {
    if (params.n0 <= 0) err("total population size must be positive! %lg",params.n0);
    if (params.lambda < 0) err("negative birth rate!");
    if (params.mu < 0) err("negative death rate!");
    if (params.psi < 0) err("negative sampling rate!");
    if (state.n < 0) err("negative population!");
    if (clock() < time()) err("invalid clock");
    this->gp_tableau_t::valid();
  };
  
  // get birth rate
  double birth_rate (void) const {
    return params.lambda;
  };

  // set birth rate
  void birth_rate (double &lambda) {
    params.lambda = lambda;
    update_clocks();
  };

  // get death rate
  double death_rate (void) const {
    return params.mu;
  };

  // set death rate
  void death_rate (double &mu) {
    params.mu = mu;
    update_clocks();
  };

  // get sampling rate
  double sampling_rate (void) const {
    return params.psi;
  };

  // set sampling rate
  void sampling_rate (double &psi) {
    params.psi = psi;
    update_clocks();
  };

  void update_clocks (void) {
    double rate;
    rate = params.lambda*state.n;
    if (rate > 0) {
      nextB = time()+rexp(1/rate);
    } else {
      nextB = R_PosInf;
    }
    rate = params.mu*state.n;
    if (rate > 0) {
      nextD = time()+rexp(1/rate);
    } else {
      nextD = R_PosInf;
    }
    rate = params.psi*state.n;
    if (rate > 0) {
      nextS = time()+rexp(1/rate);
    } else {
      nextS = R_PosInf;
    }
  };

  // time to next event
  double clock (void) const {
    double next;
    if (nextB < nextD && nextB < nextS) {
      next = nextB;
    } else if (nextD < nextB && nextD < nextS) {
      next = nextD;
    } else if (nextS < nextB && nextS < nextD) {
      next = nextS;
    } else {
      next = inf;
    }
    return next;
  };
    
  void move (void) {
    if (nextB < nextD && nextB < nextS) {
      state.n += 1.0;
      birth();
    } else if (nextD < nextB && nextD < nextS) {
      state.n -= 1.0;
      death();
    } else if (nextS < nextB && nextS < nextD) {
      sample();
    } else {
      err("there's no place like home");
    }
    update_clocks();
  };

};
