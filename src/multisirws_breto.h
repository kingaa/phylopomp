// multisir with Sampling Genealogy Process Simulator (C++)
// with multifurcation birth

#include "gp.h"

typedef struct {
  double S;             // number of susceptibles
  double I;             // number of infections
  double R;             // number of recovereds
} multisir_state_t;

const multisir_state_t default_multistate = {R_NaReal,R_NaReal,R_NaReal};

class multisirws_tableau_t : public gp_tableau_t<multisir_state_t> {

private:

  typedef struct {
    double N;                   // total population size
    double beta;                // transmission rate
    double gamma;               // recovery rate
    double psi;                 // sampling rate
    double theta;               // inverse dispersion parameter
    int S0;                     // initial susceptibles
    int I0;                     // initial infecteds
    int R0;                     // initial recoveries
  } parameters_t;

  parameters_t params;
  
  // clocks: times to next...
  double nextI;                 // ...infection
  double nextR;                 // ...recovery
  double nextS;                 // ...sample

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

  friend raw_t* operator<< (raw_t *o, const multisirws_tableau_t &T) {
    memcpy(o,&T.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o << *dynamic_cast<const gp_tableau_t*>(&T);
  };

  friend raw_t* operator>> (raw_t *o, multisirws_tableau_t &T) {
    memcpy(&T.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o >> *dynamic_cast<gp_tableau_t*>(&T);
  };

public:

  multisirws_tableau_t (void) = default;
  // basic constructor
  multisirws_tableau_t (double beta, double gamma, double psi, double theta,
                   int S0, int I0, int R0, double t0 = 0) : gp_tableau_t(t0) {
    params = {
      double(S0+I0+R0), beta, gamma, psi, theta, S0, I0, R0
    };
    state = {double(S0), double(I0), double(R0)};
    for (name_t j = 0; j < name_t(I0); j++) graft();
    update_clocks();
    valid();
  };
  // constructor from serialized binary form
  multisirws_tableau_t (raw_t *o) {
    o >> *this;
    update_clocks();
    valid();
  };
  // copy constructor
  multisirws_tableau_t (const multisirws_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T;
    o >> *this;
    delete[] o;
    update_clocks();
    valid();
  };
  // move constructor
  multisirws_tableau_t (multisirws_tableau_t &&) = delete;
  // copy assignment operator
  multisirws_tableau_t & operator= (const multisirws_tableau_t &) = delete;
  // move assignment operator
  multisirws_tableau_t & operator= (multisirws_tableau_t &&) = delete;
  // destructor
  ~multisirws_tableau_t (void) = default;

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

  // get dispersion param
  double dispersion_param (void) const {
    return params.theta;
  };

  // set dispersion rate
  void dispersion_param (double &theta) {
    params.theta = theta;
    update_clocks();
  };

  void update_clocks (void) {
    double rate, q, totalrates;
    q = state.S / (state.S + params.theta - 1.0);
    totalrates = q;
    for (int j = 2; j <= (int)state.S; j++) {
      q = q * (j-1.0) * (state.S + 1.0 - j) / j / (state.S + params.theta - j);
      totalrates += q;
    }
    rate = params.beta * state.I / params.N * params.theta * totalrates;
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
      next = inf;
    }
    return next;
  };

  // Breto & Ionides (2011)
  void birthrates (int indmax, double size, double disp, double *arr) {
    arr[0] = 0.0;
    arr[1] = size / (size + disp - 1.0);
    double p = arr[1];
    for (int j = 2; j < indmax; j++) {
     p = p * (j - 1.0) * (size + 1.0 - j) / j / (size + disp - j);
     arr[j] = arr[j-1] + p; 
    }
  }

  void move (void) {
    if (nextI < nextR && nextI < nextS) {
      int n, desc = 1, max = (int)(params.N+1.0);
      n = (int)(state.S + 1.0);
      double cum[max];
      birthrates(n, state.S, params.theta, cum);
      double Q = unif_rand() * cum[n-1];
      while (cum[desc] < Q) {
        desc++;
      }
      state.S -= double(desc);
      state.I += double(desc);
      birth(desc);
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
