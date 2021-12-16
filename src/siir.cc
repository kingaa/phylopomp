// SIIR with Sampling Genealogy Process Simulator (C++)

#include "gp.h"
#include "internal.h"

typedef struct {
  double S;             // number of susceptibles
  double I1, I2;	// number of infections
  double R;             // number of recovereds
} siir_state_t;

typedef struct {
  double N;                   // host population size
  double beta1, beta2;	      // transmission rate
  double gamma;               // recovery rate
  double psi;                 // sampling rate
  int S0;                     // initial susceptibles
  int I1_0, I2_0;	      // initial infecteds
  int R0;                     // initial recoveries
} siir_parameters_t;

class siir_tableau_t : public tableau_t<siir_state_t,siir_parameters_t,2> {

private:

  const siir_state_t default_state = {R_NaReal,R_NaReal,R_NaReal,R_NaReal};

  typedef enum {inf1, inf2, recov1, recov2, sample1, sample2} event_t;
  // clock
  slate_t next;
  event_t event;

public:

  siir_tableau_t (void) = default;
  // basic constructor
  siir_tableau_t (double beta1, double beta2,
		  double gamma, double psi,
		  int S0, int I1_0, int I2_0, int R0,
		  double t0 = 0) : tableau_t(t0) {
    params.N = double(S0+I1_0+I2_0+R0);
    params.beta1 = beta1;
    params.beta2 = beta2;
    params.gamma = gamma;
    params.psi = psi;
    params.S0 = S0;
    params.I1_0 = I1_0;
    params.I2_0 = I2_0;
    params.R0 = R0;
    state.S = double(S0);
    state.I1 = double(I1_0);
    state.I2 = double(I2_0);
    state.R = double(R0);
    for (name_t j = 0; j < name_t(I1_0); j++) graft(0);
    for (name_t j = 0; j < name_t(I2_0); j++) graft(1);
    update_clocks();
    valid();
  };
  // constructor from serialized binary form
  siir_tableau_t (raw_t *o) {
    o >> *this;
    update_clocks();
    valid();
  };
  // copy constructor
  siir_tableau_t (const siir_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T; o >> *this;
    delete[] o;
    update_clocks();
    valid();
  };
  // move constructor
  siir_tableau_t (siir_tableau_t &&) = delete;
  // copy assignment operator
  siir_tableau_t & operator= (const siir_tableau_t &) = delete;
  // move assignment operator
  siir_tableau_t & operator= (siir_tableau_t &&) = delete;
  // destructor
  ~siir_tableau_t (void) = default;

  void valid (void) {
    this->tableau_t::valid();
    if (params.N <= 0) err("total population size must be positive!");
    if (state.S < 0 || state.I1 < 0 || state.I2 < 0 || state.R < 0) err("negative state variables!");
    if (params.N != state.S+state.I1+state.I2+state.R) err("population leakage!");
    if (size_t(state.I1) != inventory[0].size()) err("inventory misaccounting!");
    if (size_t(state.I2) != inventory[1].size()) err("inventory misaccounting!");
    if (clock() < time()) err("invalid clock");
  };
  
  // get transmission rate
  double transmission_rate1 (void) const {
    return params.beta1;
  };

  // set transmission rate
  void transmission_rate1 (double &beta) {
    params.beta1 = beta;
    update_clocks();
  };

  // get transmission rate
  double transmission_rate2 (void) const {
    return params.beta2;
  };

  // set transmission rate
  void transmission_rate2 (double &beta) {
    params.beta2 = beta;
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
    return (state.I1+state.I2 > 0 && !max_size_exceeded());
  };

  void update_clocks (void) {
    double rate[6];
    double total_rate;
    rate[0] = params.beta1 * state.S * state.I1 / params.N;
    rate[1] = params.beta2 * state.S * state.I2 / params.N;
    rate[2] = params.gamma*state.I1;
    rate[3] = params.gamma*state.I2;
    rate[4] = params.psi*state.I1;
    rate[5] = params.psi*state.I2;
    total_rate = rate[0] + rate[1] + rate[2] + rate[3] + rate[4] + rate[5];
    if (total_rate > 0) {
      next = time()+rexp(1/total_rate);
    } else {
      next = R_PosInf;
    }
    double u = runif(0,total_rate);
    int k = 0;
    while (u > rate[k] && k < 5) {
      u -= rate[k];
      k++;
    }
    event = static_cast<event_t>(k);
    valid();
  };

  // time to next event
  double clock (void) const {
    return next;
  };
    
  void jump (void) {
    switch (event) {
    case inf1:
      state.S -= 1.0;
      state.I1 += 1.0;
      birth(0,0);
      break;
    case inf2:
      state.S -= 1.0;
      state.I2 += 1.0;
      birth(1,1);
      break;
    case sample1:
      sample(0);
      break;
    case sample2:
      sample(1);
      break;
    case recov1:
      state.I1 -= 1.0;
      state.R += 1.0;
      death(0);
      break;
    case recov2:
      state.I2 -= 1.0;
      state.R += 1.0;
      death(1);
      break;
    }
    update_clocks();
  };

};

siir_tableau_t *makeSIIR (SEXP Beta1, SEXP Beta2, SEXP Gamma, SEXP Psi,
			  SEXP S0, SEXP I1_0, SEXP I2_0, SEXP R0, SEXP T0, SEXP State) {
  siir_tableau_t *A;
  
  OPTIONAL_REAL_PAR(beta1,Beta1,1);
  OPTIONAL_REAL_PAR(beta2,Beta2,1);
  OPTIONAL_REAL_PAR(gamma,Gamma,1);
  OPTIONAL_REAL_PAR(psi,Psi,1);

  if (isNull(State)) {        // a fresh SIR

    double t0 = *(REAL(AS_NUMERIC(T0)));

    OPTIONAL_INT_PAR(s0,S0,100);
    OPTIONAL_INT_PAR(i1_0,I1_0,1);
    OPTIONAL_INT_PAR(i2_0,I2_0,1);
    OPTIONAL_INT_PAR(r0,R0,0);

    A = new siir_tableau_t(beta1,beta2,gamma,psi,s0,i1_0,i2_0,r0,t0);

  }  else {              // restart the SIR from the specified state

    A = new siir_tableau_t(RAW(State));
    // optionally override the stored parameters
    if (!isNull(Beta1)) A->transmission_rate1(beta1);
    if (!isNull(Beta2)) A->transmission_rate2(beta2);
    if (!isNull(Gamma)) A->recovery_rate(gamma);
    if (!isNull(Psi)) A->sample_rate(psi);

  }

  A->valid();
    
  return A;
}

extern "C" {

  // Sampled SIIR process.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  SEXP playSIIR (SEXP Beta1, SEXP Beta2, SEXP Gamma, SEXP Psi, SEXP S0, SEXP I1_0, SEXP I2_0,
		 SEXP R0, SEXP Times, SEXP T0, SEXP Tree, SEXP Compact, SEXP State) {
    SEXP out = R_NilValue;
    GetRNGstate();
    siir_tableau_t *A = makeSIIR(Beta1,Beta2,Gamma,Psi,S0,I1_0,I2_0,R0,T0,State);
    PROTECT(out = playGP<siir_tableau_t>(A,Times,Tree,Compact));
    PutRNGstate();
    delete A;
    UNPROTECT(1);
    return out;
  }

  // Extract information from the stored state of a GP process

  SEXP get_SIIR_info (SEXP X, SEXP Prune, SEXP Compact) {
    return get_info<siir_tableau_t>(X,Prune,Compact);
  }

}
