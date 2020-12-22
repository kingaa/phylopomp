// Moran Genealogy Process with Sampling Simulator (C++)

#include "gp.h"

typedef struct { } moran_state_t;

class moran_tableau_t : public gp_tableau_t<moran_state_t> {

private:

  typedef struct {
    int n;                      // population size
    double mu;                  // Moran rate
  } parameters_t;

  parameters_t params;
  
  // clock: times to next event
  double _next;

  double branch_rate (state_t &s) const {
    return params.mu;
  };

  double pop (state_t &s) const {
    return double(params.n);
  };

public:

  size_t size (void) const {
    return sizeof(parameters_t) + this->gp_tableau_t::size();
  };

  friend raw_t* operator<< (raw_t *o, const moran_tableau_t &T) {
    memcpy(o,&T.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o << *dynamic_cast<const gp_tableau_t*>(&T);
  };

  friend raw_t* operator>> (raw_t *o, moran_tableau_t &T) {
    memcpy(&T.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o >> *dynamic_cast<gp_tableau_t*>(&T);
  };

public:

  moran_tableau_t (void) = default;
  // basic constructor
  moran_tableau_t (int n, double mu, double t0, int stationary) :
    gp_tableau_t(t0,false) {
    params.n = n;
    params.mu = mu;
    if (stationary) {
      std::vector<double> times(n,0);
      double scale = 1/mu;
      times[n-1] = t0;
      for (int j = n-1; j > 0; j--) {
        // scale = choose(n,2)/choose(j+1,2)/mu
        times[j-1] = times[j] - rexp(scale);
        scale *= double(j+1)/double(j-1);
      }
      time(R_NegInf); graft();
      for (int j = 0; j < n-1; j++) {
        time(times[j]); birth();
      }
    } else {
      for (int j = 0; j < n; j++) graft();
    }
    time(t0);
    update_clocks();
    valid();
  };
  // constructor from serialized binary form
  moran_tableau_t (raw_t *o) {
    o >> *this;
    update_clocks();
    valid();
  };
  // copy constructor
  moran_tableau_t (const moran_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T;
    o >> *this;
    delete[] o;
    update_clocks();
    valid();
  };
  // move constructor
  moran_tableau_t (moran_tableau_t &&) = delete;
  // copy assignment operator
  moran_tableau_t & operator= (const moran_tableau_t &) = delete;
  // move assignment operator
  moran_tableau_t & operator= (moran_tableau_t &&) = delete;
  // destructor
  ~moran_tableau_t (void) = default;

  void valid (void) const {
    this->gp_tableau_t::valid();
    if (params.n == R_NaInt || params.n <= 0) err("'n' must be positive!");
    if (!R_FINITE(params.mu) || params.mu <= 0) err("'mu' must be positive!");
    if (clock() < time()) err("invalid clock %lg %lg",clock(),time());
  };
  
  // get transmission rate
  double moran_rate (void) const {
    return params.mu;
  };

  // set transmission rate
  void moran_rate (double &mu) {
    params.mu = mu;
    update_clocks();
  };

  // get population size
  int popsize (void) const {
    return params.n;
  };

  // set recovery rate
  void popsize (int &n) {
    params.n = n;
    update_clocks();
  };

  void update_clocks (void) {
    double rate;
    rate = moran_rate();
    if (rate > 0) {
      _next = time()+rexp(1/rate);
    } else {
      _next = R_PosInf;
    }
  };

  // time to next event
  double clock (void) const {
    return _next;
  };
    
  void move (void) {
    death();
    birth();
    update_clocks();
  };

};

moran_tableau_t *makeGP (SEXP N, SEXP Mu, SEXP T0, SEXP Stat, SEXP State) {
  moran_tableau_t *gp;
  if (isNull(State)) {		// a fresh GP
    double t0 = *REAL(AS_NUMERIC(T0));
    int stat = *INTEGER(AS_INTEGER(Stat));
    GetRNGstate();
    gp = new moran_tableau_t(*INTEGER(AS_INTEGER(N)),*REAL(AS_NUMERIC(Mu)),t0,stat);
    PutRNGstate();
  }  else {		    // restart the GP from the specified state
    gp = new moran_tableau_t(RAW(State));
    // optionally override the stored parameters
    if (!isNull(N)) gp->popsize(*INTEGER(AS_INTEGER(N)));
    if (!isNull(Mu)) gp->moran_rate(*REAL(AS_NUMERIC(Mu)));
  }
  return gp;
}

extern "C" {

  // (Sampled or unsampled) Moran genealogy process.
  // If 'sample = TRUE', one sample is taken at each timepoint.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  // optionally return state in diagrammable form ('ill = TRUE').
  SEXP playMoran (SEXP N, SEXP Mu, SEXP Times, SEXP T0, SEXP Sample, SEXP Tree, SEXP Ill, SEXP Stat, SEXP State) {
    moran_tableau_t *gp;
    SEXP out = R_NilValue;
    GetRNGstate();
    gp = makeGP(N,Mu,T0,Stat,State);
    if (*INTEGER(AS_INTEGER(Sample))) {
      PROTECT(out = playSGP<moran_tableau_t>(gp,Times,Tree,Ill));
    } else {
      PROTECT(out = playGP<moran_tableau_t>(gp,Times,Tree,Ill));
    }
    PutRNGstate();
    delete gp;
    UNPROTECT(1);
    return out;
  }

  // Play unsampled Moran W chain
  // optionally compute genealogies in Newick form ('tree = TRUE').
  // optionally return state in diagrammable form ('ill = TRUE').
  SEXP playMoranWChain (SEXP N, SEXP Mu, SEXP Ntimes, SEXP T0, SEXP Tree, SEXP Ill, SEXP Stat, SEXP State) {
    moran_tableau_t *gp;
    SEXP out = R_NilValue;
    GetRNGstate();
    gp = makeGP(N,Mu,T0,Stat,State);
    PROTECT(out = playWChain<moran_tableau_t>(gp,Ntimes,Tree,Ill));
    PutRNGstate();
    delete gp;
    UNPROTECT(1);
    return out;
  }

  // extract/compute basic information.
  SEXP get_Moran_info (SEXP X, SEXP Prune) {
    return get_info<moran_tableau_t>(X,Prune);
  }

}
