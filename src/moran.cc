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
  state_t state;
  
  // clock: times to next event
  double _next;

  double branch_rate (state_t &s) const {
    return params.mu;
  };

  double pop (state_t &s) const {
    return double(params.n);
  };

protected:

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
      time(R_NegInf); graft(state);
      for (int j = 0; j < n-1; j++) {
        time(times[j]); birth(state);
      }
    } else {
      for (int j = 0; j < n; j++) graft(state);
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

  void sample (void) {
    this->gp_tableau_t::sample(state);
  }

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
    death(state);
    birth(state);
    update_clocks();
  };

  // create the serialized state:
  friend SEXP serial (const moran_tableau_t &T) {
    SEXP out;
    PROTECT(out = NEW_RAW(T.size()));
    RAW(out) << T;
    UNPROTECT(1);
    return out;
  }

};

extern "C" {

  // (Sampled or unsampled) Moran genealogy process.
  // If 'sample = TRUE', one sample is taken at each timepoint.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  // optionally return state in diagrammable form ('ill = TRUE').
  SEXP playMoran (SEXP N, SEXP Mu, SEXP Times, SEXP T0, SEXP Sample, SEXP Tree, SEXP Ill, SEXP Stat, SEXP State) {
    int nprotect = 0;
    int nout = 3;
    double t = R_NaReal;
    int ntimes = LENGTH(Times);
    moran_tableau_t *gp;

    SEXP times, count;
    PROTECT(times = AS_NUMERIC(duplicate(Times))); nprotect++;
    PROTECT(count = NEW_INTEGER(ntimes)); nprotect++;

    int do_sample = *(INTEGER(AS_INTEGER(Sample)));

    SEXP tree = R_NilValue;
    int do_newick = *(INTEGER(AS_INTEGER(Tree)));
    if (do_newick) {
      PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
      nout++;
    }
    SEXP ill = R_NilValue;
    int do_ill = *(INTEGER(AS_INTEGER(Ill)));
    if (do_ill) {
      PROTECT(ill = NEW_CHARACTER(ntimes)); nprotect++;
      nout++;
    }

    int stat = *(INTEGER(AS_INTEGER(Stat)));

    int *xc = INTEGER(count);
    double *xt = REAL(times);

    int n = na;                 // population size
    if (!isNull(N)) {
      n = *(INTEGER(AS_INTEGER(N)));
    }

    double mu = R_NaReal;       // Moran rate
    if (!isNull(Mu)) {
      mu = *(REAL(AS_NUMERIC(Mu)));
    }

    GetRNGstate();
      
    if (isNull(State)) {        // a fresh GP

      t = *(REAL(AS_NUMERIC(T0)));
      gp = new moran_tableau_t(n,mu,t,stat);
      gp->valid();

    }  else {              // restart the GP from the specified state

      gp = new moran_tableau_t(RAW(State));
      gp->valid();
      t = gp->time();
      // optionally override the stored parameters
      if (!isNull(N)) gp->popsize(n);
      if (!isNull(Mu)) gp->moran_rate(mu);
      
    }

    if (t > xt[0]) err("must not have t0 = %lg > %g = times[1]!",t,xt[0]);

    for (int k = 0; k < ntimes; k++, xc++, xt++) {
      *xc = gp->play(*xt);
      if (do_sample) gp->sample();
      if (do_newick) newick(tree,k,*gp);
      if (do_ill) illustrate(ill,k,*gp);
      R_CheckUserInterrupt();
    }
      
    gp->valid();
    
    // pack everything up in a list
    int k = 0;
    SEXP out, outnames;
    PROTECT(out = NEW_LIST(nout)); nprotect++;
    PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
    k = set_list_elem(out,outnames,times,"time",k);
    k = set_list_elem(out,outnames,count,"count",k);
    if (do_newick) {
      k = set_list_elem(out,outnames,tree,"tree",k);
    }
    if (do_ill) {
      k = set_list_elem(out,outnames,ill,"illustration",k);
    }
    k = set_list_elem(out,outnames,serial(*gp),"state",k);
    SET_NAMES(out,outnames);

    delete gp;

    PutRNGstate();

    UNPROTECT(nprotect);
    return out;
  }

  // extract/compute basic information.
  SEXP get_Moran_info (SEXP X, SEXP Prune) {
    return get_info<moran_tableau_t>(X,Prune);
  }

}
