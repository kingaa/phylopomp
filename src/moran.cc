// Moran Genealogy Process with Sampling Simulator (C++)

#include "gp.h"

typedef struct { } moran_state_t;

class moran_tableau_t : public gp_tableau_t<moran_state_t,false> {

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

  double immig_rate (state_t &s) const {
    return 0;
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
    gp_tableau_t(t0) {
    params.n = n;
    params.mu = mu;
    if (stationary) {
      double scale = choose(double(n),2)/mu;
      std::vector<double> times(n,t0);
      for (int j = 1; j < n; j++)
	times[j] = times[j-1]+rexp(scale/choose(double(j+1),2));
      for (int j = 0; j < n; j++)
	times[j] += t0-times[n-1];
      for (int j = 1; j < n; j++)
	if (times[j] <= times[j-1]) err("yowzer!");
      time(times[0]);
      graft();
      for (int j = 1; j < n; j++) {
	time(times[j]);
	birth(random_black_ball());
      }
    } else {
      for (int j = 0; j < n; j++) graft();
    }
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
    if (params.n <= 0) err("'n' must be positive!");
    if (params.mu <= 0) err("'mu' must be positive!");
    if (clock() < time()) err("invalid clock");
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
    death(random_black_ball());
    birth(random_black_ball());
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

  // Sampled Moran genealogy process.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  SEXP playMoran (SEXP N, SEXP Mu, SEXP Times, SEXP T0, SEXP Tree, SEXP Stat, SEXP State) {
    int nprotect = 0;
    int nout = 3;
    double t = R_NaReal;
    int ntimes = LENGTH(Times);
    moran_tableau_t *mgp;

    SEXP times, count;
    PROTECT(times = AS_NUMERIC(duplicate(Times))); nprotect++;
    PROTECT(count = NEW_INTEGER(ntimes)); nprotect++;

    SEXP tree = R_NilValue;
    int do_newick = *(INTEGER(AS_INTEGER(Tree)));
    if (do_newick) {
      PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
      nout++;
    }

    int stat = *(INTEGER(AS_INTEGER(Stat)));

    int *xc = INTEGER(count);
    double *xt = REAL(times);

    int n = na;                 // population size
    if (!isNull(N)) {
      n = *(INTEGER(AS_INTEGER(N)));
    }

    double mu = R_NaReal;     // Moran rate
    if (!isNull(Mu)) {
      mu = *(REAL(AS_NUMERIC(Mu)));
    }

    GetRNGstate();
      
    if (isNull(State)) {        // a fresh MGP

      t = *(REAL(AS_NUMERIC(T0)));
      mgp = new moran_tableau_t(n,mu,t,stat);
      mgp->valid();

    }  else {              // restart the MGP from the specified state

      mgp = new moran_tableau_t(RAW(State));
      mgp->valid();
      t = mgp->time();
      // optionally override the stored parameters
      if (!isNull(N)) mgp->popsize(n);
      if (!isNull(Mu)) mgp->moran_rate(mu);
      
    }

    if (t > xt[0]) err("must not have t0 = %lg > %g = times[1]!",t,xt[0]);

    for (int k = 0; k < ntimes; k++, xc++, xt++) {
      *xc = mgp->play(*xt);
      mgp->sample();
      if (do_newick) {
        moran_tableau_t U = *mgp;
        newick(tree,k,U,true);
      }
      R_CheckUserInterrupt();
    }
      
    PutRNGstate();

    mgp->valid();
    
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
    k = set_list_elem(out,outnames,serial(*mgp),"state",k);
    SET_NAMES(out,outnames);

    delete mgp;

    UNPROTECT(nprotect);
    return out;
  }

  // extract/compute basic information.
  SEXP get_Moran_info (SEXP X, SEXP Prune, SEXP Tree) {
    int nprotect = 0;
    int nout = 5;

    // reconstruct the tableau from its serialization
    moran_tableau_t mgp(RAW(X));
    // check validity
    mgp.valid();
    
    // extract current time
    SEXP tout;
    PROTECT(tout = NEW_NUMERIC(1)); nprotect++;
    *REAL(tout) = mgp.time();

    // extract cumulative hazards
    SEXP cumhaz;
    PROTECT(cumhaz = walk(mgp)); nprotect++;
    nout++;
    
    // prune if requested
    if (*(INTEGER(AS_INTEGER(Prune)))) mgp.prune();

    SEXP tree;
    if (*(INTEGER(AS_INTEGER(Tree)))) {
      PROTECT(tree = newick(mgp,false)); nprotect++;
      nout++;
    }

    // pack up return values in a list
    int k = 0;
    SEXP out, outnames;
    PROTECT(out = NEW_LIST(nout)); nprotect++;
    PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
    k = set_list_elem(out,outnames,tout,"time",k);
    if (*(INTEGER(AS_INTEGER(Tree)))) {
      k = set_list_elem(out,outnames,newick(mgp,false),"tree",k);
    }
    k = set_list_elem(out,outnames,describe(mgp),"description",k);
    k = set_list_elem(out,outnames,get_epochs(mgp),"epochs",k);
    k = set_list_elem(out,outnames,get_times(mgp),"etimes",k);
    k = set_list_elem(out,outnames,get_lineage_count(mgp),"lineages",k);
    k = set_list_elem(out,outnames,cumhaz,"cumhaz",k);
    SET_NAMES(out,outnames);

    UNPROTECT(nprotect);
    return out;
  }

}
