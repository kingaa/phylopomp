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
  state_t state;

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

protected:

  size_t size (void) const {
    return sizeof(parameters_t) + 2*sizeof(state_t) + this->gp_tableau_t::size();
  };

  friend raw_t* operator<< (raw_t *o, const lbdp_tableau_t &T) {
    memcpy(o,&T.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    memcpy(o,&T.state,sizeof(state_t)); o += sizeof(state_t);
    return o << *dynamic_cast<const gp_tableau_t*>(&T);
  };

  friend raw_t* operator>> (raw_t *o, lbdp_tableau_t &T) {
    memcpy(&T.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    memcpy(&T.state,o,sizeof(state_t)); o += sizeof(state_t);
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
    for (int j = 0; j < n0; j++) graft(state);
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
    for (name_t i = 0; i < nplayers(); i++) {
      if (player[i]->state.n < 0 || (player[i]->state.n != floor(player[i]->state.n)))
        err("invalid player state! n=%lg\n%s",player[i]->state.n,player[i]->describe().c_str());
    }
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
      birth(state);
    } else if (nextD < nextB && nextD < nextS) {
      state.n -= 1.0;
      death(state);
    } else if (nextS < nextB && nextS < nextD) {
      sample(state);
    } else {
      err("there's no place like home");
    }
    update_clocks();
  };

  int live (void) const {
    return (state.n > 0);
  }

  // create the serialized state:
  friend SEXP serial (const lbdp_tableau_t &T) {
    SEXP out;
    PROTECT(out = NEW_RAW(T.size()));
    RAW(out) << T;
    UNPROTECT(1);
    return out;
  }

};

extern "C" {

  // BD process
  // optionally compute genealogies in Newick form ('tree = TRUE').
  SEXP playLBDP (SEXP Lambda, SEXP Mu, SEXP Psi, SEXP N0, SEXP Times, SEXP T0, SEXP Tree, SEXP State) {
    int nprotect = 0;
    int nout = 3;
    double t = R_NaReal;
    int ntimes = LENGTH(Times);
    lbdp_tableau_t *gp;

    SEXP times, count;
    PROTECT(times = AS_NUMERIC(duplicate(Times))); nprotect++;
    PROTECT(count = NEW_INTEGER(ntimes)); nprotect++;

    SEXP tree = R_NilValue;
    int do_newick = *(INTEGER(AS_INTEGER(Tree)));
    if (do_newick) {
      PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
      nout++;
    }

    int *xc = INTEGER(count);
    double *xt = REAL(times);

    int n0 = na;                // initial number of infections
    if (!isNull(N0)) {
      n0 = *(INTEGER(AS_INTEGER(N0)));
    }

    double lambda = R_NaReal;   // birth rate
    if (!isNull(Lambda)) {
      lambda = *(REAL(AS_NUMERIC(Lambda)));
    }

    double mu = R_NaReal;       // death rate
    if (!isNull(Mu)) {
      mu = *(REAL(AS_NUMERIC(Mu)));
    }

    double psi = R_NaReal;       // sampling rate
    if (!isNull(Psi)) {
      psi = *(REAL(AS_NUMERIC(Psi)));
    }

    GetRNGstate();
      
    if (isNull(State)) {        // a fresh GP

      t = *(REAL(AS_NUMERIC(T0)));
      gp = new lbdp_tableau_t(lambda,mu,psi,n0,t);
      gp->valid();

    }  else {              // restart the GP from the specified state

      gp = new lbdp_tableau_t(RAW(State));
      gp->valid();
      t = gp->time();
      // optionally override the stored parameters
      if (!isNull(Lambda)) gp->birth_rate(lambda);
      if (!isNull(Mu)) gp->death_rate(mu);
      if (!isNull(Psi)) gp->sampling_rate(psi);
      
    }

    {
      double estsize = n0*exp((gp->birth_rate())*REAL(times)[ntimes-1]);
      if (estsize > 1e6) err("too big!");
    }

    if (t > xt[0]) err("must not have t0 = %lg > %g = times[1]!",t,xt[0]);

    for (int k = 0; k < ntimes; k++, xc++, xt++) {
      if (gp->live()) {
        *xc = gp->play(*xt);
      } else {
        *xc = R_NaInt;
      }
      if (do_newick) {
        lbdp_tableau_t U = *gp;
        newick(tree,k,U);
      }
      R_CheckUserInterrupt();
    }
      
    PutRNGstate();

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
    k = set_list_elem(out,outnames,serial(*gp),"state",k);
    SET_NAMES(out,outnames);
      
    delete gp;

    UNPROTECT(nprotect);
    return out;
  }

  // extract/compute basic information.
  SEXP get_LBDP_info (SEXP X, SEXP Prune, SEXP Tree) {
    int nprotect = 0;
    int nout = 5;

    // reconstruct the tableau from its serialization
    lbdp_tableau_t gp(RAW(X));
    // check validity
    gp.valid();
    
    // extract current time
    SEXP tout;
    PROTECT(tout = NEW_NUMERIC(1)); nprotect++;
    *REAL(tout) = gp.time();

    // extract cumulative hazards
    SEXP cumhaz;
    PROTECT(cumhaz = walk(gp)); nprotect++;
    nout++;
    
    // prune if requested
    if (*(INTEGER(AS_INTEGER(Prune)))) gp.prune();

    SEXP tree;
    if (*(INTEGER(AS_INTEGER(Tree)))) {
      PROTECT(tree = newick(gp)); nprotect++;
      nout++;
    }

    // pack up return values in a list
    int k = 0;
    SEXP out, outnames;
    PROTECT(out = NEW_LIST(nout)); nprotect++;
    PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
    k = set_list_elem(out,outnames,tout,"time",k);
    if (*(INTEGER(AS_INTEGER(Tree)))) {
      k = set_list_elem(out,outnames,newick(gp),"tree",k);
    }
    //    k = set_list_elem(out,outnames,describe(gp),"description",k);
    k = set_list_elem(out,outnames,get_epochs(gp),"epochs",k);
    k = set_list_elem(out,outnames,get_times(gp),"etimes",k);
    k = set_list_elem(out,outnames,get_lineage_count(gp),"lineages",k);
    k = set_list_elem(out,outnames,get_sample_times(gp),"stimes",k);
    k = set_list_elem(out,outnames,cumhaz,"cumhaz",k);
    SET_NAMES(out,outnames);

    UNPROTECT(nprotect);
    return out;
  }

}
