// SIR with Sampling Genealogy Process Simulator (C++)

#include "gp.h"

typedef struct {
  double S;             // number of susceptibles
  double I;             // number of infections
  double R;             // number of recovereds
} sir_state_t;

const sir_state_t default_state = {R_NaReal,R_NaReal,R_NaReal};

class sirws_tableau_t : public gp_tableau_t<sir_state_t> {

private:

  typedef struct {
    double N;                   // host population size
    double beta;                // transmission rate
    double gamma;               // recovery rate
    double psi;                 // sampling rate
    int S0;                     // initial susceptibles
    int I0;                     // initial infecteds
  } parameters_t;

  parameters_t params;
  sir_state_t state;

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

protected:

  size_t size (void) const {
    return sizeof(parameters_t) + 2*sizeof(state_t) + this->gp_tableau_t::size();
  };

  friend raw_t* operator<< (raw_t *o, const sirws_tableau_t &T) {
    memcpy(o,&T.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    memcpy(o,&T.state,sizeof(state_t)); o += sizeof(state_t);
    return o << *dynamic_cast<const gp_tableau_t*>(&T);
  };

  friend raw_t* operator>> (raw_t *o, sirws_tableau_t &T) {
    memcpy(&T.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    memcpy(&T.state,o,sizeof(state_t)); o += sizeof(state_t);
    return o >> *dynamic_cast<gp_tableau_t*>(&T);
  };

public:

  sirws_tableau_t (void) = default;
  // basic constructor
  sirws_tableau_t (double beta, double gamma, double psi,
                   int S0, int I0, double t0 = 0) : gp_tableau_t(t0) {
    params = {
      double(S0+I0), beta, gamma, psi, S0, I0
    };
    state = {double(S0), double(I0), 0};
    for (name_t j = 0; j < name_t(I0); j++) graft(state);
    update_clocks();
    valid();
  };
  // constructor from serialized binary form
  sirws_tableau_t (raw_t *o) {
    o >> *this;
    update_clocks();
    valid();
  };
  // copy constructor
  sirws_tableau_t (const sirws_tableau_t &T) {
    raw_t *o = new raw_t[T.size()];
    o << T;
    o >> *this;
    delete[] o;
    update_clocks();
    valid();
  };
  // move constructor
  sirws_tableau_t (sirws_tableau_t &&) = delete;
  // copy assignment operator
  sirws_tableau_t & operator= (const sirws_tableau_t &) = delete;
  // move assignment operator
  sirws_tableau_t & operator= (sirws_tableau_t &&) = delete;
  // destructor
  ~sirws_tableau_t (void) = default;

  void valid (void) const {
    this->gp_tableau_t::valid();
    if (params.N <= 0) err("total population size must be positive!");
    if (params.N != state.S+state.I+state.R) err("population leakage!");
    if (clock() < time()) err("invalid clock");
    for (name_t n = 0; n < nplayers(); n++) {
      if (player[n]->state.S+player[n]->state.I+player[n]->state.R != params.N)
	err("invalid player state!\n%s",player[n]->describe().c_str());
    }
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
      next = inf;
    }
    return next;
  };
    
  void move (void) {
    if (nextI < nextR && nextI < nextS) {
      state.S -= 1.0;
      state.I += 1.0;
      birth(state);
    } else if (nextS < nextI && nextS < nextR) {
      sample(state);
    } else if (nextR < nextI && nextR < nextS) {
      state.I -= 1.0;
      state.R += 1.0;
      death(state);
    }
    update_clocks();
  };

  int live (void) const {
    return (state.I > 0);
  }

  // create the serialized state:
  friend SEXP serial (const sirws_tableau_t &T) {
    SEXP out;
    PROTECT(out = NEW_RAW(T.size()));
    RAW(out) << T;
    UNPROTECT(1);
    return out;
  }

};

extern "C" {

  // Sampled SIR process.
  // optionally compute genealogies in Newick form ('tree = TRUE').
  SEXP playSIRwS (SEXP Beta, SEXP Gamma, SEXP Psi, SEXP S0, SEXP I0, SEXP Times, SEXP T0, SEXP Tree, SEXP State) {
    int nprotect = 0;
    int nout = 3;
    double t = R_NaReal;
    int ntimes = LENGTH(Times);
    sirws_tableau_t *gp;

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

    int s0 = na;                // initial susceptible pool
    if (!isNull(S0)) {
      s0 = *(INTEGER(AS_INTEGER(S0)));
    }

    int i0 = na;                // initial number of infections
    if (!isNull(I0)) {
      i0 = *(INTEGER(AS_INTEGER(I0)));
    }

    double beta = R_NaReal;     // transmission rate
    if (!isNull(Beta)) {
      beta = *(REAL(AS_NUMERIC(Beta)));
    }

    double gamma = R_NaReal;    // recovery rate
    if (!isNull(Gamma)) {
      gamma = *(REAL(AS_NUMERIC(Gamma)));
    }

    double psi = R_NaReal;      // sampling rate
    if (!isNull(Psi)) { 
      psi = *(REAL(AS_NUMERIC(Psi)));
    }

    GetRNGstate();
      
    if (isNull(State)) {        // a fresh SIR

      t = *(REAL(AS_NUMERIC(T0)));
      gp = new sirws_tableau_t(beta,gamma,psi,s0,i0,t);
      gp->valid();

    }  else {              // restart the SIR from the specified state

      gp = new sirws_tableau_t(RAW(State));
      gp->valid();
      t = gp->time();
      // optionally override the stored parameters
      if (!isNull(Beta)) gp->transmission_rate(beta);
      if (!isNull(Gamma)) gp->recovery_rate(gamma);
      if (!isNull(Psi)) gp->sample_rate(psi);
      
    }

    if (t > xt[0]) err("must not have t0 = %lg > %g = times[1]!",t,xt[0]);

    for (int k = 0; k < ntimes; k++, xc++, xt++) {
      if (gp->live()) {
        *xc = gp->play(*xt);
      } else {
        *xc = R_NaInt;
      }
      if (do_newick) {
        sirws_tableau_t U = *gp;
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
  SEXP get_SIRwS_info (SEXP X, SEXP Prune, SEXP Tree) {
    int nprotect = 0;
    int nout = 7;

    // reconstruct the tableau from its serialization
    sirws_tableau_t gp(RAW(X));
    // check validity
    gp.valid();
    
    // extract current time
    SEXP tout;
    PROTECT(tout = NEW_NUMERIC(1)); nprotect++;
    *REAL(tout) = gp.time();

    if (*(INTEGER(AS_INTEGER(Tree)))) nout++;

    // prune if requested
    if (*(INTEGER(AS_INTEGER(Prune)))) gp.prune();

    // pack up return values in a list
    int k = 0;
    SEXP out, outnames;
    PROTECT(out = NEW_LIST(nout)); nprotect++;
    PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
    k = set_list_elem(out,outnames,tout,"time",k);
    k = set_list_elem(out,outnames,describe(gp),"description",k);
    k = set_list_elem(out,outnames,get_epochs(gp),"epochs",k);
    k = set_list_elem(out,outnames,get_times(gp),"etimes",k);
    k = set_list_elem(out,outnames,get_lineage_count(gp),"lineages",k);
    k = set_list_elem(out,outnames,get_sample_times(gp),"stimes",k);
    k = set_list_elem(out,outnames,walk(gp),"cumhaz",k);
    if (*(INTEGER(AS_INTEGER(Tree)))) {
      k = set_list_elem(out,outnames,newick(gp),"tree",k);
    }
    SET_NAMES(out,outnames);

    UNPROTECT(nprotect);
    return out;
  }

}
