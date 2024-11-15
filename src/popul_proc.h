// -*- C++ -*-
// POPUL_PROC class

#ifndef _POPUL_PROC_H_
#define _POPUL_PROC_H_

#include <string>
#include <cstring>

#include "internal.h"

//! Population process class.

//! The class for the simulation of the Markov process.
//! - STATE is a datatype that holds the state of the Markov process.
//! - PARAMETERS is a datatype for the model parameters
//! - NEVENT is the number of event-types
//! - NDEME is the number of demes
template <class STATE, class PARAMETERS, size_t NEVENT, size_t NDEME = 1>
class popul_proc_t  {

protected:
  // TYPE DEFINITIONS
  typedef STATE state_t;
  typedef PARAMETERS parameters_t;
  static const size_t nevent = NEVENT;
  static const size_t ndeme = NDEME;

protected:
  // MEMBER DATA
  slate_t next;                 // time of next event
  size_t event;                 // mark of next event
  slate_t current;              // current time
  state_t state;                // current state
  parameters_t params;          // model parameters
  slate_t nextout;              // time of next output
  std::vector<state_t> state_history; // vector to store state history
  std::vector<slate_t> time_history;  // vector to store output times

  virtual void get_state_elements(size_t i, double *time, int *intg, double *dbl) const = 0;
  virtual size_t n_integer_elements() const = 0;
  virtual size_t n_double_elements() const = 0;
  virtual const char** integer_names() const = 0;
  virtual const char** double_names() const = 0;

private:

  void clean (void) {
    state_history.clear();
    time_history.clear();
  };         // memory cleanup

public:

  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    return 3*sizeof(slate_t) + sizeof(size_t)
      + sizeof(state_t) + sizeof(parameters_t)
      + sizeof(size_t)
      + state_history.size() * sizeof(state_t)
      + time_history.size() * sizeof(slate_t);
  };
  //! binary serialization
  friend raw_t* operator>> (const popul_proc_t &X, raw_t *o) {
    slate_t A[3]; A[0] = X.current; A[1] = X.next; A[2] = X.nextout;
    memcpy(o,A,sizeof(A)); o += sizeof(A);
    memcpy(o,&X.event,sizeof(size_t)); o += sizeof(size_t);
    memcpy(o,&X.state,sizeof(state_t)); o += sizeof(state_t);
    memcpy(o,&X.params,sizeof(parameters_t)); o += sizeof(parameters_t);

    size_t size = X.state_history.size();
    memcpy(o, &size, sizeof(size_t)); o += sizeof(size_t);
    memcpy(o, X.state_history.data(), size * sizeof(state_t));
    o += size * sizeof(state_t);
    memcpy(o, X.time_history.data(), size * sizeof(slate_t));
    o += size * sizeof(slate_t);

    return o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t *o, popul_proc_t &X) {
    X.clean();
    slate_t A[3];
    memcpy(A,o,sizeof(A)); o += sizeof(A);
    X.current = A[0]; X.next = A[1]; X.nextout = A[2];
    memcpy(&X.event,o,sizeof(size_t)); o += sizeof(size_t);
    memcpy(&X.state,o,sizeof(state_t)); o += sizeof(state_t);
    memcpy(&X.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);

    size_t size;
    memcpy(&size, o, sizeof(size_t)); o += sizeof(size_t);
    X.state_history.resize(size);
    X.time_history.resize(size);
    memcpy(X.state_history.data(), o, size * sizeof(state_t));
    o += size * sizeof(state_t);
    memcpy(X.time_history.data(), o, size * sizeof(slate_t));
    o += size * sizeof(slate_t);

    return o;
  };

public:
  // CONSTRUCTORS, ETC.
  //! basic constructor for popul_proc class
  //!  t0 = initial time
  popul_proc_t (double t0 = 0) {
    clean();
    next = current = nextout = slate_t(t0);
    event = 0;
  };
  //! constructor from serialized binary form
  popul_proc_t (raw_t *o) {
    o >> *this;
  };
  //! copy constructor
  popul_proc_t (const popul_proc_t & X) {
    raw_t *o = new raw_t[X.bytesize()];
    X >> o;
    o >> *this;
    delete[] o;
  };
  //! copy assignment operator
  popul_proc_t & operator= (const popul_proc_t & X) {
    clean();
    raw_t *o = new raw_t[X.bytesize()];
    X >> o;
    o >> *this;
    delete[] o;
    return *this;
  };
  //! move constructor
  popul_proc_t (popul_proc_t &&) = delete;
  //! move assignment operator
  popul_proc_t & operator= (popul_proc_t &&) = delete;
  //! destructor
  ~popul_proc_t (void) {
    clean();
  };

public:

  // INFORMATION EXTRACTORS
  //! get current time.
  slate_t time (void) const {
    return current;
  };
  //! get state size
  size_t n_states() const {
    return state_history.size();
  }

public:

  virtual void valid (void) const {};

public:

  //! set parameters
  void update_params (double*, int);
  //! set initial-value parameters
  void update_IVPs (double*, int);
  //! compute event rates
  double event_rates (double *rate, int n) const;
  //! initialize the state
  virtual void rinit (void) = 0;
  //! makes a jump
  virtual void jump (int e) = 0;
  //! machine/human readable info
  std::string yaml (std::string tab) const;
  //! get state history and time as SEXP vector
  SEXP format_states() const {
    size_t n = time_history.size();

    if (n == 0 || n > 1000000) {
      Rprintf("Invalid size: %zu\n", n);
      return R_NilValue;
    }

    size_t n_int = n_integer_elements();
    size_t n_dbl = n_double_elements();
    size_t total_cols = 1 + n_int + n_dbl;  // time + integers + doubles

    int protect_count = 0;  // Keep track of protections

    // Create output list
    SEXP out = PROTECT(Rf_allocVector(VECSXP, total_cols)); protect_count++;

    // Create time column
    SEXP time_col = PROTECT(Rf_allocVector(REALSXP, n)); protect_count++;
    SET_VECTOR_ELT(out, 0, time_col);

    // Create integer columns
    std::vector<SEXP> int_cols(n_int);
    for(size_t j = 0; j < n_int; j++) {
      int_cols[j] = PROTECT(Rf_allocVector(INTSXP, n)); protect_count++;
      SET_VECTOR_ELT(out, j+1, int_cols[j]);
    }

    // Create double columns
    std::vector<SEXP> dbl_cols(n_dbl);
    for(size_t j = 0; j < n_dbl; j++) {
      dbl_cols[j] = PROTECT(Rf_allocVector(REALSXP, n)); protect_count++;
      SET_VECTOR_ELT(out, j+n_int+1, dbl_cols[j]);
    }

    // Create names
    SEXP names = PROTECT(Rf_allocVector(STRSXP, total_cols)); protect_count++;

    // Set column names
    SET_STRING_ELT(names, 0, mkChar("time"));
    const char** int_names = integer_names();
    const char** dbl_names = double_names();

    for(size_t j = 0; j < n_int; j++) {
      SET_STRING_ELT(names, j+1, mkChar(int_names[j]));
    }
    for(size_t j = 0; j < n_dbl; j++) {
      SET_STRING_ELT(names, j+n_int+1, mkChar(dbl_names[j]));
    }

    // Fill data
    std::vector<int> int_vals(n_int);
    std::vector<double> dbl_vals(n_dbl);
    double time;

    for(size_t i = 0; i < n; i++) {
      get_state_elements(i, &time, int_vals.data(), dbl_vals.data());

      REAL(time_col)[i] = time;

      for(size_t j = 0; j < n_int; j++) {
        INTEGER(int_cols[j])[i] = int_vals[j];
      }
      for(size_t j = 0; j < n_dbl; j++) {
        REAL(dbl_cols[j])[i] = dbl_vals[j];
      }
    }
    Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(protect_count);
    return out;
  }

public:

  //! updates clock and next event
  void update_clocks (void) {
    double rate[nevent];
    double total_rate = event_rates(rate,nevent);
    if (total_rate > 0) {
      next = current+rexp(1/total_rate);
    } else {
      next = R_PosInf;
    }
    double u = runif(0,total_rate);
    event = 0;
    while (u > rate[event] && event < nevent) {
      if (rate[event] < 0)
        err("in '%s' (%s line %d): invalid negative rate[%zd]=%lg", // #nocov
            __func__,__FILE__,__LINE__,event,rate[event]); // #nocov
      u -= rate[event];
      event++;
    }
    assert(event < nevent);
  };

  //! run process to a specified time.
  //! return number of events that have occurred.
  int play (double tfin) {
    int count = 0;
    if (current > tfin)
      err("cannot simulate backward! (current t=%lg, requested t=%lg)",current,tfin);

    if (time_history.empty()) {
      time_history.push_back(current);
      state_history.push_back(state);
      nextout = current + params.dt;
    }

    while (next < tfin) {
      while (nextout <= next && nextout <= tfin) {
        time_history.push_back(nextout);
        state_history.push_back(state);
        nextout += params.dt;
      }

      current = next;
      jump(event);
      update_clocks();
      count++;
      R_CheckUserInterrupt();
    }

    while (nextout <= tfin) {
      time_history.push_back(nextout);
      state_history.push_back(state);
      nextout += params.dt;
    }

    if (next > tfin) current = tfin;
    return count;
  };
};

#define PARAM_SET(X) if (!ISNA(p[m])) params.X = p[m];  \
  m++;
#define RATE_CALC(X) total += rate[m++] = (X);
#define YAML_PARAM(X) (t + #X + ": " + std::to_string(params.X) + "\n")
#define YAML_STATE(X) (t + #X + ": " + std::to_string(state.X) + "\n")

#endif
