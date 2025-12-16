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

private:

  void clean (void) {};         // memory cleanup

public:

  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    return 2*sizeof(slate_t) + sizeof(size_t)
      + sizeof(state_t) + sizeof(parameters_t);
  };
  //! binary serialization
  friend raw_t* operator>> (const popul_proc_t &X, raw_t *o) {
    slate_t A[2]; A[0] = X.current; A[1] = X.next;
    memcpy(o,A,sizeof(A)); o += sizeof(A);
    memcpy(o,&X.event,sizeof(size_t)); o += sizeof(size_t);
    memcpy(o,&X.state,sizeof(state_t)); o += sizeof(state_t);
    memcpy(o,&X.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t *o, popul_proc_t &X) {
    X.clean();
    slate_t A[2];
    memcpy(A,o,sizeof(A)); o += sizeof(A);
    X.current = A[0]; X.next = A[1];
    memcpy(&X.event,o,sizeof(size_t)); o += sizeof(size_t);
    memcpy(&X.state,o,sizeof(state_t)); o += sizeof(state_t);
    memcpy(&X.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o;
  };

public:
  // CONSTRUCTORS, ETC.
  //! basic constructor for popul_proc class
  //!  t0 = initial time
  popul_proc_t (double t0 = 0) {
    clean();
    next = current = slate_t(t0);
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

public:

  //! updates clock and next event
  void update_clocks (void) {
    double rate[nevent];
    double total_rate = event_rates(rate,nevent);
    if (R_FINITE(total_rate)) {
      if (total_rate > 0) {
        next = current+rexp(1/total_rate);
      } else {
        next = R_PosInf;
      }
    } else {
      for (event = 0; event < nevent; event++) {
        if (!R_FINITE(rate[event]))
          Rprintf("in '%s' (%s line %d): invalid event rate[%zd]=%lg\n",
                  __func__,__FILE__,__LINE__,event,rate[event]);
      }
      err("in '%s' (%s line %d): invalid total event rate=%lg",
          __func__,__FILE__,__LINE__,total_rate);
    }
    double u = runif(0,total_rate);
    event = 0;
    while (u > rate[event] && event < nevent) {
      if (rate[event] < 0)
        err("in '%s' (%s line %d): invalid rate[%zd]=%lg", // #nocov
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
    while (next < tfin) {
      current = next;
      jump(event);
      update_clocks();
      count++;
      R_CheckUserInterrupt();
    }
    if (next > tfin) current = tfin; // relies on Markov property
    return count;
  };

};

#define PARAM_SET(X) if (!ISNA(p[m])) params.X = p[m];  \
  m++;
#define RATE_CALC(X) total += rate[m++] = (X);
#define YAML_PARAM(X) (t + #X + ": " + std::to_string(params.X) + "\n")
#define YAML_STATE(X) (t + #X + ": " + std::to_string(state.X) + "\n")

#endif
