// -*- C++ -*-
// Generic Markov Population Process Simulator (C++)

#ifndef _POPUL_PROC_H_
#define _POPUL_PROC_H_

#include <string>
#include <cstring>

#include "internal.h"

// POPN_PROC CLASS
// the class for the simulation of the Markov process
// - STATE is a datatype that holds the state of the Markov process.
// - PARAMETERS is a datatype for the model parameters
// - NEVENT is the number of event-types
// - NDEME is the number of demes
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
  slate_t next;			// time of next event
  size_t event;			// mark of next event
  slate_t t0;			// initial time
  slate_t current;		// current time
  state_t state;		// current state
  parameters_t params;		// model parameters

private:

  void clean (void) {};		// memory cleanup

public:
  
  // SERIALIZATION
  // size of serialized binary form
  size_t bytesize (void) const {
    return 3*sizeof(slate_t) + sizeof(size_t)
      + sizeof(state_t) + sizeof(parameters_t);
  };
  // binary serialization
  raw_t* serialize (raw_t *o) const {
    slate_t A[3]; A[0] = t0; A[1] = current; A[2] = next;
    memcpy(o,A,sizeof(A)); o += sizeof(A);
    memcpy(o,&event,sizeof(size_t)); o += sizeof(size_t);
    memcpy(o,&state,sizeof(state_t)); o += sizeof(state_t);
    memcpy(o,&params,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o;
  };
  // binary deserialization
  raw_t* deserialize (raw_t *o) {
    clean();
    slate_t A[3];
    memcpy(A,o,sizeof(A)); o += sizeof(A);
    t0 = A[0]; current = A[1]; next = A[2]; 
    memcpy(&event,o,sizeof(size_t)); o += sizeof(size_t);
    memcpy(&state,o,sizeof(state_t)); o += sizeof(state_t);
    memcpy(&params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    return o;
  };
  friend raw_t* operator<< (raw_t *o, const popul_proc_t &X) {
    return X.serialize(o);
  }
  friend raw_t* operator>> (raw_t *o, popul_proc_t &X) {
    return X.deserialize(o);
  }

public:
  // CONSTRUCTORS, ETC.
  // basic constructor for popul_proc class
  //  t0 = initial time
  popul_proc_t (double t0 = 0) {
    clean();
    next = current = t0 = slate_t(t0);
    event = 0;
  };
  // constructor from serialized binary form
  popul_proc_t (raw_t *o) {
    o >> *this;
  };
  // copy constructor
  popul_proc_t (const popul_proc_t & X) {
    raw_t *o = new raw_t[X.bytesize()];
    (o << X) >> *this;
    delete[] o;
  };
  // copy assignment operator
  popul_proc_t & operator= (const popul_proc_t & X) {
    clean();
    raw_t *o = new raw_t[X.bytesize()];
    (o << X) >> *this;
    delete[] o;
    return *this;
  };
  // move constructor
  popul_proc_t (popul_proc_t &&) = delete;
  // move assignment operator
  popul_proc_t & operator= (popul_proc_t &&) = delete;
  // destructor
  ~popul_proc_t (void) {
    clean();
  };

protected:
  //reset current time
  virtual void time (const double& t) {
    current = t;
  };

public:
  
  // INFORMATION EXTRACTORS
  // get current time.
  slate_t time (void) const {
    return current;
  };
  // get zero time.
  slate_t timezero (void) const {
    return t0;
  };

public:

  void valid (void) const {};

protected:
  
  // initialize the state
  virtual void rinit (void) = 0;
  // compute event rates
  virtual double event_rates (double *rate, int n) const = 0;
  // makes a jump
  virtual void jump (int e) = 0;
  // set parameters 
  virtual void update_params (double*, int) = 0;
  // set initial conditions
  virtual void update_ICs (double*, int) = 0;

public:

  // updates clock and next event
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
      if (rate[event] < 0) err("invalid rate[%ld]=%lg",event,rate[event]); // # nocov
      u -= rate[event];
      event++;
    }
    if (event >= nevent) err("invalid event %ld!",event); // # nocov
  };

  // run process to a specified time.
  // return number of events that have occurred.
  virtual int play (double tfin) {
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

#endif
