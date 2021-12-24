// -*- C++ -*-
// Master Process

#ifndef _MASTER_H_
#define _MASTER_H_

#include <string>
#include <cstring>
#include "internal.h"
#include "popul_proc.h"
#include "genealogy.h"

// MASTER CLASS
template <class POPN, size_t NDEME = 1>
class master_t : public POPN {

public:

  typedef POPN popul_t;
  const static size_t ndeme = NDEME;

public:
  // DATA MEMBERS
  genealogy_t<ndeme> geneal;
  inventory_t<ndeme> inventory;

public:
  // size of serialized binary form
  size_t bytesize (void) const {
    return popul_t::bytesize() + geneal.bytesize();
  };
  // binary serialization
  raw_t* serialize (raw_t* o) const {
    o = popul_t::serialize(o);
    o = geneal.serialize(o);
    return o;
  }
  // binary deserialization
  raw_t* deserialize (raw_t* o) {
    o = popul_t::deserialize(o);
    o = geneal.deserialize(o);
    inventory = geneal.extant();
    return o;
  }
  friend raw_t* operator<< (raw_t* o, const master_t& A) {
    return A.serialize(o);
  }
  friend raw_t* operator>> (raw_t* o, master_t& A) {
    return A.deserialize(o);
  }

private:
  
  void clean (void) { };

public:
  // CONSTRUCTORS, ETC.
  // basic constructor
  //  t0 = initial time
  master_t (double t0 = 0) : popul_t(t0), geneal(t0) {};
  // constructor from serialized binary form
  master_t (raw_t *o) {
    o >> *this;
  };
  // copy constructor
  master_t (const master_t& A) {
    raw_t *o = new raw_t[A.bytesize()];
    (o << A) >> *this;
    delete[] o;
  };
  // copy assignment operator
  master_t & operator= (const master_t& A) {
    clean();
    raw_t *o = new raw_t[A.bytesize()];
    (o << A) >> *this;
    delete[] o;
    return *this;
  };
  // move constructor
  master_t (master_t &&) = delete;
  // move assignment operator
  master_t & operator= (master_t &&) = delete;
  // destructor
  ~master_t (void) {
    clean();
  };

  //reset current time
  void time (const double& t) {
    popul_t::time(t);
    geneal.time(t);
  };

  int play (double tfin) {
    int count = popul_t::play(tfin);
    geneal.time(tfin);
    return count;
  };

public:
  // current time
  slate_t time (void) const {
    return popul_t::time();
  };
  // human-readable info
  std::string describe (void) const {
    return geneal.describe();
  };
  // machine/human readable info
  std::string yaml (std::string tab = "") const {
    return popul_t::yaml(tab) + geneal.yaml(tab);
  };
  // tree in Newick format
  std::string newick (bool compact = true) const {
    return geneal.newick(compact);
  };
  // lineage count table
  size_t lineage_count (double *t = 0, int *ell = 0) const {
    return geneal.lineage_count(t,ell);
  };
  // structure in R list format
  SEXP structure (void) const {
    return geneal.structure();
  };

public:
  
  // n births into deme j with parent in deme i
  void birth (name_t i = 0, name_t j = 0, int n = 1) {
    ball_t *a = inventory.random_ball(i);
    ball_t *b = geneal.birth(a,time(),j);
    inventory.insert(b);
  };
  
  // death in deme i
  void death (name_t i = 0) {
    ball_t *a = inventory.random_ball(i);
    inventory.erase(a);
    geneal.death(a,time());
  };
  
  // new root in deme i
  void graft (name_t i = 0) {
    ball_t *a = geneal.graft(time(),i);
    inventory.insert(a);
  };

  // sample in deme i
  void sample (name_t i = 0) {
    ball_t *a = inventory.random_ball(i);
    geneal.sample(a,time());
  };
  
  // migration from deme i to deme j
  void migrate (name_t i = 0, name_t j = 0) {
    ball_t *a = inventory.random_ball(i);
    inventory.erase(a);
    geneal.migrate(a,time(),j);
    a->deme = j;
    inventory.insert(a);
  };

};

#endif
