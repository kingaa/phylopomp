// -*- C++ -*-
//! \mainpage Source code for Markov genealogy process simulators
//!
//! Markov genealogy processes are described in the 2022 *Theoretical Population Biology* paper ["Markov genealogy processes"](https://doi.org/10.1016/j.tpb.2021.11.003).
//! These codes are part of the **phylopomp** package for **R**.
//! See the [package manual](https://kingaa.github.io/manuals/phylopomp/) for more information.
//!

#ifndef _MASTER_H_
#define _MASTER_H_

#include <string>
#include <cstring>
#include "popul_proc.h"
#include "genealogy.h"
#include "internal.h"

//! Encodes the master process.

//! This consists of a population process and a genealogy process.
template <class POPN, size_t NDEME = 1>
class master_t : public POPN {

public:

  typedef POPN popul_t;
  const static size_t ndeme = NDEME;

protected:
  // Forward virtual functions to the population process
  void get_state_elements(size_t i, double *time, int *intg, double *dbl) const override {
    popul_t::get_state_elements(i, time, intg, dbl);
  }

  size_t n_integer_elements() const override {
    return popul_t::n_integer_elements();
  }

  size_t n_double_elements() const override {
    return popul_t::n_double_elements();
  }

  const char** integer_names() const override {
    return popul_t::integer_names();
  }

  const char** double_names() const override {
    return popul_t::double_names();
  }

public:
  // DATA MEMBERS
  genealogy_t geneal;
  inventory_t<ndeme> inventory;

public:
  //! size of serialized binary form
  size_t bytesize (void) const {
    return popul_t::bytesize() + geneal.bytesize();
  };
  //! binary serialization
  friend raw_t* operator>> (const master_t& A, raw_t* o) {
    o = (reinterpret_cast<const popul_t&>(A) >> o);
    o = (A.geneal >> o);
    return o;
  }
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, master_t& A) {
    A.clean();
    o = (o >> reinterpret_cast<popul_t&>(A));
    o = (o >> A.geneal);
    A.inventory = A.geneal.extant();
    return o;
  }

private:

  void clean (void) { };

public:
  // CONSTRUCTORS, ETC.
  //! basic constructor
  //!  t0 = initial time
  master_t (double t0 = 0) : popul_t(t0), geneal(t0,0,ndeme) {};
  //! constructor from serialized binary form
  master_t (raw_t *o) {
    o >> *this;
  };
  //! constructor from RAW SEXP (containing binary serialization)
  master_t (SEXP o) {
    if (LENGTH(o)==0)
      err("in %s (%s line %d): cannot deserialize a NULL.",
          __func__,__FILE__,__LINE__);
    PROTECT(o = AS_RAW(o));
    RAW(o) >> *this;
    UNPROTECT(1);
  };
  //! copy constructor
  master_t (const master_t& A) {
    raw_t *o = new raw_t[A.bytesize()];
    A >> o;
    o >> *this;
    delete[] o;
  };
  //! copy assignment operator
  master_t & operator= (const master_t& A) {
    clean();
    raw_t *o = new raw_t[A.bytesize()];
    A >> o;
    o >> *this;
    delete[] o;
    return *this;
  };
  //! move constructor
  master_t (master_t &&) = default;
  //! move assignment operator
  master_t & operator= (master_t &&) = default;
  //! destructor
  ~master_t (void) {
    clean();
  };
  //! get zero-time
  slate_t timezero (void) const {
    return geneal.timezero();
  };
  //! runs the process to time `tfin`
  int play (double tfin) {
    int count = popul_t::play(tfin);
    geneal.time() = tfin;
    return count;
  };

public:
  //! current time
  slate_t time (void) const {
    return popul_t::time();
  };
  //! human-readable info
  std::string describe (void) const {
    return geneal.describe();
  };
  //! machine/human readable info
  std::string yaml (std::string tab = "") const {
    std::string t = tab + "  ";
    std::string s = popul_t::yaml(tab)
      + "genealogy:\n" + geneal.yaml(t);
    return s;
  };
  //! tree in Newick format
  std::string newick (void) const {
    return geneal.newick();
  };
  //! lineage count table
  SEXP lineage_count (void) const {
    return geneal.lineage_count();
  };
  //! structure in R list format
  SEXP structure (void) const {
    return geneal.structure();
  };
  //! get states
  SEXP get_states() const {
    return popul_t::format_states();
  }

public:
  //! n births into deme j with parent in deme i
  void birth (name_t i = 0, name_t j = 0, int n = 1) {
    ball_t *a = inventory.random_ball(i);
    ball_t *b = geneal.birth(a,time(),j);
    inventory.insert(b);
    while (n > 1) {
      b = geneal.birth(b->holder(),j);
      inventory.insert(b);
      n--;
    }
  };
  //! death in deme i
  void death (name_t i = 0) {
    ball_t *a = inventory.random_ball(i);
    inventory.erase(a);
    geneal.death(a,time());
  };
  //! new root in deme i
  void graft (name_t i = 0, int m = 1) {
    for (int j = 0; j < m; j++) {
      ball_t *a = geneal.graft(time(),i);
      inventory.insert(a);
    }
  };
  //! sample in deme i
  void sample (name_t i = 0, int n = 1) {
    if (n > 0) {
      pocket_t *p = inventory.random_balls(i,n);
      for (ball_it a = p->begin(); a != p->end(); a++) {
        geneal.sample(*a,time());
      }
      p->clear();
      delete p;
    }
  };
  //! sample_death in deme i
  void sample_death (name_t i = 0, int n = 1) {
    if (n > 0) {
      pocket_t *p = inventory.random_balls(i,n);
      for (ball_it a = p->begin(); a != p->end(); a++) {
        inventory.erase(*a);
        geneal.sample_death(*a,time());
      }
      p->clear();
      delete p;
    }
  };
  //! migration from deme i to deme j
  void migrate (name_t i = 0, name_t j = 0) {
    ball_t *a = inventory.random_ball(i);
    inventory.erase(a);
    geneal.migrate(a,time(),j);
    a->deme() = j;
    inventory.insert(a);
  };
  void rinit (void) override;  // Add override
  void jump (int e) override;  // Add override
};

#endif
