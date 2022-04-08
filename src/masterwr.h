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
#include "Rmath.h"
#include "internal.h"
#include "popul_proc.h"
#include "genealogy.h"

//! Encodes the master process.

//! This consists of a population process and a genealogy process.
template <class POPN, size_t NDEME = 1, size_t NSEG = 1>
class master_t : public POPN {

public:

  typedef POPN popul_t;
  const static size_t ndeme = NDEME;
  const static size_t nseg = NSEG;

public:
  // DATA MEMBERS
  // genealogy_t<ndeme>* geneal;
  genealogy_t<ndeme> geneal[nseg];
  inventory_t<ndeme> inventory[nseg];

public:
  //! size of serialized binary form
  size_t bytesize (void) const {
    size_t bsize = popul_t::bytesize();
    for (name_t s = 0; s < nseg; s++)
      bsize += geneal[s].bytesize();
    return bsize;
  };
  //! binary serialization
  friend raw_t* operator>> (const master_t& A, raw_t* o) {
    for (name_t s = 0; s < nseg; s++)
      A.geneal[s] >> o;
    reinterpret_cast<const popul_t&>(A) >> o;
    return o;
  }
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, master_t& A) {
    A.clean();
    o = (o >> reinterpret_cast<popul_t&>(A));
    for (name_t s = 0; s < A.nseg; s++) {
      o = (o >> A.geneal[s]);
      A.inventory[s] = A.geneal[s].extant();
    }
    return o;
  }

private:
  
  void clean (void) { };

public:
  // CONSTRUCTORS, ETC.
  //! basic constructor
  //!  t0 = initial time
  master_t (double t0 = 0) : popul_t(t0) {
    for (name_t s = 0; s < nseg; s++) {
      geneal[s] = genealogy_t<ndeme>(t0);
    }
  };
  //! constructor from serialized binary form
  master_t (raw_t *o) {
    o >> *this;
  };
  //! constructor from RAW SEXP (containing binary serialization)
  master_t (SEXP o) {
    PROTECT(o = AS_RAW(o));
    RAW(o) >> *this;
    UNPROTECT(1);
  };
  //! copy constructor
  master_t (const master_t& A) {
    raw_t *o = new raw_t[A.bytesize()];
    A >> o >> *this;
    delete[] o;
  };
  //! copy assignment operator
  master_t & operator= (const master_t& A) {
    clean();
    raw_t *o = new raw_t[A.bytesize()];
    A >> o >> *this;
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
  //! runs the process to time `tfin`
  int play (double tfin) {
    int count = popul_t::play(tfin);
    for (name_t s = 0; s < nseg; s++)
      geneal[s].time() = tfin;
    return count;
  };

public:
  //! current time
  slate_t time (void) const {
    return popul_t::time();
  };
  //! human-readable info for segment s
  std::string describe (name_t s = 0) const {
    return geneal[s].describe();
  };
  //! machine/human readable info
  std::string yaml (std::string tab = "") const {
    std::string t = tab + "  ";
    std::string str = popul_t::yaml(tab) + "genealogy:\n";
    for (name_t s = 0; s < nseg; s++)
      str += tab + geneal[s].yaml(t) + "\n";
    return str;
  };
  //! tree in segment s in Newick format
  std::string newick (bool compact = true, name_t s = 0) const {
    return geneal[s].newick(compact);
  };
  //! lineage count table of segment s
  SEXP lineage_count (name_t s = 0) const {
    return geneal[s].lineage_count();
  };
  //! structure in R list format of segment s
  SEXP structure (name_t s = 0) const {
    return geneal[s].structure();
  };

public:
  //! n births into deme j with parent in deme i
  void birth (name_t i = 0, name_t j = 0, int n = 1) {
    //! draw one index
    name_t ind = random_integer((inventory[0])[i].size());
    for (name_t s = 0; s < nseg; s++) {
      ball_t *a = inventory[s].get_ball(ind, i);
      ball_t *b = geneal[s].birth(a,time(),j);
      if (s < 1)  inventory[s].insert(b);
      while (n > 1) {
        b = geneal[s].birth(b->holder(),j);
        if (s < 1)  inventory[s].insert(b);
        n--;
      }
    }
  };
  //! death in deme i
  void death (name_t i = 0) {
    //! draw one index
    name_t ind = random_integer((inventory[0])[i].size());
    for (name_t s = 0; s < nseg; s++) {
      ball_t *a = inventory[s].get_ball(ind, i);
      inventory[s].erase(a);
      geneal[s].death(a,time());
    }
  };
  //! new root in deme i
  void graft (name_t i = 0, int m = 1) {
    for (int j = 0; j < m; j++) {
      for (name_t s = 0; s < nseg; s++) {
        ball_t *a = geneal[s].graft(time(),i);
        inventory[s].insert(a);
      }
    }
  };
  //! sample in deme i
  void sample (name_t i = 0) {
    //! draw one index
    name_t ind = random_integer((inventory[0])[i].size());
    for (name_t s = 0; s < nseg; s++) {
      ball_t *a = inventory[s].get_ball(ind, i);
      geneal[s].sample(a,time());
    }
  };
  //! migration from deme i to deme j
  void migrate (name_t i = 0, name_t j = 0) {
    //! draw one index
    name_t ind = random_integer((inventory[0])[i].size());
    for (name_t s = 0; s < nseg; s++) {
      ball_t *a = inventory[s].get_ball(ind, i);
      inventory[s].erase(a);
      geneal[s].migrate(a,time(),j);
      a->deme() = j;
      inventory[s].insert(a);
    }
  };
  //! reassort between deme i and deme j in segment s
  void reassort (name_t i = 0, name_t j = 0, name_t s = 0) {
    name_t ind1 = random_integer((inventory[0])[i].size());
    name_t ind2 = random_integer((inventory[0])[j].size());
    if ((i == j) && (ind1 == ind2)) {
      if (ind1 < 1) {
        ind2++;
      } else {
        ind2--;
      }
    }
    ball_t *a = inventory[s].get_ball(ind1, i);
    ball_t *b = inventory[s].get_ball(ind2, i);
    geneal[s].reassort(a,b,time());
  }
  //! initialize the state
  void rinit (void);
  //! makes a jump
  void jump (int e);
};

#endif
