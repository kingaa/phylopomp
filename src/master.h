// -*- C++ -*-
//! \mainpage Source code for Markov genealogy process simulators with reassortment
//!
//! Extends phylopomp's Markov genealogy processes with multi-segment
//! reassortment support.

#ifndef _MASTER_H_
#define _MASTER_H_

#include <string>
#include <cstring>
#include <list>
#include "popul_proc.h"
#include "genealogy.h"
#include "internal.h"

//! Encodes the master process.

//! This consists of a population process and a genealogy process.
//! NSEG = number of segments (default 1 = no reassortment).
template <class POPN, size_t NDEME = 1, size_t NSEG = 1>
class master_t : public POPN {

public:

  typedef POPN popul_t;
  const static size_t ndeme = NDEME;
  const static size_t nseg = NSEG;
  // extra genealogy slot for initial infection tree when NSEG > 1
  const static size_t ngeneal = (NSEG > 1) ? NSEG + 1 : NSEG;

public:
  // DATA MEMBERS
  genealogy_t geneal[ngeneal];
  inventory_t<ndeme> inventory[ngeneal];

public:
  //! size of serialized binary form
  size_t bytesize (void) const {
    size_t bsize = popul_t::bytesize();
    for (name_t s = 0; s < ngeneal; s++)
      bsize += geneal[s].bytesize();
    return bsize;
  };
  //! binary serialization
  friend raw_t* operator>> (const master_t& A, raw_t* o) {
    o = (reinterpret_cast<const popul_t&>(A) >> o);
    for (name_t s = 0; s < ngeneal; s++)
      o = (A.geneal[s] >> o);
    return o;
  }
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, master_t& A) {
    A.clean();
    o = (o >> reinterpret_cast<popul_t&>(A));
    for (name_t s = 0; s < ngeneal; s++) {
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
  master_t (double t0 = 0) : popul_t(t0) {
    for (name_t s = 0; s < ngeneal; s++) {
      geneal[s] = genealogy_t(t0,0,ndeme);
    }
  };
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
    return geneal[0].timezero();
  };
  //! runs the process to time `tfin`
  int play (double tfin) {
    int count = popul_t::play(tfin);
    for (name_t s = 0; s < ngeneal; s++)
      geneal[s].time() = tfin;
    return count;
  };

public:
  //! check the consistency of multiple inventories
  void valid_invens (std::string e) const {
    if (nseg <= 1) return;
    size_t n;
    name_t s, d;
    ball_t *a, *b;
    for (d = 0; d < ndeme; d++) {
      n = (inventory[0]).size(d);
      for (s = 1; s < ngeneal; s++) {
        if ((inventory[s]).size(d) != n)
          err("Inconsistent sizes between Segment %ld and %ld for deme %ld in event '%s'.",
              (size_t)0, (size_t)s, (size_t)d, e.c_str());
        for (name_t i = 0; i < n; i++) {
          a = inventory[0].get_ball_idx(i,d);
          b = inventory[s].get_ball_idx(i,d);
          if (a->uniq != b->uniq) {
            err("For event %s, in deme %ld, ball %ld: Seg 0: %ld; Seg %ld: %ld.\n",
                e.c_str(), (size_t)d, (size_t)i, a->uniq, (size_t)s, b->uniq);
          }
        }
      }
    }
  };
  //! current time
  slate_t time (void) const {
    return popul_t::time();
  };
  //! human-readable info for segment s
  std::string describe (name_t s = 0) const {
    return geneal[s].describe();
  };
  //! machine/human readable info
  std::string yaml (std::string tab = "", name_t s = 0) const {
    std::string t = tab + "  ";
    std::string str = popul_t::yaml(tab)
      + "genealogy:\n" + geneal[s].yaml(t);
    return str;
  };
  //! tree in segment s in Newick format
  std::string newick (name_t s = 0) const {
    return geneal[s].newick();
  };
  //! tree in segment s in compact Newick format
  std::string compact_newick_str (name_t s = 0) const {
    return geneal[s].compact_newick_str();
  };
  //! lineage count table of segment s
  SEXP lineage_count (name_t s = 0) const {
    return geneal[s].lineage_count();
  };
  //! structure in R list format of segment s
  SEXP structure (name_t s = 0) const {
    return geneal[s].structure();
  };
  //! gendat (data-frame format) of segment s
  SEXP gendat (name_t s = 0) const {
    return geneal[s].gendat();
  };

public:
  //! n births into deme j with parent in deme i
  //! synchronized across all segments
  void birth (name_t i = 0, name_t j = 0, int n = 1) {
    std::string e = "birth";
    int ind = random_integer((inventory[0])[i].size());
    ball_t *a, *b;
    for (name_t s = 0; s < ngeneal; s++) {
      a = inventory[s].get_ball_idx(ind,i);
      b = geneal[s].birth(a,time(),j);
      inventory[s].insert(b);
      int nn = n;
      while (nn > 1) {
        b = geneal[s].birth(b->holder(),j);
        inventory[s].insert(b);
        nn--;
      }
    }
    valid_invens(e);
  };
  //! death in deme i
  void death (name_t i = 0) {
    std::string e = "death";
    int ind = random_integer((inventory[0])[i].size());
    ball_t *a;
    for (name_t s = 0; s < ngeneal; s++) {
      a = inventory[s].get_ball_idx(ind,i);
      inventory[s].erase(a);
      geneal[s].death(a,time());
    }
    valid_invens(e);
  };
  //! new root in deme i
  void graft (name_t i = 0, int m = 1) {
    std::string e = "graft";
    for (int j = 0; j < m; j++) {
      for (name_t s = 0; s < ngeneal; s++) {
        ball_t *a = geneal[s].graft(time(),i);
        inventory[s].insert(a);
      }
    }
    valid_invens(e);
  };
  //! sample in deme i (lineage continues, stays in inventory)
  void sample (name_t i = 0) {
    std::string e = "sample";
    int ind = random_integer((inventory[0])[i].size());
    ball_t *a;
    for (name_t s = 0; s < ngeneal; s++) {
      a = inventory[s].get_ball_idx(ind,i);
      geneal[s].sample(a,time());
    }
    valid_invens(e);
  };
  //! sample with probability p of terminating the lineage.
  //! if terminated: remove from inventory and drop lineage.
  //! if not terminated: keep in inventory, lineage continues.
  //! returns true if the lineage was terminated.
  bool sample (name_t i, double p) {
    std::string e = "sample";
    int ind = random_integer((inventory[0])[i].size());
    bool terminate = (p >= 1.0 || (p > 0.0 && unif_rand() < p));
    ball_t *a;
    for (name_t s = 0; s < ngeneal; s++) {
      a = inventory[s].get_ball_idx(ind,i);
      if (terminate) {
        inventory[s].erase(a);
        geneal[s].sample_death(a,time());
      } else {
        geneal[s].sample(a,time());
      }
    }
    valid_invens(e);
    return terminate;
  };
  //! sample and terminate lineage in deme i
  void sample_death (name_t i = 0) {
    std::string e = "sample_death";
    int ind = random_integer((inventory[0])[i].size());
    ball_t *a;
    for (name_t s = 0; s < ngeneal; s++) {
      a = inventory[s].get_ball_idx(ind,i);
      inventory[s].erase(a);
      geneal[s].sample_death(a,time());
    }
    valid_invens(e);
  };
  //! migration from deme i to deme j
  void migrate (name_t i = 0, name_t j = 0) {
    std::string e = "migrate";
    int ind = random_integer((inventory[0])[i].size());
    ball_t *a;
    for (name_t s = 0; s < ngeneal; s++) {
      a = inventory[s].get_ball_idx(ind,i);
      inventory[s].erase(a);
      geneal[s].migrate(a,time(),j);
      a->deme() = j;
      inventory[s].insert(a);
    }
    valid_invens(e);
  };
  //! reassort between deme i and deme j in segments specified by seg array.
  //! optionally migrate the ball from deme i to deme imig after reassortment.
  //! set imig to a valid deme to enable migration; set imig == i to skip.
  void reassort (name_t i = 0, name_t j = 0, name_t* seg = NULL, name_t sz = 1,
                 name_t imig = 0, bool do_migrate = false) {
    std::string e = "reassort";
    slate_t t = time();
    bool exist;
    if (sz < 1 || seg == NULL) err("Empty segment array!");
    if (sz > nseg) err("Requested number of segments larger than total number.");
    if (sz == nseg) err("All segments reassort simultaneously!");

    int inda, indb;
    ball_t *a, *b;
    inda = random_integer((inventory[0])[i].size());
    indb = random_integer((inventory[0])[j].size());
    while ((i==j) && (inda==indb))
      indb = random_integer((inventory[0])[j].size());
    for (name_t k = 0; k < ngeneal; k++) {
      exist = anyof(seg, sz, k);
      a = inventory[k].get_ball_idx(inda,i);
      b = inventory[k].get_ball_idx(indb,j);
      if (exist) {
        geneal[k].reassort(a,b,t);
      } else {
        geneal[k].update_uniq();
        geneal[k].reassort_notice(a,t);
      }
    }
    // optionally migrate the reassorted individual (ball a) from deme i to imig
    if (do_migrate && imig != i) {
      for (name_t k = 0; k < ngeneal; k++) {
        a = inventory[k].get_ball_idx(inda,i);
        inventory[k].erase(a);
        geneal[k].migrate(a,t,imig);
        a->deme() = imig;
        inventory[k].insert(a);
      }
    }
    valid_invens(e);
  };
  //! initialize the state
  void rinit (void);
  //! makes a jump
  void jump (int e);
};

#endif
