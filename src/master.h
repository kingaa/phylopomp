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
#include <list>
#include <cstdlib>
#include "internal.h"
#include "popul_proc.h"
#include "genealogy.h"

//! Encodes the master process.

//! This consists of a population process and a genealogy process.
template <class POPN, size_t NDEME = 1, size_t NSEG = 1, bool CONT = false>
class master_t : public POPN {

public:

  typedef POPN popul_t;
  typedef typename std::list<slate_t>::const_iterator retime_it;
  const static size_t ndeme = NDEME;
  const static size_t nseg = NSEG;
  const static bool cont = CONT;

public:
  // DATA MEMBERS
  genealogy_t<ndeme> geneal[nseg];
  inventory_t<ndeme> inventory[nseg];
  std::list<slate_t> retimes;

public:
  //! size of serialized binary form
  size_t bytesize (void) const {
    size_t bsize = popul_t::bytesize() + sizeof(size_t) + retimes.size()*sizeof(slate_t);
    for (name_t s = 0; s < nseg; s++) bsize += geneal[s].bytesize();
    return bsize;
  };
  //! binary serialization
  friend raw_t* operator>> (const master_t& A, raw_t* o) {
    o = (reinterpret_cast<const popul_t&>(A) >> o);
    for (name_t s = 0; s < A.nseg; s++) o = (A.geneal[s] >> o);
    size_t rsize = A.retimes.size();
    memcpy(o,&rsize,sizeof(size_t)); o += sizeof(size_t);
    slate_t* rts = new slate_t[rsize];
    name_t i = 0;
    for (retime_it it = A.retimes.begin(); it != A.retimes.end(); it++) {
      rts[i] = *it;
      i++;
    }
    memcpy(o,rts,rsize*sizeof(slate_t)); o += rsize*sizeof(slate_t);
    delete[] rts;
    return o;
  }
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, master_t& A) {
    A.clean();
    o = (o >> reinterpret_cast<popul_t&>(A));
    for (name_t s = 0; s < A.nseg; s++) {
      o = o >> A.geneal[s];
      A.inventory[s] = A.geneal[s].extant();
    }
    size_t rsize;
    memcpy(&rsize,o,sizeof(size_t)); o += sizeof(size_t);
    slate_t* rts = new slate_t[rsize];
    memcpy(rts,o,rsize*sizeof(slate_t)); o += rsize*sizeof(slate_t);
    for (size_t i = 0; i < rsize; i++) {
      A.retimes.push_back(rts[i]);
    }
    delete[] rts;
    return o;
  }

private:
  
  void clean (void) { 
    retimes.clear();
  };

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
  //! check the consistency of multiple inventories
  void valid_invens (std::string e) const {
    size_t n;
    name_t s, d, i;
    ball_t *a, *b;
    for (d = 0; d < ndeme; d++) {
      n = (inventory[0]).size(d);
      for (s = 1; s < nseg; s++) {
        //! check size
        if ((inventory[s]).size(d) != n)
          err("Inconsistent sizes between Segment %ld and %ld for deme %ld in event '%s'.", 0, s, d, e.c_str());
        //! check black ball names
        for (i = 0; i < n; i++) {
          a = inventory[0].get_ball_idx(i,d);
          b = inventory[s].get_ball_idx(i,d);
          if (a->uniq != b->uniq) {
            err("For event %s, in deme %ld, ball %ld: Seg 0: %ld; Seg %ld: %ld.\n", e.c_str(), d, i, a->uniq, s, b->uniq);
          }
        }
      }
    }
  };
  //! t0
  // slate_t timezero (void) const {
  //   return popul_t::timezero();
  // };
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
  //! reassortment times
  SEXP reassortment_times (void) const {
    int n = retimes.size();
    SEXP out;
    PROTECT(out = NEW_NUMERIC(n));
    int i = 0;
    for (retime_it it = retimes.begin(); it!=retimes.end(); it++) {
      SET_REAL_ELT(out, i, *it);
      i++;
    }
    UNPROTECT(1);
    return out;
  };

public:
  //! n births into deme j with parent in deme i
  void birth (name_t i = 0, name_t j = 0, int n = 1) {
    std::string e = "birth";
    int ind = random_integer((inventory[0])[i].size());
    ball_t *a, *b;
    for (name_t s = 0; s < nseg; s++) {
      a = inventory[s].get_ball_idx(ind,i);
      b = geneal[s].birth(a,time(),j);
      inventory[s].insert(b);
      while (n > 1) {
        b = geneal[s].birth(b->holder(),j);
        inventory[s].insert(b);
        n--;
      }
    }
    valid_invens(e);
  };
  //! death in deme i
  void death (name_t i = 0) {
    std::string e = "death";
    int ind = random_integer((inventory[0])[i].size());
    ball_t *a;
    for (name_t s = 0; s < nseg; s++) {
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
      for (name_t s = 0; s < nseg; s++) {
        ball_t *a = geneal[s].graft(time(),i);
        inventory[s].insert(a);
      }
    }
    valid_invens(e);
  };
  //! sample in deme i
  void sample (name_t i = 0) {
    std::string e = "sample";
    size_t n = (inventory[0])[i].size();
    int ind = random_integer(int(n));
    ball_t *a;
    for (name_t s = 0; s < nseg; s++) {
      a = inventory[s].get_ball_idx(name_t(ind),i);
      if (cont) inventory[s].erase(a);
      geneal[s].sample(a,time(),cont);
    }
    valid_invens(e);
  };
  //! migration from deme i to deme j
  void migrate (name_t i = 0, name_t j = 0) {
    std::string e = "migrate";
    int ind = random_integer((inventory[0])[i].size());
    ball_t *a;
    for (name_t s = 0; s < nseg; s++) {
      a = inventory[s].get_ball_idx(ind,i);
      inventory[s].erase(a);
      geneal[s].migrate(a,time(),j);
      a->deme() = j;
      inventory[s].insert(a);
    }
    valid_invens(e);
  };
  //! reassort between deme i and deme j in segment array seg
  void reassort (name_t i = 0, name_t j = 0, name_t* seg = NULL, name_t size = 1) {
    std::string e = "reassort";
    retimes.push_back(time());
    bool exist;
    if (size < 1 || seg == NULL) err("Empty segment array!");
    if (size > nseg)  err("Requested number of segments larger the total number.");
    if (size == nseg)  err("All segments reassort simultaneously!");
    
    int inda, indb;
    ball_t *a, *b;
    inda = random_integer((inventory[0])[i].size());
    indb = random_integer((inventory[0])[j].size());
    while ((i==j) && (inda==indb))  indb = random_integer((inventory[0])[j].size());

    for (name_t k = 0; k < nseg; k++) {
      exist = anyof(seg, size, k);
      a = inventory[k].get_ball_idx(inda,i);
      if (exist) {
        b = inventory[k].get_ball_idx(indb,i);
        geneal[k].reassort(a,b,time());
      } else {
        geneal[k].update_uniq();
        geneal[k].reassort_notice(a,time());
      }
    }
    valid_invens(e);
  };
  //! batch_sampling at the end of the time, with fraction frac
  void batch_sample (double frac = 0) {
    size_t N = 0, deme_size[ndeme];
    name_t i;
    for (i = 0; i < ndeme; i++) {
      deme_size[i] = (inventory[0])[i].size();
      N += deme_size[i];
    }
    size_t n = (size_t)(N*frac);
    if (n > 0) {
      std::string e = "batch sample";
      valid_invens(e);
      ball_t *a;
      name_t* inds = (name_t*)R_Calloc(n,name_t);
      random_numbers(inds, N, n);
      qsort(inds, n, sizeof(name_t), compare_int);
      i = 0;
      for (name_t d = 0; d < ndeme; d++) {
        while (inds[i] < deme_size[d]) {
          for (name_t s = 0; s < nseg; s++) {
            a = inventory[s].get_ball_idx(inds[i],d);
            geneal[s].sample(a,time());
          }
          i++;
        }
      }
      valid_invens(e);
      R_Free(inds);
    }
  };
  //! initialize the state
  void rinit (void);
  //! makes a jump
  void jump (int e);
  //! set an ending pose
  void batch (void);
};

#endif
