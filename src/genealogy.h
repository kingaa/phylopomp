// -*- C++ -*-
// GENEALOGY class

#ifndef _GENEALOGY_H_
#define _GENEALOGY_H_

#include <utility>
#include <stdexcept>

#include "nodeseq.h"
#include "internal.h"

static const size_t MEMORY_MAX = (1<<28); // 256MB

//! Encodes a genealogy.

//! A genealogy consists of a sequence of nodes
//! and the current time.
class genealogy_t : public nodeseq_t {

private:

  // GENEALOGY member data:
  // - a counter of serial numbers
  // - an initial time
  // - the current time
  // - a sequence of nodes

  //! The next unique name.
  name_t _unique;
  //! The initial time.
  slate_t _t0;
  //! The current time.
  slate_t _time;
  //! The number of demes (excluding the undeme).
  size_t _ndeme;

  const static name_t magic = 1123581321;

private:

  //! get the next unique name
  name_t unique (void) {
    name_t u = _unique;
    _unique++;
    return u;
  };

  //! clean up
  void clean (void) {
    _unique = 0;
    _ndeme = 0;
    _t0 = _time = R_NaReal;
  };

public:

  //! number of demes
  size_t ndeme (void) const {
    return _ndeme;
  };
  //! number of demes
  size_t& ndeme (void) {
    return _ndeme;
  };

public:

  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    return 3*sizeof(name_t) +
      2*sizeof(slate_t) + nodeseq_t::bytesize();
  };
  //! binary serialization
  friend raw_t* operator>> (const genealogy_t& G, raw_t* o) {
    name_t A[3]; A[0] = magic; A[1] = G._unique; A[2] = name_t(G.ndeme());
    slate_t B[2]; B[0] = G.timezero(); B[1] = G.time();
    memcpy(o,A,sizeof(A)); o += sizeof(A);
    memcpy(o,B,sizeof(B)); o += sizeof(B);
    return reinterpret_cast<const nodeseq_t&>(G) >> o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, genealogy_t& G) {
    G.clean();
    name_t A[3];
    slate_t B[2];
    memcpy(A,o,sizeof(A)); o += sizeof(A);
    memcpy(B,o,sizeof(B)); o += sizeof(B);
    if (A[0] != magic)
      err("in %s: corrupted genealogy serialization.",__func__);
    G._unique = A[1]; G.ndeme() = size_t(A[2]);
    G.timezero() = B[0]; G.time() = B[1];
    return o >> reinterpret_cast<nodeseq_t&>(G);
  };

public:
  // CONSTRUCTORS
  //! basic constructor for genealogy class
  //! - t0 = initial time
  //! - ndeme = number of demes (excluding the undeme)
  genealogy_t (double t0 = R_NaReal, size_t ndeme = 0) {
    clean();
    _time = _t0 = slate_t(t0);
    _ndeme = ndeme;
  };
  //! constructor from serialized binary form
  genealogy_t (raw_t *o) {
    o >> *this;
  };
  //! constructor from RAW SEXP (containing binary serialization)
  genealogy_t (SEXP o) {
    if (LENGTH(o)==0)
      err("in %s: cannot deserialize a NULL.",__func__);
    PROTECT(o = AS_RAW(o));
    RAW(o) >> *this;
    UNPROTECT(1);
  };
  //! copy constructor
  genealogy_t (const genealogy_t& G) {
    raw_t *o = new raw_t[G.bytesize()];
    G >> o;
    o >> *this;
    delete[] o;
  };
  //! copy assignment operator
  genealogy_t& operator= (const genealogy_t& G) {
    clean();
    raw_t *o = new raw_t[G.bytesize()];
    G >> o;
    o >> *this;
    delete[] o;
    return *this;
  };
  //! move constructor
  genealogy_t (genealogy_t&&) = default;
  //! move assignment operator
  genealogy_t& operator= (genealogy_t&&) = default;
  //! destructor
  ~genealogy_t (void) {
    clean();
  };

  //! view/set current time.
  slate_t& time (void) {
    return _time;
  };
  //! view current time.
  slate_t time (void) const {
    return _time;
  };
  //! view/set zero time.
  slate_t& timezero (void) {
    return _t0;
  };
  //! get zero time.
  slate_t timezero (void) const {
    return _t0;
  };

public:

  //! lineage count, saturation, and event-type.
  //! types are:
  //! -  0 = non-event
  //! - -1 = root
  //! -  1 = sample
  //! -  2 = non-sample node
  //! -  3 = end of interval
  void lineage_count (double *tout, int *deme,
                      int *ell, int *sat, int *etype) const;
  //! lineage count and saturation
  SEXP lineage_count (void) const;

  //! genealogy information in list format
  void gendat (double *tout, int *anc, int *lin,
               int *sat, int *type, int *deme,
               int *index, int *child) const;
  //! genealogy information in list format
  SEXP gendat (void) const;

  //! number of samples
  size_t nsample (void) const {
    size_t n = 0;
    for (const node_t *p : *this) {
      if (p->holds(blue)) n++;
    }
    return n;
  };

  //! number of roots
  size_t nroot (void) const {
    size_t n = 0;
    for (const node_t *p : *this) {
      if (p->holds_own()) n++;
    }
    return n;
  };

public:

  //! human-readable info
  string_t describe (void) const;
  //! machine-readable info
  string_t yaml (string_t tab = "") const;
  //! R list description
  SEXP structure (void) const;
  //! put genealogy at current time into Newick format.
  string_t newick (bool extended = true) const;

public:

  //! check the validity of the genealogy.
  void valid (void) const {};
  //! check the size of the genealogy (to prevent memory exhaustion).
  bool check_genealogy_size (size_t grace = 0) const {
    static size_t maxq = MEMORY_MAX/(sizeof(node_t)+2*sizeof(ball_t));
    bool ok = true;
    if (size() > maxq+grace) {
      err("maximum genealogy size exceeded!"); // #nocov
    } else if (size() > maxq) {
      ok = false;               // #nocov
    }
    return ok;
  };

private:

  //! create a node holding its own green ball.
  //! this does not insert into the genealogy.
  node_t* make_node (name_t d = undeme) {
    check_genealogy_size(0);
    name_t u = unique();
    node_t *p = new node_t(u,_time);
    ball_t *g = new ball_t(p,u,green,d);
    p->green_ball() = g;
    p->insert(g);
    return p;
  };

public:

  //! birth into deme d
  ball_t* birth (ball_t* a, slate_t t, name_t d) {
    time() = t;
    node_t *p = make_node(a->deme());
    ball_t *b = new ball_t (p,p->uniq,black,d);
    p->insert(b);
    p->slate = time();
    add(p,a);
    return b;
  };
  //! birth of second or subsequent sibling into deme d
  ball_t* birth (node_t* p, name_t d) {
    ball_t *b = new ball_t(p,unique(),black,d);
    p->insert(b);
    return b;
  };
  //! death
  void death (ball_t *a, slate_t t) {
    time() = t;
    drop(a);
  };
  //! graft a new lineage into deme d
  ball_t* graft (slate_t t, name_t d) {
    time() = t;
    node_t *p = make_node();
    ball_t *b = new ball_t (p,p->uniq,black,d);
    p->insert(b);
    p->slate = timezero();
    push_front(p);
    return b;
  };
  //! insert a sample node
  void sample (ball_t* a, slate_t t) {
    time() = t;
    node_t *p = make_node(a->deme());
    ball_t *b = new ball_t (p,p->uniq,blue,a->deme());
    p->insert(b);
    p->slate = time();
    add(p,a);
  };
  //! insert a sample node and simultaneously terminate the lineage
  void sample_death (ball_t* a, slate_t t) {
    time() = t;
    node_t *p = make_node(a->deme());
    ball_t *b = new ball_t (p,p->uniq,blue,a->deme());
    p->insert(b);
    p->slate = time();
    add(p,a);
    drop(a);
  };
  //! movement into deme d
  void migrate (ball_t* a, slate_t t, name_t d = 0) {
    time() = t;
    node_t *p = make_node(a->deme());
    p->slate = time();
    add(p,a);
    a->deme() = d;
  };
  //! insert a sample node and simultaneously migrate the lineage
  void sample_migrate (ball_t* a, slate_t t, name_t d = 0) {
    time() = t;
    node_t *p = make_node(a->deme());
    ball_t *b = new ball_t (p,p->uniq,blue,a->deme());
    p->insert(b);
    p->slate = time();
    add(p,a);
    a->deme() = d;
  };
  //! prune the tree (drop all black balls)
  genealogy_t& prune (void) {
    pocket_t *blacks = colored(black);
    while (!blacks->empty()) {
      ball_t *b = *(blacks->begin());
      blacks->erase(b);
      drop(b);
    }
    delete blacks;
    return *this;
  };
  //! erase all deme information
  genealogy_t& obscure (void) {
    // erase deme information from black balls.
    pocket_t *blacks = colored(black);
    while (!blacks->empty()) {
      ball_t *a = *(blacks->begin());
      a->deme() = undeme;
      blacks->erase(a);
    }
    delete blacks;
    // erase deme information from nodes.
    for (node_t *p : *this) {
      p->deme() = undeme;
    }
    // drop superfluous nodes (holding just one ball).
    comb();
    ndeme() = 0;
    return *this;
  };

  //! curtail the genealogy by removing nodes
  //! with times later than tnew and/or earlier than troot
  void curtail (slate_t tnew, slate_t troot);

private:

  void reuniqify (name_t shift);

public:

  //! merge two genealogies:
  //! 1. the node-sequences are merged;
  //! 2. the root time retreats as necessary;
  //! 3. the current time advances as necessary;
  //! 4. the unique-name stack advances as necessary.
  genealogy_t& operator+= (const genealogy_t& other);

  //! insert zero-length branches for all samples
  void insert_zlb (void) {
    for (node_t *p : *this) {
      if (p->holds(green) && p->holds(blue)) {
        assert(!p->holds(black)); // genealogy should have already been pruned
        ball_t *b = p->last_ball();
        assert(b->is(blue));
        node_t *q = make_node(p->deme());
        q->slate = p->slate;
        swap(q->green_ball(),b);
        push_back(q);
      }
    }
    sort();
  };
  //! drop all zero-length branches
  void drop_zlb (void) {
    for (node_t *p : *this) {
      if (!p->holds_own() &&
          p->slate == p->parent()->slate &&
          p->deme() == p->parent()->deme()) {
        while (!p->empty()) {
          ball_t *b = p->last_ball();
          p->erase(b); p->parent()->insert(b);
        }
        detach(p);
      }
    }
  };

private:

  //! - tips without descendants are reclassified as samples.
  //! - tips with black balls are swapped out.
  void repair_tips (void);
  //! roots are added at zero time if needed
  void repair_roots (void) {
    node_nit j = begin();
    while (j != end()) {
      if ((*j)->holds_own() && (*j)->slate > timezero()) {
        node_t *q = make_node();
        q->slate = timezero();
        attach(q,*j);
        push_front(q);
      }
      j++;
    }
    sort();
  };

  //! Scan the branch (label+branch-length) string.
  //! This has format [&&PhyloPOMP deme=%d type=%s]%s:%f
  node_t *scan_branch (string_t::const_iterator b, string_t::const_iterator e);

public:

  //! Parse a Newick string and create the indicated genealogy.
  genealogy_t& parse (const string_t& s);

};

#endif
