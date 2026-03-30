// -*- C++ -*-
// NODE CLASS

#ifndef _NODE_H_
#define _NODE_H_

#include "ball.h"
#include "pocket.h"
#include "internal.h"

static const name_t null_lineage = name_t(NA_INTEGER);

//! Encodes a genealogical node.

//! Each node has:
//! - a unique name (uniq)
//! - a pocket containting two or more balls
//! - a "slate" with the time
//! - a lineage
//! - a pointer to its own green ball
class node_t : public pocket_t {

private:

  ball_t *_green_ball;
  name_t _lineage;

  void clean (void) { };

public:

  name_t uniq;
  slate_t slate;

public:

  //! size of binary serialization
  size_t bytesize (void) const {
    return 2*sizeof(name_t) + sizeof(slate_t)
      + pocket_t::bytesize();
  };
  //! binary serialization of node_t
  friend raw_t* operator>> (const node_t &p, raw_t *o) {
    name_t buf[2] = {p.uniq, p._lineage};
    memcpy(o,buf,sizeof(buf)); o += sizeof(buf);
    memcpy(o,&p.slate,sizeof(slate_t)); o += sizeof(slate_t);
    return reinterpret_cast<const pocket_t&>(p) >> o;
  };
  //! binary deserialization of node_t
  friend raw_t* operator>> (raw_t *o, node_t &p) {
    p.clean();
    name_t buf[2];
    memcpy(buf,o,sizeof(buf)); o += sizeof(buf);
    memcpy(&p.slate,o,sizeof(slate_t)); o += sizeof(slate_t);
    p.uniq = buf[0]; p._lineage = buf[1];
    o = (o >> reinterpret_cast<pocket_t&>(p));
    p.repair_holder(&p);
    return o;
  };

  void reuniqify (name_t shift);

public:

  //! basic constructor for node class
  node_t (name_t u = 0, slate_t t = R_NaReal) {
    uniq = u;
    slate = t;
    _green_ball = 0;
    _lineage = null_lineage;
  };
  //! copy constructor
  node_t (const node_t &p) = delete;
  //! move constructor
  node_t (node_t && p) = delete;
  //! copy assignment operator
  node_t & operator= (const node_t & p) = delete;
  //! move assignment operator
  node_t & operator= (node_t && p) = delete;
  //! destructor
  ~node_t (void) {
    clean();
  };

public:

  //! pointer to my green ball
  ball_t* green_ball (void) const {
    return _green_ball;
  };
  //! set green ball
  ball_t*& green_ball (void) {
    return _green_ball;
  };
  //! view deme
  name_t deme (void) const {
    return _green_ball->deme();
  };
  //! set deme
  name_t& deme (void) {
    return _green_ball->deme();
  };
  //! view lineage
  name_t lineage (void) const {
    return _lineage;
  };
  //! view lineage associated with a green ball
  name_t lineage (const ball_t *g) const {
    return g->owner()->lineage();
  };
  //! set lineage
  name_t& lineage (void) {
    return _lineage;
  };
  node_t* parent (void) const {
    return _green_ball->holder();
  };
  bool holds_own (void) const {
    return (_green_ball->holder() == this);
  };
  bool is_root (void) const {
    return holds_own();
  };
  bool dead_root (void) const {
    return holds_own() && size()==1;
  };

public:

  //! number of descendants
  int nchildren (void) const {
    int n = 0;
    for (ball_t *b : *this) {
      switch (b->color) {
      case green: case black:
        n++;
        break;
      default:
        break;
      }
    }
    if (holds_own()) n--;
    return n;
  };
  //! lineage count, saturation, and event-type
  //! types are:
  //! -  0 = non-event
  //! - -1 = root
  //! -  1 = sample
  //! -  2 = non-sample node
  void lineage_incr (int *incr, int *sat, int *etype) const;

public:

  //! insert a ball into the pocket of a node
  void insert (ball_t *a) {
    a->holder() = this;
    pocket_t::insert(a);
  };

  //! human/machine-readable info
  string_t yaml (string_t tab = "") const;
  //! R list description
  SEXP structure (void) const;
  //! Newick-format output
  string_t newick (const slate_t& tnow, const slate_t& tpar,
                   bool showdeme, bool extended) const;

};

#endif
