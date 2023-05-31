// -*- C++ -*-
// NODE CLASS

#ifndef _NODE_H_
#define _NODE_H_

#include "internal.h"
#include "ball.h"
#include "pocket.h"
#include <string>
#include <cstring>

//! Encodes a genealogical node.

//! Each node has:
//! - a unique name (uniq)
//! - a deme
//! - a pocket containting two or more balls
//! - a "slate" with the time
//! - a pointer to its own green ball
class node_t : public pocket_t {

private:

  ball_t *_green_ball;

  void clean (void) { };

public:
    
  name_t uniq, deme;
  slate_t slate;

public:
  //! size of binary serialization
  size_t bytesize (void) const {
    return 2*sizeof(name_t) + sizeof(slate_t)
      + pocket_t::bytesize();
  };
  //! binary serialization of node_t
  friend raw_t* operator>> (const node_t &p, raw_t *o) {
    name_t buf[2];
    buf[0] = p.uniq; buf[1] = p.deme;
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
    p.uniq = buf[0]; p.deme = buf[1];
    o = (o >> reinterpret_cast<pocket_t&>(p));
    p.repair_holder(&p);
    return o;
  };

public:
  
  //! basic constructor for node class
  node_t (name_t u = 0, slate_t t = R_NaReal, name_t d = 0) {
    uniq = u;
    slate = t;
    deme = d;
    _green_ball = 0;
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
  //! pointer to my green ball
  ball_t* green_ball (void) const {
    return _green_ball;
  };
  //! set green ball
  ball_t*& green_ball (void) {
    return _green_ball;
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
  //! number of descendants
  int nchildren (void) const {
    int n = 0;
    for (ball_it i = begin(); i != end(); i++) {
      switch ((*i)->color) {
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
  void lineage_incr (int *incr, int *sat, int *etype) const {
    incr[deme]--;
    for (ball_it i = cbegin(); i != cend(); i++) {
      ball_t *b = *i;
      switch (b->color) {
      case green:
        incr[b->child()->deme]++;
        sat[b->child()->deme]++;
        break;
      case black:
        incr[b->deme()]++;
        sat[b->deme()]++;
        break;
      default:
        break;
      }
    }
    if (holds_own()) {
      sat[deme]--;
      etype[deme] = -1;
    } else if (holds(blue)) {
      etype[deme] = 1;
    } else {
      etype[deme] = 2;
    }
  };
  //! human-readable info
  std::string describe (void) const {
    std::string s = "node(" + std::to_string(uniq)
      + "," + std::to_string(deme) + ") ";
    s += pocket_t::describe();
    s += ", t = " + std::to_string(slate) + "\n";
    return s;
  };
  //! R list description
  SEXP structure (void) const {
    SEXP O, On, Name, Time, Deme, Pocket;
    PROTECT(O = NEW_LIST(4));
    PROTECT(On = NEW_CHARACTER(4));
    PROTECT(Name = NEW_INTEGER(1));
    *INTEGER(Name) = int(uniq);
    PROTECT(Time = NEW_NUMERIC(1));
    *REAL(Time) = double(slate);
    PROTECT(Deme = NEW_INTEGER(1));
    *INTEGER(Deme) = int(deme);
    PROTECT(Pocket = pocket_t::structure());
    set_list_elem(O,On,Name,"name",0);
    set_list_elem(O,On,Time,"time",1);
    set_list_elem(O,On,Deme,"deme",2);
    set_list_elem(O,On,Pocket,"pocket",3);
    SET_NAMES(O,On);
    UNPROTECT(6);
    return O;
  };
  //! machine-readable info
  std::string yaml (std::string tab = "") const {
    std::string t = tab + "  ";
    std::string o = "name: " + std::to_string(uniq) + "\n"
      + tab + "deme: " + std::to_string(deme) + "\n"
      + tab + "time: " + std::to_string(slate) + "\n"
      + tab + "pocket:\n"
      + pocket_t::yaml(tab);
    return o;
  };
  //! Newick format
  std::string newick (const slate_t& tnow, const slate_t& tpar) const {
    std::string o1 = "", o2 = "", o3 = "";
    int n = nchildren();
    if (n > 0) {
      o1 = "("; o3 = ")";
    }
    if (holds(blue)) {
      o3 += "b_";
    } else if (holds_own()) {
      o3 += "m_";
    } else {
      o3 += "g_";
    }
    for (ball_it i = begin(); i != end(); i++, n--) {
      ball_t *b = *i;
      node_t *p = 0;
      switch (b->color) {
      case green:
        p = b->child();
        if (p != this) {
          o2 += p->newick(tnow,slate);
          if (n > 1) o2 += ",";
        }
        break;
      case black:
        o2 += b->newick(tnow-slate);
        if (n > 1) o2 += ",";
        break;
      case blue:
        break;
      }
    }
    return o1 + o2 + o3
      + std::to_string(deme)
      + "_" + std::to_string(uniq)
      + ":" + std::to_string(slate - tpar);
  };
  //! order relation
  friend bool compare (const node_t*p, const node_t* q) {
    return (p->slate < q->slate) ||
      ((p->slate == q->slate) && (p->uniq < q->uniq));
  };
};

//! Ordering for nodes.
//! Within a node sequence, nodes should be ordered in time.
static inline bool node_compare (const node_t* p, const node_t* q) {
  return compare(p,q);
}

#endif
