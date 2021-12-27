// -*- C++ -*-
// NODE CLASS
// each node has:
// - a unique name (uniq)
// - a deme
// - a pocket containting two or more balls
// - a "slate" with the time
// - a pointer to its own green ball

#ifndef _NODE_H_
#define _NODE_H_

#include "internal.h"
#include "ball.h"
#include "pocket.h"
#include <string>
#include <cstring>

class node_t : public pocket_t {

 private:

  ball_t *_green_ball;

  void clean (void) { };

 public:
    
  name_t uniq, deme;
  slate_t slate;

  // basic constructor for node class
  node_t (name_t u = 0, slate_t t = R_NaReal, name_t d = 0) {
    uniq = u;
    slate = t;
    deme = d;
    _green_ball = 0;
  };

 public:
  // size of binary serialization
  size_t bytesize (void) const {
    return 2*sizeof(name_t) + sizeof(slate_t)
      + pocket_t::bytesize();
  };
  // binary serialization of node_t
  friend raw_t* operator<< (raw_t *o, const node_t &p) {
    name_t buf[2];
    buf[0] = p.uniq; buf[1] = p.deme;
    memcpy(o,buf,sizeof(buf)); o += sizeof(buf);
    memcpy(o,&p.slate,sizeof(slate_t)); o += sizeof(slate_t);
    return o << reinterpret_cast<const pocket_t&>(p);
  };
  // binary deserialization of node_t
  friend raw_t* operator>> (raw_t *o, node_t &p) {
    p.clean();
    name_t buf[2];
    memcpy(buf,o,sizeof(buf)); o += sizeof(buf);
    memcpy(&p.slate,o,sizeof(slate_t)); o += sizeof(slate_t);
    p.uniq = buf[0]; p.deme = buf[1];
    o = (o >> reinterpret_cast<pocket_t&>(p));
    p.set_holder(&p);
    return o;
  };
  
  // copy constructor
  node_t (const node_t &p) = delete;
  // move constructor
  node_t (node_t && p) = delete;
  // copy assignment operator
  node_t & operator= (const node_t & p) = delete;
  // move assignment operator
  node_t & operator= (node_t && p) = delete;
  // destructor
  ~node_t (void) {
    clean();
  };
  // pointer to my green ball
  ball_t* green_ball (void) const {
    return _green_ball;
  };
  // set green ball
  void green_ball (ball_t *g) {
    _green_ball = g;
  };
  bool holds_own (void) const {
    return (_green_ball->holder() == this);
  };
  bool is_root (void) const {
    return holds_own();
  };
  // number of descendants
  int nchildren (bool compact = false) const {
    int n = 0;
    if (compact) {
      for (ball_it i = begin(); i != end(); i++) {
	switch ((*i)->color) {
	case green: case black:
	  n++;
	  break;
	default:
	  break;
	}
      }
    } else {
      n = size();
    }
    if (holds_own()) n--;
    return n;
  };
  // increment to lineage count
  void lineage_incr (int *incr) const {
    incr[deme]--;
    for (ball_it i = begin(); i != end(); i++) {
      switch ((*i)->color) {
      case green:
	incr[(*i)->child()->deme]++;
	break;
      case black:
	incr[(*i)->deme]++;
	break;
      default:
	break;
      }
    }
  };
  // human-readable info
  std::string describe (void) const {
    std::string s = "node(" + std::to_string(uniq)
      + "," + std::to_string(deme) + ") ";
    s += pocket_t::describe();
    s += ", t = " + std::to_string(slate) + "\n";
    return s;
  };
  // R list description
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
  // machine-readable info
  std::string yaml (std::string tab = "") const {
    std::string t = tab + "  ";
    std::string o = "name: " + std::to_string(uniq) + "\n"
      + tab + "deme: " + std::to_string(deme) + "\n"
      + tab + "time: " + std::to_string(slate) + "\n"
      + tab + "pocket:\n"
      + pocket_t::yaml(tab);
    return o;
  };
  // Newick format
  std::string newick (const slate_t& tnow, const slate_t& tpar) const {
    std::string o = "(";
    int n = nchildren(false);
    for (ball_it i = begin(); i != end(); i++, n--) {
      ball_t *b = *i;
      node_t *p = 0;
      switch (b->color) {
      case green:
	p = b->child();
	if (p != this) {
	  o += p->newick(tnow,slate);
	}
	break;
      case black:
	o += b->newick(tnow-slate);
	break;
      case purple: case red: case blue:
	o += b->newick(0);
	break;
      default:                                 // # nocov
	err("in 'newick': c'est impossible!"); // # nocov
	break;
      }
      if (n > 1) o += ",";
    }
    o += ")g_" + std::to_string(deme)
      + "_" + std::to_string(uniq)
      + ":" + std::to_string(slate - tpar);
    return o;
  };
  // compact Newick format
  std::string compact_newick (const slate_t& tnow, const slate_t& tpar) const {
    std::string o1 = "(";
    std::string o2 = "";
    std::string o3 = (holds_own()) ? ")m_" : ")g_";
    bool rednode = holds(red);
    int n = nchildren(true);
    for (ball_it i = begin(); i != end(); i++, n--) {
      ball_t *b = *i;
      node_t *p = 0;
      switch (b->color) {
      case green:
	p = b->child();
	if (p != this) {
	  o2 += p->compact_newick(tnow,slate);
	}
	break;
      case black:
	o2 += b->newick(tnow-slate);
	break;
      case purple:
	o3 = ")p_";
	break;
      case red:
	o1 = ""; o2 = ""; o3 = "r_";
	rednode = true;
	break;
      case blue:
	if (!rednode) o3 = ")b_";
	break;
      default:                                         // # nocov
	err("in 'compact_newick': c'est impossible!"); // # nocov
	break;
      }
      if (n > 1) o2 += ",";
    }
    return o1 + o2 + o3
      + std::to_string(deme)
      + "_" + std::to_string(uniq)
      + ":" + std::to_string(slate - tpar);
  };

};

#endif
