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
#include <string>
#include <cstring>

class node_t {

 private:

  ball_t *_greenball;

 public:
    
  name_t uniq, deme;
  pocket_t pocket;
  slate_t slate;

  // basic constructor for node class
  node_t (name_t u = 0, slate_t t = R_NaReal, name_t d = 0) {
    uniq = u;
    slate = t;
    deme = d;
    _greenball = 0;
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
  // delete balls and clear pocket
  void clean (void) {
    for (ball_it i = pocket.begin(); i != pocket.end(); i++) delete *i;
    pocket.clear();
  };
  // does this node hold this ball?
  bool holds (ball_t *b) const {
    ball_it i = pocket.find(b);
    return (i != pocket.end());
  };
  // does this node hold a ball of this color?
  bool holds (color_t c) const {
    bool result = false;
    for (ball_it i = pocket.begin(); !result && i != pocket.end(); i++) {
      result = ((*i)->color == c);
    }
    return result;
  };
  // pointer to my green ball
  ball_t* green_ball (void) const {
    return _greenball;
  };
  // set green ball
  void green_ball (ball_t *g) {
    _greenball = g;
  };
  bool holds_own (void) const {
    return (_greenball->holder() == this);
  };
  bool is_root (void) const {
    return holds_own();
  };
  // retrieve the first ball of the specified color.
  ball_t *ball (const color_t c) const {
    for (ball_it i = pocket.begin(); i != pocket.end(); i++) {
      if ((*i)->color == c) return *i;
    }
    err("no ball of color %s",colores[c]); // # nocov
    return 0;
  };
  // return a pointer to another ball
  ball_t *other (const ball_t *b) const {
    for (ball_it i = pocket.begin(); i != pocket.end(); i++) {
      if (*i != b) return *i;
    }
    err("error in 'other': no other ball"); // # nocov
    return 0;
  };
  // number of descendants
  int nchildren (bool compact = false) const {
    int n = 0;
    if (compact) {
      for (ball_it i = pocket.begin(); i != pocket.end(); i++) {
	switch ((*i)->color) {
	case green: case black:
	  n++;
	  break;
	case blue: case red: case purple: case grey:
	  break;
	}
      }
    } else {
      n = pocket.size();
    }
    if (holds_own()) n--;
    return n;
  };
  // human-readable info
  std::string describe (void) const {
    std::string s = "node(" + std::to_string(uniq)
      + "," + std::to_string(deme) + ") {";
    ball_it i = pocket.begin();
    s += (*i)->describe(); ++i;
    while (i != pocket.end()) {
      s += ", " + (*i)->describe(); ++i;
    }
    s += "}, t = " + std::to_string(slate) + "\n";
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
    PROTECT(Pocket = NEW_LIST(pocket.size()));
    int k = 0;
    for (ball_it i = pocket.begin(); i != pocket.end(); i++) {
      SET_ELEMENT(Pocket,k++,(*i)->structure());
    }
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
    std::string o;
    std::string t = tab + "  ";
    o += "name: " + std::to_string(uniq) + "\n"
      + tab + "deme: " + std::to_string(deme) + "\n"
      + tab + "time: " + std::to_string(slate) + "\n"
      + tab + "pocket:\n";
    for (ball_it i = pocket.begin(); i != pocket.end(); i++) {
      o += tab + "- " + (*i)->yaml(t);
    }
    return o;
  };
  // Newick format
  std::string newick (const slate_t& tnow, const slate_t& tpar) const {
    std::string o = "(";
    int n = nchildren(false);
    for (ball_it i = pocket.begin(); i != pocket.end(); i++, n--) {
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
    for (ball_it i = pocket.begin(); i != pocket.end(); i++, n--) {
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
  }

 public:
  // size of binary serialization
  size_t size (void) const {
    return 2*sizeof(name_t)+sizeof(slate_t)
      +pocket.size()*sizeof(ball_t);
  };
  // binary serialization of node_t
  friend raw_t* operator<< (raw_t *o, const node_t &p) {
    name_t buf[3];
    buf[0] = p.uniq; buf[1] = p.deme; buf[2] = p.pocket.size();
    memcpy(o,buf,sizeof(buf)); o += sizeof(buf);
    memcpy(o,&p.slate,sizeof(slate_t)); o += sizeof(slate_t);
    for (ball_it i = p.pocket.begin(); i != p.pocket.end(); i++) 
      o = (o << **i);
    return o;
  };
  // binary deserialization of node_t
  friend raw_t* operator>> (raw_t *o, node_t &p) {
    name_t buf[3];
    memcpy(buf,o,sizeof(buf)); o += sizeof(buf);
    memcpy(&p.slate,o,sizeof(slate_t)); o += sizeof(slate_t);
    p.clean();
    p.uniq = buf[0]; p.deme = buf[1];
    for (size_t i = 0; i < buf[2]; i++) {
      ball_t *b = new ball_t(&p);
      o = (o >> *b);
      p.pocket.insert(b); b->holder(&p);
    }
    return o;
  };
};

#endif
