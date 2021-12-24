// -*- C++ -*-
// Generic Genealogy Process (GP) Simulator (C++)

#ifndef _GENEALOGY_H_
#define _GENEALOGY_H_

#include <unordered_map>
#include <string>
#include <cstring>
#include <utility>

#include "internal.h"
#include "ball.h"
#include "node.h"
#include "inventory.h"

static const size_t MEMORY_MAX = (1<<28); // 256MB

// GENEALOGY CLASS
// the class to hold the state of the genealogy process.
template<size_t ndeme = 1>
class genealogy_t {

private:
  
  // GENEALOGY member data:
  // - a counter of serial numbers
  // - an initial time
  // - the current time
  // - a sequence of nodes
  
  name_t _unique;               // next unique name
  slate_t _t0;                  // initial time
  slate_t _time;                // current time

public:

  nodes_t nodes;                // sequence of pointers to nodes

private:

  // get the next unique name
  name_t unique (void) {
    name_t u = _unique;
    _unique++;
    return u;
  };

  // clean up: delete all nodes, reset globals
  void clean (void) {
    for (node_it i = nodes.begin(); i != nodes.end(); i++) delete *i;
    nodes.clear();
    _time = R_NaReal;
    _unique = 0;
  };

public:
  // SERIALIZATION
  // size of serialized binary form
  size_t bytesize (void) const {
    size_t s = 2*sizeof(name_t) + 2*sizeof(slate_t);
    for (node_it i = nodes.begin(); i != nodes.end(); i++)
      s += (*i)->bytesize();
    return s;
  };
  // binary serialization of genealogy_t
  raw_t* serialize (raw_t *o) const {
    name_t A[2]; A[0] = _unique; A[1] = nodes.size();
    slate_t B[2]; B[0] = _t0; B[1] = _time;
    memcpy(o,A,sizeof(A)); o += sizeof(A);
    memcpy(o,B,sizeof(B)); o += sizeof(B);
    for (node_it i = nodes.begin(); i != nodes.end(); i++) {
      o = (*i)->serialize(o);
    }
    return o;
  };
  // binary deserialization of genealogy_t
  raw_t* deserialize (raw_t *o) {
    clean();
    name_t A[2];
    slate_t B[2];
    std::unordered_map<name_t,node_t*> nodeptr;
    typename std::unordered_map<name_t,node_t*>::const_iterator npit;
    memcpy(A,o,sizeof(A)); o += sizeof(A);
    memcpy(B,o,sizeof(B)); o += sizeof(B);
    _unique = A[0]; _t0 = B[0]; _time = B[1];
    size_t nnode = A[1];
    nodeptr.reserve(nnode);
    for (size_t i = 0; i < nnode; i++) {
      node_t *p = new node_t();
      o = p->deserialize(o);
      nodes.push_back(p);
      nodeptr.insert({p->uniq,p});
    }
    for (node_it i = nodes.begin(); i != nodes.end(); i++) {
      for (ball_it j = (*i)->pocket.begin(); j != (*i)->pocket.end(); j++) {
        ball_t *b = *j;
        if (b->is(green)) {
          npit = nodeptr.find(b->uniq);
          if (npit == nodeptr.end()) {
            err("cannot find unique id %ld",b->uniq); // # nocov
          } else {
            node_t *q = npit->second;
            b->owner(q); q->green_ball(b);
          }
        }
      }
    }
    return o;
  };
  friend raw_t* operator<< (raw_t *o, const genealogy_t &G) {
    return G.serialize(o);
  }
  friend raw_t* operator>> (raw_t *o, genealogy_t &G) {
    return G.deserialize(o);
  }

public:
  // CONSTRUCTORS
  // basic constructor for genealogy class
  //  t0 = initial time
  genealogy_t (double t0 = 0) {
    clean();
    _time = _t0 = slate_t(t0);
  };
  // constructor from serialized binary form
  genealogy_t (raw_t *o) {
    o >> *this;
  };
  // copy constructor
  genealogy_t (const genealogy_t& G) {
    raw_t *o = new raw_t[G.bytesize()];
    (o << G) >> *this;
    delete[] o;
  };
  // copy assignment operator
  genealogy_t & operator= (const genealogy_t& G) {
    clean();
    raw_t *o = new raw_t[G.bytesize()];
    o << G; o >> *this;
    delete[] o;
    return *this;
  };
  // move constructor
  genealogy_t (genealogy_t&&) = delete;
  // move assignment operator
  genealogy_t& operator= (genealogy_t&&) = delete;
  // destructor
  ~genealogy_t (void) {
    clean();
  };
  
  // is empty?
  bool empty (void) const {
    return nodes.empty();
  };
  // get current time.
  slate_t time (void) const {
    return _time;
  };
  // reset current time.
  void time (slate_t t) {
    _time = t;
  };
  // get zero time.
  slate_t timezero (void) const {
    return _t0;
  };

private:

  slate_t dawn (void) const {
    return (nodes.empty()) ? R_NaReal : nodes.front()->slate;
  };
  slate_t dusk (void) const {
    return (nodes.empty()) ? R_NaReal : nodes.back()->slate;
  }

public:
  
  // report all the node times and lineage count
  size_t lineage_count (double *t = 0, int *ell = 0) const {
    size_t count = 1;
    int n = 0;
    slate_t tcur = R_NegInf;
    for (node_it i = nodes.begin(); i != nodes.end(); i++) {
      if ((*i)->is_root()) n++;
      if (tcur < (*i)->slate) {
        tcur = (*i)->slate;
        count++;
      }
    }
    if (t != 0 && ell != 0) {
      tcur = R_NegInf;
      *ell = n;
      for (node_it i = nodes.begin(); i != nodes.end(); i++) {
        n += (*i)->nchildren(true)-1;
        if (tcur < (*i)->slate) {
          *(t++) = tcur = (*i)->slate;
          *(ell++) = n;
        }
      }
      *t = time();
      *ell = 0;
    }
    return count;
  };

public:
  
  // R list description
  SEXP structure (void) const {
    SEXP O, On, Ndeme, Time, Nodes;
    PROTECT(O = NEW_LIST(3));
    PROTECT(On = NEW_CHARACTER(3));
    PROTECT(Ndeme = NEW_INTEGER(1));
    *INTEGER(Ndeme) = int(ndeme);
    PROTECT(Time = NEW_NUMERIC(1));
    *REAL(Time) = double(time());
    PROTECT(Nodes = NEW_LIST(nodes.size()));
    int k = 0;
    for (node_it i = nodes.begin(); i != nodes.end(); i++) {
      SET_ELEMENT(Nodes,k++,(*i)->structure());
    }
    set_list_elem(O,On,Ndeme,"ndemes",0);
    set_list_elem(O,On,Time,"time",1);
    set_list_elem(O,On,Nodes,"nodes",2);
    SET_NAMES(O,On);
    UNPROTECT(5);
    return O;
  };
  
public:

  // human-readable info
  std::string describe (void) const {
    std::string o = "time = " + std::to_string(time()) + "\n";
    for (node_it p = nodes.begin(); p != nodes.end(); p++) {
      o += (*p)->describe();
    }
    return o;
  };

public:

  friend SEXP describe (const genealogy_t& G) {
    SEXP out;
    PROTECT(out = NEW_CHARACTER(1));
    SET_STRING_ELT(out,0,mkChar(G.describe().c_str()));
    UNPROTECT(1);
    return out;
  }

public:
  
  // machine-readable info
  virtual std::string yaml (std::string tab = "") const {
    std::string o;
    std::string t = tab + "  ";
    o = "ndemes: " + std::to_string(ndeme) + "\n"
      + tab + "time: " + std::to_string(time()) + "\n"
      + tab + "nodes:\n";
    for (node_it p = nodes.begin(); p != nodes.end(); p++) {
      o += tab + "- " + (*p)->yaml(t);
    }
    return o;
  };

public:

  // put genealogy at current time into Newick format.
  std::string newick (bool compact = true) const {
    slate_t te = dawn();
    std::string o = "(i_NA_NA:0.0,i_NA_NA:0.0";
    for (node_it i = nodes.begin(); i != nodes.end(); i++) {
      node_t *p = *i;
      if (p->is_root()) {
        o += ",(" + ((compact) ? p->compact_newick(time(),te) : p->newick(time(),te)) + ")i_NA_NA:0.0";
      }
    }
    o += ")i_NA_NA;";
    return o;
  };

protected:

  // check the validity of the genealogy.
  void valid (void) const {};

public:

  bool check_genealogy_size (size_t grace = 0) const {
    static size_t maxq = MEMORY_MAX/(sizeof(node_t)+2*sizeof(ball_t));
    bool ok = true;
    if (nodes.size() > maxq+grace) {
      err("maximum genealogy size exceeded!");
    } else if (nodes.size() > maxq) {
      ok = false;
    }
    return ok;
  };

private:

  node_t* make_node (color_t col, name_t d = 0) {
    check_genealogy_size(0);
    name_t u = unique();
    node_t *p = new node_t(u,_time,d);
    ball_t *g = new ball_t(p,u,green,d);
    ball_t *b = new ball_t(p,u,col,d);
    p->green_ball(g);
    p->pocket.insert(g);
    p->pocket.insert(b);
    return p;
  };

  void destroy_node (node_t *p) {
    if (!p->holds_own())
      err("cannot destroy a node that does not hold its own green ball."); // # nocov
    if (p->pocket.size() != 2)
      err("cannot destroy a node with more than 2 balls."); // # nocov
    nodes.remove(p);
    delete p;
  };
  
  // swap balls a and b, wherever they lie
  void swap (ball_t *a, ball_t *b) {
    node_t *p = a->holder();
    node_t *q = b->holder();
    if (p != q) {
      q->pocket.insert(a); p->pocket.erase(a); a->holder(q);
      p->pocket.insert(b); q->pocket.erase(b); b->holder(p);
    }
  };

  // add node p; take as parent the node holding ball b.
  void add (node_t *p, ball_t *b) {
    swap(b,p->green_ball());
    p->deme = b->deme;
    nodes.push_back(p);
  };

  // drop the node holding black ball a.
  void drop (ball_t *a) {
    if (!a->is(black))
      err("in 'drop': inconceivable! color: %s",colores[a->color]); // # nocov
    node_t *p = a->holder();
    if (p->pocket.size() > 2) { // pocket is large: we simply drop the ball
      p->pocket.erase(a);
      delete a;
    } else {	  // pocket is tight: action depends on the other ball
      ball_t *b = p->other(a);
      switch (b->color) {
      case blue:                // change black ball for red ball
        a->color = red;
        break;
      case purple:      // swap black ball for green ball, delete node
        swap(a,p->green_ball());
        destroy_node(p);
        drop(a);		// recursively pursue dropping ball a
        break;
      case red: case grey:			// # nocov
        err("in 'drop': inconceivable error."); // # nocov
        break;
      case black: case green:	// swap other for green, delete node
        swap(b,p->green_ball());
        destroy_node(p);
        break;
      }
    }
  };

public:
  // birth into deme d 
  ball_t* birth (ball_t* a, slate_t t, name_t d = 0) {
    time(t);
    node_t *p = make_node(black,d);
    ball_t *b = p->ball(black);	// TODO: OBVIATE SEARCH FOR NEW BALL
    p->slate = time();
    add(p,a);
    return b;		
  };
  // birth of second or subsequent sibling into deme d
  ball_t* birth (node_t* p, name_t d = 0) {
    ball_t *b = new ball_t(p,unique(),black,d);
    p->pocket.insert(b);
    return b;
  };
  // death
  void death (ball_t *a, slate_t t) {
    time(t);
    drop(a);
  };
  // graft a new lineage into deme d
  ball_t* graft (slate_t t, name_t d = 0) {
    time(t);
    node_t *p = make_node(black,d);
    ball_t *b = p->ball(black);
    p->slate = timezero();
    nodes.push_front(p);
    return b;
  };
  // insert a sample node
  void sample (ball_t* a, slate_t t) {
    time(t);
    node_t *p = make_node(blue,a->deme);
    p->slate = time();
    add(p,a);
  };
  // movement into deme d
  ball_t* migrate (ball_t* a, slate_t t, name_t d = 0) {
    time(t);
    node_t *p = make_node(purple,a->deme);
    p->slate = time();
    a->deme = d;
    add(p,a);
    return a;
  };

  // set up for extraction of black balls
  std::pair<node_it, node_it> extant (void) const {
    return std::pair<node_it,node_it>(nodes.cbegin(),nodes.cend());
  };

  // all balls of a color
  pocket_t* colored (color_t col) const {
    pocket_t *p = new pocket_t;
    for (node_it i = nodes.begin(); i != nodes.end(); i++) {
      for (ball_it j = (*i)->pocket.begin(); j != (*i)->pocket.end(); j++) {
	if ((*j)->is(col)) p->insert(*j);
      }
    }
    return p;
  };

  // prune the tree (drop all black balls)
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

  // drop all purple balls
  // and erase all deme information
  genealogy_t& obscure (void) {
    pocket_t *purples = colored(purple);
    while (!purples->empty()) {
      ball_t *a = *(purples->begin());
      node_t *p = a->holder();
      ball_t *b = p->other(a);
      swap(b,p->green_ball());
      destroy_node(p);
      purples->erase(a);
    }
    delete purples;
    pocket_t *blacks = colored(black);
    while (!blacks->empty()) {
      ball_t *a = *(blacks->begin());
      a->deme = 0;
      blacks->erase(a);
    }
    delete blacks;
    for (node_it i = nodes.begin(); i != nodes.end(); i++)
      (*i)->deme = 0;
    return *this;
  };

  // truncate the genealogy by removing nodes
  // with times later than tnew
  // NB: this destroys the genealogy inasmuch
  // as the state is no longer correct.
  void truncate (slate_t tnew) {
    if (!nodes.empty()) {
      node_t *n = nodes.back();
      while (!nodes.empty() && n->slate > tnew) {
	if (n->holds(black)) {
	  ball_t *b = n->ball(black);
	  drop(b);
	} else if (n->holds(red)) {
	  ball_t *b = n->ball(red);
	  b->color = black;
	  swap(b,n->green_ball());
	  destroy_node(n);
	} else {
	  err("in 'truncate': inconceivable error."); // #nocov
	}
	n = nodes.back();
      }
      time(tnew);
    }
  };
};

#endif
