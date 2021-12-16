// -*- C++ -*-
// Generic Genealogy Process (GP) Simulator (C++)
// State of the GP is represented as a "tableau".

// TODO:

#ifndef _GP_H_
#define _GP_H_

#include <list>
#include <unordered_set>
#include <unordered_map>
#include <string>
#include <cstring>

#include "internal.h"

// interface with R's integer RNG
static int random_integer (int n) {
  return int(floor(R_unif_index(double(n))));
}

// helper function for filling a return list
static int set_list_elem (SEXP list, SEXP names, SEXP element,
                          const char *name, int pos) {
  SET_ELEMENT(list,pos,element);
  SET_STRING_ELT(names,pos,mkChar(name));
  return ++pos;
}

typedef Rbyte raw_t; // must match with R's 'Rbyte' (see Rinternals.h)
typedef size_t name_t;
typedef double slate_t;

const name_t na = name_t(R_NaInt);
const slate_t inf = R_PosInf;
const slate_t default_slate = R_NaReal;
const size_t MEMORY_MAX = (1<<26); // roughly 1/4 of mem/cpu

static const char *colores[] = {"green", "black", "blue", "red", "grey", "purple"};
static const char *colorsymb[] = {"g", "o", "b", "r", "z", "p"};

// GP TABLEAU CLASS
// the class to hold the state of the genealogy process (a "tableau")..
// STATE is a datatype that holds the state of the Markov process.
template <class STATE, class PARAMETERS, size_t NDEME = 1>
class tableau_t  {

private:

  // BALL COLORS
  // green must be first, numbers in sequence.
  static const name_t ncolors = sizeof(colores);
  typedef enum {green = 0, black = 1, blue = 2, red = 3, grey = 4, purple = 5} color_t;

  typedef STATE state_t;
  typedef PARAMETERS parameters_t;

private:

  class ball_t;
  class inventory_t;
  class node_t;

  typedef std::list<node_t*> nodes_t;
  typedef typename nodes_t::const_iterator node_it;
  typedef std::unordered_set<ball_t*> pocket_t;
  typedef typename pocket_t::const_iterator ball_it;
  
  name_t _unique;		// next unique name
  slate_t _t0;			// initial time
  slate_t _time;		// current time
  nodes_t nodes;		// pointers to all nodes

protected:

  inventory_t inventory;	// the inventory process
  state_t state;                // current state of the GP
  parameters_t params;		// parameters of the GP

private:

  // BALL CLASS
  // each ball has:
  // - a color
  // - a name, unique within its color
  // - a pointer to the node in whose pocket it lies.
  class ball_t {
  private:
    node_t *_holder;
    node_t *_owner;
  public:
    name_t uniq;
    color_t color;
    name_t deme;
    // basic constructor for ball class
    ball_t (node_t *who, name_t u = 0, color_t col = green) {
      _holder = who;
      _owner = who;
      uniq = u;
      color = col;
      deme = na;
    };
    // copy constructor
    ball_t (const ball_t &b) = delete;
    // move constructor
    ball_t (ball_t &&b) = delete;
    // copy assignment operator
    ball_t & operator= (const ball_t & b) = delete;
    // move assignment operator
    ball_t & operator= (ball_t && b) = delete;
    // destructor
    ~ball_t (void) = default;
    // who owns me?
    node_t* owner (void) const {
      if (color != green)
	err("do not ask who owns a ball that is not green.");
      return _owner;
    };
    // change owner
    void owner (node_t *who) {
      if (color != green)
	err("do not meddle in the ownership of non-green balls.");
      _owner = who;
    };
    // in whose pocket do I lie?
    node_t* holder (void) const {
      return _holder;
    };
    // change pockets
    void holder (node_t *who) {
      _holder = who;
    };
    node_t* child (void) const {
      return owner();
    };
    bool is (color_t c) const {
      return color==c;
    };
    // human-readable colors
    std::string color_name (void) const {
      return colores[color];
    };
    // machine-readable color symbols
    std::string color_symbol (void) const {
      return colorsymb[color];
    };
    // human-readable info
    std::string describe (void) const {
      return color_name()
	+ "(" + std::to_string(uniq)
	+ "," + std::to_string(deme) + ")";
    };
    // element of a newick representation
    std::string newick (const slate_t &t) const {
      if (deme == na) err("undefined deme");
      return color_symbol()
	+ "_" + std::to_string(deme)
	+ "_" + std::to_string(uniq)
	+ ":" + std::to_string(t);
    };
    // size of binary serialization
    size_t size (void) const {
      return 2*sizeof(name_t) + sizeof(color_t);
    };
    // binary serialization
    friend raw_t* operator<< (raw_t *o, const ball_t &b) {
      memcpy(o,&b.uniq,sizeof(name_t)); o += sizeof(name_t);
      memcpy(o,&b.deme,sizeof(name_t)); o += sizeof(name_t);
      memcpy(o,&b.color,sizeof(color_t)); o += sizeof(color_t);
      return o;
    };
    // binary deserialization
    friend raw_t* operator>> (raw_t *o, ball_t &b) {
      memcpy(&b.uniq,o,sizeof(name_t)); o += sizeof(name_t);
      memcpy(&b.deme,o,sizeof(name_t)); o += sizeof(name_t);
      memcpy(&b.color,o,sizeof(color_t)); o += sizeof(color_t);
      b.holder(0);
      return o;
    };
  };

  // INVENTORY CLASS
  class inventory_t {
  private:
    pocket_t _inven[NDEME];
  public:
    // basic constructor
    inventory_t (void) = default;
    // copy constructor
    inventory_t (const inventory_t &) = default;
    /// move constructor
    inventory_t (inventory_t &&) = delete;
    // copy assignment operator
    inventory_t & operator= (const inventory_t &) = delete;
    // move assignment operator
    inventory_t & operator= (inventory_t &&) = delete;
    // destructor
    ~inventory_t (void) = default;
    // draw a random integer in the interval [0,n-1]
    void draw_one (name_t n, name_t *x) const {
      *x = random_integer(n);
    };
    // draw a pair of random integers in the interval [0,n-1]
    void draw_two (name_t n, name_t *x) const {
      x[0] = random_integer(n);
      x[1] = random_integer(n-1);
      if (x[1] >= x[0]) x[1]++;
    };
    // n-th deme
    pocket_t & operator[] (const name_t n) {
      return _inven[n];
    };
    // n-th member of deme d
    ball_t * random_black_ball (name_t d) {
      name_t draw;
      name_t n = _inven[d].size();
      if (n < 1) err("cannot draw from empty inventory %ld",d);
      draw_one(n,&draw);
      ball_it i = _inven[d].begin();
      while (draw > 0 && i != _inven[d].end()) {
	draw--; i++;
      }
      return *i;
    };
    // add black ball to deme i;
    // remove from existing deme if necessary
    void insert (ball_t *b, name_t i) {
      if (b->is(black)) {
	if (b->deme != na) _inven[b->deme].erase(b);
	_inven[i].insert(b);
	b->deme = i;
      }
    };
    // remove black ball from its deme
    void remove (ball_t *b) {
      if (b->is(black)) {
	if (_inven[b->deme].empty())
	  err("in 'inventory::remove': empty deme %ld.",b->deme);
	_inven[b->deme].erase(b);
      }
    };
  };
  
  // NODE CLASS
  // each node has:
  // - a unique name
  // - a pocket containting two or more balls
  // - a slate with the time of seating
  // - knowledge of the Markov process state at time of seating
  // - a deme
  class node_t {

  private:

    ball_t *_greenball;

  public:
    
    name_t uniq, deme;
    pocket_t pocket;
    slate_t slate;
    state_t state;

    // basic constructor for node class
    node_t (name_t u = 0, slate_t t = na, name_t d = 0) {
      if (d >= NDEME) err("deme %ld does not exist",d);
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
      err("no ball of color %s",colores[c]);
      return 0;
    };
    // return a pointer to another ball
    ball_t *other (const ball_t *b) const {
      for (ball_it i = pocket.begin(); i != pocket.end(); i++) {
	if (*i != b) return *i;
      }
      err("error in 'other': no other ball");
      return 0;
    };
    // human-readable info
    std::string describe (void) const {
      std::string s = "node(" + std::to_string(uniq)
	+ "," + std::to_string(deme) + ") {";
      ball_it i = pocket.begin();
      s += (*i)->describe(); ++i;
      while (i != pocket.end()) {
        s += "," + (*i)->describe(); ++i;
      }
      s += "}, t = " + std::to_string(slate) + "\n";
      return s;
    };
    // Newick format
    std::string newick (const slate_t& tnow, const slate_t& tpar) const {
      std::string o = "(";
      bool single = true;
      for (ball_it i = pocket.begin(); i != pocket.end(); i++) {
	ball_t *b = *i;
	node_t *p = 0;
	if (!single) o += ",";
	single = is_root();
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
	default:
	  err("in 'newick': c'est impossible!");
	  break;
	}
      }
      o += ")g_"
	+ std::to_string(deme)
	+ "_" + std::to_string(uniq)
	+ ":" + std::to_string(slate - tpar);
      return o;
    };
    // compact Newick format
    std::string compact_newick (const slate_t& tnow, const slate_t& tpar) const {
      std::string o1 = "(";
      std::string o2 = "";
      std::string o3 = ")g_";
      bool single = true;
      bool rednode = false;
      for (ball_it i = pocket.begin(); i != pocket.end(); i++) {
	ball_t *b = *i;
	node_t *p = 0;
	if (!single) o2 += ",";
	single = is_root() || holds(red) || holds(blue) || holds(purple);
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
	default:
	  err("in 'compact_newick': c'est impossible!");
	  break;
	}
      }
      return o1 + o2 + o3
	+ std::to_string(deme)
	+ "_" + std::to_string(uniq)
	+ ":" + std::to_string(slate - tpar);
    }
    // size of binary serialization
    size_t size (void) const {
      return 2*sizeof(name_t)+sizeof(slate_t)
	+sizeof(state_t)+pocket.size()*sizeof(ball_t);
    };
    // binary serialization of node_t
    friend raw_t* operator<< (raw_t *o, const node_t &p) {
      name_t buf[3];
      buf[0] = p.uniq; buf[1] = p.deme; buf[2] = p.pocket.size();
      memcpy(o,buf,sizeof(buf)); o += sizeof(buf);
      memcpy(o,&p.slate,sizeof(slate_t)); o += sizeof(slate_t);
      memcpy(o,&p.state,sizeof(state_t)); o += sizeof(state_t);
      for (ball_it i = p.pocket.begin(); i != p.pocket.end(); i++) 
	o = (o << **i);
      return o;
    };
    // binary deserialization of node_t
    friend raw_t* operator>> (raw_t *o, node_t &p) {
      name_t buf[3];
      memcpy(buf,o,sizeof(buf)); o += sizeof(buf);
      memcpy(&p.slate,o,sizeof(slate_t)); o += sizeof(slate_t);
      memcpy(&p.state,o,sizeof(state_t)); o += sizeof(state_t);
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
    for (size_t d = 0; d < NDEME; d++) inventory[d].clear();
    _time = default_slate;
    _unique = 0;
  };

public:

  // size of serialized binary form
  size_t size (void) const {
    size_t s = 2*sizeof(name_t) + 2*sizeof(slate_t)
      + sizeof(state_t) + sizeof(parameters_t);
    for (node_it i = nodes.begin(); i != nodes.end(); i++)
      s += (*i)->size();
    return s;
  };

  // binary serialization of tableau_t
  friend raw_t* operator<< (raw_t *o, const tableau_t &T) {
    name_t A[2]; A[0] = T._unique; A[1] = T.nodes.size();
    slate_t B[2]; B[0] = T._t0; B[1] = T._time;
    memcpy(o,A,sizeof(A)); o += sizeof(A);
    memcpy(o,B,sizeof(B)); o += sizeof(B);
    memcpy(o,&T.state,sizeof(state_t)); o += sizeof(state_t);
    memcpy(o,&T.params,sizeof(parameters_t)); o += sizeof(parameters_t);
    for (node_it i = T.nodes.begin(); i != T.nodes.end(); i++) {
      o = (o << **i);
    }
    return o;
  }

  // binary deserialization of tableau_t
  friend raw_t* operator>> (raw_t *o, tableau_t &T) {
    name_t A[2];
    slate_t B[2];
    std::unordered_map<name_t,node_t*> nodeptr;
    typename std::unordered_map<name_t,node_t*>::const_iterator npit;
    memcpy(A,o,sizeof(A)); o += sizeof(A);
    memcpy(B,o,sizeof(B)); o += sizeof(B);
    memcpy(&T.state,o,sizeof(state_t)); o += sizeof(state_t);
    memcpy(&T.params,o,sizeof(parameters_t)); o += sizeof(parameters_t);
    T._unique = A[0]; T._t0 = B[0]; T._time = B[1];
    for (name_t i = 0; i < A[1]; i++) {
      node_t *p = new node_t();
      o = (o >> *p);
      T.nodes.push_back(p);
      nodeptr.insert({p->uniq,p});
    }
    for (node_it i = T.nodes.begin(); i != T.nodes.end(); i++) {
      node_t *p = *i;
      for (ball_it j = p->pocket.begin(); j != p->pocket.end(); j++) {
	ball_t *b = *j;
	T.inventory.insert(b,b->deme);
	if (b->is(green)) {
	  npit = nodeptr.find(b->uniq);
	  if (npit == nodeptr.end()) {
	    err("cannot find uniq id %ld",b->uniq);
	  } else {
	    node_t *q = npit->second;
	    b->owner(q); q->green_ball(b);
	  }
	}
      }
    }
    return o;
  }

public:
  
  // basic constructor
  //  t0 = initial time
  tableau_t (slate_t t0 = 0) {
    clean();
    _time = _t0 = t0;
  };
  // constructor from serialized binary form
  tableau_t (raw_t *o) {
    o >> *this;
  };
  // copy constructor
  tableau_t (const tableau_t & T) {
    raw_t *o = new raw_t[T.size()];
    o << T; o >> *this;
    delete[] o;
  };
  // move constructor
  tableau_t (tableau_t &&) = delete;
  // copy assignment operator
  tableau_t & operator= (const tableau_t & T) {
    clean();
    raw_t *o = new raw_t[T.size()];
    o << T; o >> *this;
    delete[] o;
    return *this;
  };
  // move assignment operator
  tableau_t & operator= (tableau_t &&) = delete;  // destructor
  virtual ~tableau_t (void) {
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

  // get zero time.
  slate_t timezero (void) const {
    return _t0;
  };

protected:

  // set current time.
  void time (const double t) {
    _time = slate_t(t);
  };

private:

  slate_t dawn (void) const {
    return (nodes.empty()) ? default_slate : nodes.front()->slate;
  };
  slate_t dusk (void) const {
    return (nodes.empty()) ? default_slate : nodes.back()->slate;
  }

  // get number of nodes
  name_t nnodes (void) const {
    return nodes.size();
  }
  
private:
  
  // report all the seating times and lineage count
  name_t lineage_count (slate_t *t = 0, int *ell = 0) const;

  // human-readable info
  std::string describe (void) const {
    std::string o = "";
    for (node_it p = nodes.begin(); p != nodes.end(); p++) {
      o += (*p)->describe();
    }
    o += "time = " + std::to_string(time()) + "\n";
    return o;
  };

public:

  // create a human-readable description
  friend void describe (SEXP x, int k, const tableau_t &T) {
    SET_STRING_ELT(x,k,mkChar(T.describe().c_str()));
  }

  friend SEXP describe (const tableau_t &T) {
    SEXP out;
    PROTECT(out = NEW_CHARACTER(1));
    SET_STRING_ELT(out,0,mkChar(T.describe().c_str()));
    UNPROTECT(1);
    return out;
  }

private:

  // put genealogy at current time into Newick format.
  std::string newick (bool compact = true) const {
    slate_t te = dawn(), tl = time();
    std::string o = std::to_string(tl) + "(i_NA_NA:0.0,i_NA_NA:0.0";
    for (node_it i = nodes.begin(); i != nodes.end(); i++) {
      node_t *p = *i;
      if (p->is_root()) {
	o += ",(" + ((compact) ? p->compact_newick(time(),te) : p->newick(time(),te)) + ")i_NA_NA:0.0";
      }
    }
    o += ")i_NA_NA;";
    return o;
  };

public:
  
  // extract the tree structure in Newick form.
  // store in element k of character-vector x.
  friend void newick (SEXP x, int k, const tableau_t &T, bool compact = false) {
    SET_STRING_ELT(x,k,mkChar(T.newick(compact).c_str()));
  }

  friend SEXP newick (const tableau_t &T, bool compact = false) {
    SEXP x;
    PROTECT(x = NEW_CHARACTER(1));
    SET_STRING_ELT(x,0,mkChar(T.newick(compact).c_str()));
    UNPROTECT(1);
    return x;
  }

protected:

  // check the validity of the tableau.
  void valid (void) const {};

private:

  // draw random black ball
  ball_t *random_black_ball (name_t d = 0) {
    return inventory.random_black_ball(d);
  };

public:

  bool max_size_exceeded (size_t grace = 0) const {
    static size_t maxq = MEMORY_MAX/(sizeof(node_t)+2*sizeof(ball_t));
    return (nodes.size() > maxq+grace);
  };

private:

  node_t* make_node (color_t col, name_t d = 0) {
    if (max_size_exceeded(1))
      err("maximum tableau size exceeded!");
    name_t u = unique();
    node_t *p = new node_t(u,_time,d);
    ball_t *g = new ball_t(p,u,green);
    ball_t *b = new ball_t(p,u,col);
    p->green_ball(g);
    g->deme = d; b->deme = d;
    p->pocket.insert(g);
    p->pocket.insert(b);
    inventory.insert(b,d);
    return p;
  };

  void drop_node (node_t *p) {
    if (!p->holds_own())
      err("cannot drop a node that does not hold its own green ball.");
    if (p->pocket.size() != 2)
      err("cannot drop a node with more than 2 balls.");
    for (ball_it i = p->pocket.begin(); i != p->pocket.end(); i++)
      inventory.remove(*i);
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

  // seat node p; take as parent the node holding ball b.
  void seat (node_t *p, ball_t *b) {
    swap(b,p->green_ball());
    p->deme = p->green_ball()->deme = b->deme;
    nodes.push_back(p);
  };

  // unseat the node holding black ball a.
  void unseat (ball_t *a) {
    if (!a->is(black))
      err("in 'unseat': ball is %s, not black.",colores[a->color]);
    node_t *p = a->holder();
    if (p->pocket.size() > 2) {
      p->pocket.erase(a);
      inventory.remove(a);
      delete a;
    } else {
      ball_t *b = p->other(a);
      switch (b->color) {
      case blue:		// change black ball for red ball
	inventory.remove(a);
	a->color = red;
	break;
      case purple:	// swap black ball for green ball, delete node
	swap(a,p->green_ball());
	a->deme = p->green_ball()->deme;
	drop_node(p);
	unseat(a);		// recursively pursue dropping ball a
	break;
      case red: case grey:
	err("in 'unseat': inconceivable error.");
	break;
      default:		// swap other ball for green ball, delete node
	swap(b,p->green_ball());
	b->deme = p->deme;
	drop_node(p);
	break;
      }
    }
  };

  // birth into deme j with parent in deme i
  void birth (const state_t &s, name_t i = 0, name_t j = 0) {
    ball_t *a = random_black_ball(i);
    node_t *p = make_node(black,j);
    p->slate = time();
    p->state = s;
    seat(p,a);
  };
  // death from deme i
  void death (const state_t &s, name_t i = 0) {
    ball_t *a = random_black_ball(i);
    unseat(a);
  };
  // graft a new lineage into deme i
  void graft (const state_t &s, name_t i = 0) {
    node_t *p = make_node(black,i);
    p->slate = time();
    p->state = s;
    nodes.push_back(p);
  };
  // insert a sample node into deme i
  void sample (const state_t &s, name_t i = 0) {
    ball_t *a = random_black_ball(i);
    node_t *p = make_node(blue,i);
    p->slate = time();
    p->state = s;
    seat(p,a);
  };
  // movement from deme i to deme j
  void migrate (const state_t &s, name_t i = 0, name_t j = 0) {
    ball_t *a = random_black_ball(i);
    node_t *p = make_node(purple,j);
    p->slate = time();
    p->state = s;
    seat(p,a);
  };

public:

  // prune the tree (drop all black balls)
  tableau_t &prune (void) {
    for (size_t d = 0; d < NDEME; d++) {
      while (!inventory[d].empty()) {
	ball_t *b = *(inventory[d].begin());
	unseat(b);
      }
    }
    return *this;
  };

  void birth (name_t i = 0, name_t j = 0) {
    birth(this->state,i,j);
  };
  
  void death (name_t i = 0) {
    death(this->state,i);
  };
  
  void graft (name_t i = 0) {
    graft(this->state,i);
  };

  void sample (name_t i = 0) {
    sample(this->state,i);
  };
  
  void migrate (name_t i = 0, name_t j = 0) {
    migrate(this->state,i,j);
  };
  
  // determines if process is still alive
  virtual bool live (void) const = 0;
  // returns time of next event
  virtual slate_t clock (void) const = 0;
  // updates clocks
  virtual void update_clocks (void) = 0;
  // makes a move
  virtual void jump (void) = 0;

public:

  // run process to a specified time.
  // return number of events that have occurred.
  int play (double tfin) {
    int count = R_NaInt;

    if (max_size_exceeded())
      warn("maximum tableau size reached.");
    
    if (!live()) return count;

    double next = clock();

    count = 0;
    while (next < tfin && live()) {
      _time = next;
      jump();
      next = clock();
      count++;
    }
    if (next > tfin)  _time = tfin; // relies on Markov property

    return count;
  };

  // take one step of the process
  // return new time.
  slate_t play1 (void);

  friend tableau_t* prune (tableau_t & T) {
    tableau_t *U = new tableau_t(T);
    U->prune();
    return U;
  }

};

// create the serialized state:
template <class GPTYPE>
SEXP serial (const GPTYPE &T) {
  SEXP out;
  PROTECT(out = NEW_RAW(T.size()));
  RAW(out) << T;
  UNPROTECT(1);
  return out;
}

// play a genealogy process
// this requires that the RNG state has been handled elsewhere
template<class GPTYPE>
SEXP playGP (GPTYPE *gp, SEXP Times, SEXP Tree, SEXP Compact) {
  int nprotect = 0;
  int nout = 3;
  int ntimes = LENGTH(Times);

  gp->valid();

  SEXP times, count;
  PROTECT(times = AS_NUMERIC(duplicate(Times))); nprotect++;
  PROTECT(count = NEW_INTEGER(ntimes)); nprotect++;

  SEXP tree = R_NilValue;
  int do_tree = *(INTEGER(AS_INTEGER(Tree)));
  if (do_tree) {
    PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  bool compact = *LOGICAL(AS_LOGICAL(Compact));

  int *xc = INTEGER(count);
  slate_t *xt = REAL(times);

  if (gp->time() > xt[0]) err("must not have t0 = %lg > %g = times[1]!",gp->time(),xt[0]);

  for (int k = 0; k < ntimes; k++, xc++, xt++) {
    *xc = gp->play(*xt);
    if (do_tree) newick(tree,k,*gp,compact);
    R_CheckUserInterrupt();
  }
      
  gp->valid();
    
  // pack everything up in a list
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout)); nprotect++;
  PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
  k = set_list_elem(out,outnames,times,"time",k);
  k = set_list_elem(out,outnames,count,"count",k);
  if (do_tree) {
    k = set_list_elem(out,outnames,tree,"tree",k);
  }
  k = set_list_elem(out,outnames,serial(*gp),"state",k);
  SET_NAMES(out,outnames);

  UNPROTECT(nprotect);
  return out;
}

// play a sampled genealogy process
// this requires that the RNG state has been handled elsewhere
template<class GPTYPE>
SEXP playSGP (GPTYPE *gp, SEXP Times, SEXP Tree, SEXP Compact) {
  int nprotect = 0;
  int nout = 3;
  int ntimes = LENGTH(Times);

  gp->valid();

  SEXP times, count;
  PROTECT(times = AS_NUMERIC(duplicate(Times))); nprotect++;
  PROTECT(count = NEW_INTEGER(ntimes)); nprotect++;

  SEXP tree = R_NilValue;
  int do_tree = *(INTEGER(AS_INTEGER(Tree)));
  if (do_tree) {
    PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  bool compact = *LOGICAL(AS_LOGICAL(Compact));

  int *xc = INTEGER(count);
  slate_t *xt = REAL(times);

  if (gp->time() > xt[0]) err("must not have t0 = %lg > %g = times[1]!",gp->time(),xt[0]);

  for (int k = 0; k < ntimes; k++, xc++, xt++) {
    *xc = gp->play(*xt);
    gp->sample();
    if (do_tree) newick(tree,k,*gp,compact);
    R_CheckUserInterrupt();
  }
      
  gp->valid();
    
  // pack everything up in a list
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout)); nprotect++;
  PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
  k = set_list_elem(out,outnames,times,"time",k);
  k = set_list_elem(out,outnames,count,"count",k);
  if (do_tree) {
    k = set_list_elem(out,outnames,tree,"tree",k);
  }
  k = set_list_elem(out,outnames,serial(*gp),"state",k);
  SET_NAMES(out,outnames);

  UNPROTECT(nprotect);
  return out;
}

template<class GPTYPE>
SEXP playWChain (GPTYPE *gp, SEXP N, SEXP Tree, SEXP Compact) {
  int nprotect = 0;
  int nout = 2;
  int ntimes = *(INTEGER(AS_INTEGER(N)));

  gp->valid();

  SEXP times;
  PROTECT(times = NEW_NUMERIC(ntimes)); nprotect++;

  SEXP tree = R_NilValue;
  int do_tree = *(INTEGER(AS_INTEGER(Tree)));
  if (do_tree) {
    PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  bool compact = *LOGICAL(AS_LOGICAL(Compact));

  slate_t *xt = REAL(times);

  GetRNGstate();
  for (int k = 0; k < ntimes; k++, xt++) {
    *xt = gp->play1();
    if (do_tree) newick(tree,k,*gp,compact);
    R_CheckUserInterrupt();
  }
  PutRNGstate();
      
  gp->valid();
    
  // pack everything up in a list
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout)); nprotect++;
  PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
  k = set_list_elem(out,outnames,times,"time",k);
  if (do_tree) {
    k = set_list_elem(out,outnames,tree,"tree",k);
  }
  k = set_list_elem(out,outnames,serial(*gp),"state",k);
  SET_NAMES(out,outnames);

  UNPROTECT(nprotect);
  return out;
}

// extract/compute basic information.
template <class GPTYPE>
SEXP get_info (SEXP X, SEXP Prune, SEXP Compact) {
  int nprotect = 0;
  
  // reconstruct the tableau from its serialization
  GPTYPE gp(RAW(X));
  // check validity
  gp.valid();
  
  // extract current time
  SEXP t0, tout;
  PROTECT(t0 = NEW_NUMERIC(1)); nprotect++;
  *REAL(t0) = gp.timezero();
  PROTECT(tout = NEW_NUMERIC(1)); nprotect++;
  *REAL(tout) = gp.time();

  bool compact = *LOGICAL(AS_LOGICAL(Compact));

  // prune if requested
  if (*(LOGICAL(AS_LOGICAL(Prune)))) gp.prune();

  // pack up return values in a list
  int nout = 4;
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout)); nprotect++;
  PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
  k = set_list_elem(out,outnames,t0,"t0",k);
  k = set_list_elem(out,outnames,tout,"time",k);
  k = set_list_elem(out,outnames,describe(gp),"description",k);
  //  k = set_list_elem(out,outnames,lineage_count(gp),"lineages",k);
  k = set_list_elem(out,outnames,newick(gp,compact),"tree",k);
  SET_NAMES(out,outnames);

  UNPROTECT(nprotect);
  return out;
}

#endif
