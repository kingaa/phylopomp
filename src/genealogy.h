// -*- C++ -*-
// GENEALOGY class

#ifndef _GENEALOGY_H_
#define _GENEALOGY_H_

#include <string>
#include <cstring>
#include <utility>

#include "internal.h"
#include "nodeseq.h"
#include "inventory.h"

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
  
  //! The number of demes.
  size_t ndeme;
  //! The next unique name.
  name_t _unique;               
  //! The initial time.
  slate_t _t0;
  //! The current time.
  slate_t _time;
  
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
    _t0 = _time = R_NaReal;
  };

public:

  void set_unique (name_t u) {
    if (u < _unique) warn("potential re-use of names!");
    _unique = u;
  };

public:
  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    return 2*sizeof(name_t) +
      2*sizeof(slate_t) + nodeseq_t::bytesize();
  };
  //! binary serialization
  friend raw_t* operator>> (const genealogy_t& G, raw_t* o) {
    name_t A[2]; A[0] = G._unique; A[1] = name_t(G.ndeme);
    slate_t B[2]; B[0] = G._t0; B[1] = G._time;
    memcpy(o,A,sizeof(A)); o += sizeof(A);
    memcpy(o,B,sizeof(B)); o += sizeof(B);
    return reinterpret_cast<const nodeseq_t&>(G) >> o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, genealogy_t& G) {
    G.clean();
    name_t A[2];
    slate_t B[2];
    memcpy(A,o,sizeof(A)); o += sizeof(A);
    memcpy(B,o,sizeof(B)); o += sizeof(B);
    G._unique = A[0]; G.ndeme = size_t(A[1]);
    G._t0 = B[0]; G._time = B[1];
    return o >> reinterpret_cast<nodeseq_t&>(G);
  };

public:
  // CONSTRUCTORS
  //! basic constructor for genealogy class
  //!  t0 = initial time
  genealogy_t (double t0 = 0, name_t u = 0, size_t nd = 1) {
    clean();
    ndeme = nd;
    _unique = u;
    _time = _t0 = slate_t(t0);
  };
  //! constructor from serialized binary form
  genealogy_t (raw_t *o) {
    o >> *this;
  };
  //! copy constructor
  genealogy_t (const genealogy_t& G) {
    raw_t *o = new raw_t[G.bytesize()];
    G >> o >> *this;
    delete[] o;
  };
  //! copy assignment operator
  genealogy_t & operator= (const genealogy_t& G) {
    clean();
    raw_t *o = new raw_t[G.bytesize()];
    G >> o >> *this;
    delete[] o;
    return *this;
  };
  //! move constructor
  genealogy_t (genealogy_t&&) = delete;
  //! move assignment operator
  genealogy_t& operator= (genealogy_t&&) = delete;
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
  //! get zero time.
  slate_t timezero (void) const {
    return _t0;
  };

public:
  
  //! lineage count and saturation
  void lineage_count (double *t, int *deme, int *ell, int *sat) const {
    slate_t tcur = *t = timezero();
    for (size_t j = 0; j < ndeme; j++) {
      sat[j] = ell[j] = 0;
      deme[j] = j+1;
    }
    for (node_it i = begin(); i != end(); i++) {
      if (tcur < (*i)->slate) {
        t++; ell += ndeme; sat += ndeme; deme += ndeme;
        *t = tcur = (*i)->slate;
        for (size_t j = 0; j < ndeme; j++) {
	  ell[j] = (ell-ndeme)[j];
	  sat[j] = 0;
	  deme[j] = j+1;
	}
      }
      (*i)->lineage_incr(ell,sat);
    }
    t++; ell += ndeme; sat += ndeme; deme += ndeme;
    *t = time();
    for (size_t j = 0; j < ndeme; j++) {
      sat[j] = ell[j] = 0;
      deme[j] = j+1;
    }
  };
  //! lineage count and saturation
  SEXP lineage_count (void) const {
    SEXP t, deme, ell, sat, out, outn;
    int nt = ntime(timezero())+1;
    int nl = ndeme*nt;
    PROTECT(t = NEW_NUMERIC(nt));
    PROTECT(deme = NEW_INTEGER(nl));
    PROTECT(ell = NEW_INTEGER(nl));
    PROTECT(sat = NEW_INTEGER(nl));
    PROTECT(out = NEW_LIST(4));
    PROTECT(outn = NEW_CHARACTER(4));
    set_list_elem(out,outn,t,"time",0);
    set_list_elem(out,outn,deme,"deme",1);
    set_list_elem(out,outn,ell,"lineages",2);
    set_list_elem(out,outn,sat,"saturation",3);
    SET_NAMES(out,outn);
    lineage_count(REAL(t),INTEGER(deme),INTEGER(ell),INTEGER(sat));
    UNPROTECT(6);
    return out;
  };

public:
  
  //! R list description
  SEXP structure (void) const {
    SEXP O, On, T0, Time, Nodes;
    PROTECT(O = NEW_LIST(3));
    PROTECT(On = NEW_CHARACTER(3));
    PROTECT(Time = NEW_NUMERIC(1));
    *REAL(Time) = double(time());
    PROTECT(T0 = NEW_NUMERIC(1));
    *REAL(T0) = double(timezero());
    PROTECT(Nodes = nodeseq_t::structure());
    set_list_elem(O,On,Time,"time",0);
    set_list_elem(O,On,T0,"t0",1);
    set_list_elem(O,On,Nodes,"nodes",2);
    SET_NAMES(O,On);
    UNPROTECT(5);
    return O;
  };
  
public:

  //! human-readable info
  std::string describe (void) const {
    std::string o = "time = " + std::to_string(time()) + "\n"
      + "t0 = " + std::to_string(timezero()) + "\n"
      + nodeseq_t::describe();
    return o;
  };

public:
  
  //! machine-readable info
  virtual std::string yaml (std::string tab = "") const {
    std::string o;
    std::string t = tab + "  ";
    o = tab + "t0: " + std::to_string(timezero()) + "\n"
      + tab + "time: " + std::to_string(time()) + "\n"
      + tab + "nodes:\n" + nodeseq_t::yaml(tab);
    return o;
  };

public:

  //! put genealogy at current time into Newick format.
  std::string newick (void) const {
    return nodeseq_t::newick(time());
  };

public:

  //! check the validity of the genealogy.
  void valid (void) const {};

  bool check_genealogy_size (size_t grace = 0) const {
    static size_t maxq = MEMORY_MAX/(sizeof(node_t)+2*sizeof(ball_t));
    bool ok = true;
    if (size() > maxq+grace) {
      err("maximum genealogy size exceeded!");
    } else if (size() > maxq) {
      ok = false;
    }
    return ok;
  };

public:

  node_t* make_node (name_t d = 0) {
    check_genealogy_size(0);
    name_t u = unique();
    node_t *p = new node_t(u,_time,d);
    ball_t *g = new ball_t(p,u,green,d);
    p->green_ball() = g;
    p->insert(g);
    return p;
  };

public:
  //! birth into deme d 
  ball_t* birth (ball_t* a, slate_t t, name_t d = 0) {
    time() = t;
    node_t *p = make_node(a->deme());
    ball_t *b = new ball_t (p,p->uniq,black,d);
    p->insert(b);
    p->slate = time();
    add(p,a);
    return b;           
  };
  //! birth of second or subsequent sibling into deme d
  ball_t* birth (node_t* p, name_t d = 0) {
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
  ball_t* graft (slate_t t, name_t d = 0) {
    time() = t;
    node_t *p = make_node(d);
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
  //! movement into deme d
  ball_t* migrate (ball_t* a, slate_t t, name_t d = 0) {
    time() = t;
    node_t *p = make_node(a->deme());
    p->slate = time();
    add(p,a);
    a->deme() = d;
    return a;
  };

  //! set up for extraction of black balls
  //! (see 'inventory.h')
  std::pair<node_it, node_it> extant (void) const {
    return std::pair<node_it,node_it>(cbegin(),cend());
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
      a->deme() = 0;
      blacks->erase(a);
    }
    delete blacks;
    // erase deme information from nodes.
    for (node_it i = begin(); i != end(); i++) {
      (*i)->deme = 0;
    }
    // drop superfluous nodes (holding just one ball).
    comb();
    ndeme = 1;
    return *this;
  };
  //! truncate the genealogy by removing nodes
  //! with times later than tnew
  //! NB: this destroys the genealogy inasmuch
  //! as the state is no longer correct.
  void truncate (slate_t tnew) {
    if (!empty()) {
      node_t *n = back();
      while (!empty() && n->slate > tnew) {
	ball_t *b = n->last_ball();
	switch (b->color) {
	case black:
	  drop(b);
	  break;
	case blue:
          b->color = black;
          swap(b,n->green_ball());
          destroy_node(n);
	  break;
	case green:
          err("in '%s': inconceivable!",__func__); // #nocov
	  break;
	}
        n = back();
      }
      time() = tnew;
    }
  };
  //! merge two genealogies:
  //! 1. the node-sequences are merged;
  //! 2. the root time retreats as necessary;
  //! 3. the current time advances as necessary;
  //! 4. the next unique-name counter advances as necessary.
  genealogy_t& operator+= (genealogy_t& G) {
    reinterpret_cast<nodeseq_t&>(*this) += reinterpret_cast<nodeseq_t&>(G);
    _t0 = (_t0 > G._t0) ? G._t0 : _t0;
    _time = (_time < G._time) ? G._time : _time;
    _unique = (_unique < G._unique) ? G._unique : _unique; 
    ndeme = (ndeme < G.ndeme) ? G.ndeme : ndeme;
    return *this;
  };
  
private:
  
  //! Scan the Newick-format label string.
  //! This has format %c_%d_%d:%f
  void scan_label (const std::string& s, color_t* col,
		    name_t *deme, slate_t *time) const {
    char c = s[0];
    switch (c) {
    case 'o':
      *col = black;
      break;
    case 'b':
      *col = blue;
      break;
    case 'g': case 'm':
      *col = green;
      break;
    default:
      err("in '%s': invalid color %s",__func__,c);
      break;
    }
    size_t k = 2, sz;
    *deme = name_t(stoi(s.substr(k),&sz));
    k += sz+1;			// +1 is for separator
    stoi(s.substr(k),&sz);	// ignore name
    k += sz+1;
    *time = slate_t(stod(s.substr(k)));
  };
  //! Scan the Newick string and put the ball
  //! into the indicated pocket, as appropriate.
  void scan_ball (const std::string& s, const slate_t t0, node_t *p) {
    color_t col;
    name_t deme;
    slate_t t;
    scan_label(s,&col,&deme,&t);
    t += t0;
    _time = (_time < t) ? t : _time;
    ndeme = (ndeme <= deme) ? deme+1 : ndeme;
    if (col != black) err("in '%s': bad Newick string (1)",__func__);
    if (p == 0) err("in '%s': bad Newick string (2)",__func__);
    ball_t *b = new ball_t(p,unique(),col,deme);
    p->insert(b);
  };
  //! Scan the Newick string and create the indicated node.
  node_t* scan_node (const std::string& s, const slate_t t0) {
    color_t col;
    name_t deme;
    slate_t t;
    name_t u = unique();
    scan_label(s,&col,&deme,&t);
    t += t0;
    ndeme = (ndeme <= deme) ? deme+1 : ndeme;
    _time = (_time < t) ? t : _time;
    node_t *p = new node_t(u,t,deme);
    ball_t *g = new ball_t(p,u,green,deme);
    p->green_ball() = g;
    p->insert(g);
    if (col==blue) {
      ball_t *b = new ball_t(p,p->uniq,blue,deme);
      p->insert(b);
    }
    push_back(p);
    return p;
  };
  //! Parse a single-root Newick tree.
  //! This assumes the string starts with a '('.
  std::string::const_iterator parse1 (std::string::const_iterator& i,
				      const std::string::const_iterator& e,
				      const slate_t t0, node_t **root) {
    size_t stack = 1;
    std::string::const_iterator j, k;
    i++; j = i;
    while (j != e && stack > 0) {
      switch (*j) {
      case '(':
	stack++;
	break;
      case ')':
	stack--;
	break;
      default:
	break;
      }
      j++;
    }
    if (stack > 0)
      err("in '%s': premature end of Newick string.",__func__);
    k = j;
    while (k != e && *k != ';' && *k != ',') k++;
    std::string a(i,j-1), b(j,k-1);
    node_t *p = scan_node(b,t0);
    parse(a,p->slate,p);
    *root = p;
    return k;
  };

public:
  
  //! Parse a Newick string and create the indicated genealogy.
  genealogy_t& parse (std::string& s, slate_t t0, node_t *p = 0) {
    std::string::const_iterator i = s.cbegin();
    while (i != s.cend()) {
      char c = *i;
      switch (c) {
      case '(':
	{
	  genealogy_t G(t0,unique());
	  node_t *q = 0;
	  i = G.parse1(i,s.cend(),t0,&q);
	  *this += G;
	  if (p != 0) {
	    ball_t *g = q->green_ball();
	    q->erase(g); p->insert(g); g->holder() = p;
	  }
	}
	break;
      case 'b':
	{
	  std::string::const_iterator j = i;
	  while (j != s.cend() && *j != ';' && *j != ',') j++;
	  std::string a(i,j);
	  node_t *q = scan_node(a,t0);
	  if (p != 0) {
	    ball_t *g = q->green_ball();
	    q->erase(g); p->insert(g); g->holder() = p;
	  }
	  i = j;
	}
	break;
      case 'o':
	{
	  std::string::const_iterator j = i;
	  while (j != s.cend() && *j != ';' && *j != ',') j++;
	  std::string a(i,j);
	  scan_ball(a,t0,p);
	  i = j;
	}
	break;
      default:
	i++;
	break;
      }
    }
    return *this;
  };
};

#endif
