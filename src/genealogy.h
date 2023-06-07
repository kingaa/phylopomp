// -*- C++ -*-
// GENEALOGY class

#ifndef _GENEALOGY_H_
#define _GENEALOGY_H_

#include <string>
#include <cstring>
#include <utility>
#include <stdexcept>

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
  
  //! The next unique name.
  name_t _unique;               
  //! The initial time.
  slate_t _t0;
  //! The current time.
  slate_t _time;

public:
  
  //! The number of demes.
  size_t _ndeme;
  
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

  //! set the unique name counter
  //  void set_unique (name_t u) {
  //    if (u < _unique) warn("potential re-use of names!");
  //    _unique = u;
  //  };
  //! number of demes
  size_t ndeme (void) const {
    return _ndeme;
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
    name_t A[2]; A[0] = G._unique; A[1] = name_t(G._ndeme);
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
    G._unique = A[0]; G._ndeme = size_t(A[1]);
    G._t0 = B[0]; G._time = B[1];
    return o >> reinterpret_cast<nodeseq_t&>(G);
  };

public:
  // CONSTRUCTORS
  //! basic constructor for genealogy class
  //!  t0 = initial time
  genealogy_t (double t0 = 0, name_t u = 0, size_t nd = 1) {
    clean();
    _ndeme = nd;
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
  
  //! lineage count, saturation, and event-type.
  //! types are:
  //! -  0 = non-event
  //! - -1 = root
  //! -  1 = sample
  //! -  2 = non-sample node
  //! -  3 = end of interval
  void lineage_count (double *t, int *deme,
                      int *ell, int *sat, int *etype) const {
    slate_t tcur = *t = timezero();
    for (size_t j = 0; j < _ndeme; j++) {
      deme[j] = j+1;
      sat[j] = ell[j] = 0;
      etype[j] = 0;
    }
    for (node_it i = begin(); i != end(); i++) {
      node_t *p = *i;
      if (tcur < p->slate) {
        t++;
        ell += _ndeme; sat += _ndeme; deme += _ndeme; etype += _ndeme;
        *t = tcur = p->slate;
        for (size_t j = 0; j < _ndeme; j++) {
          deme[j] = j+1;
          ell[j] = (ell-_ndeme)[j];
          sat[j] = 0;
          etype[j] = 0;
        }
      }
      p->lineage_incr(ell,sat,etype);
    }
    t++;
    ell += _ndeme; sat += _ndeme; deme += _ndeme; etype += _ndeme;
    *t = time();
    for (size_t j = 0; j < _ndeme; j++) {
      sat[j] = ell[j] = 0;
      deme[j] = j+1;
      etype[j] = 3;
    }
  };
  //! lineage count and saturation
  SEXP lineage_count (void) const {
    SEXP t, deme, ell, sat, etype, out, outn;
    int nt = ntime(timezero())+1;
    int nl = _ndeme*nt;
    PROTECT(t = NEW_NUMERIC(nt));
    PROTECT(deme = NEW_INTEGER(nl));
    PROTECT(ell = NEW_INTEGER(nl));
    PROTECT(sat = NEW_INTEGER(nl));
    PROTECT(etype = NEW_INTEGER(nl));
    PROTECT(out = NEW_LIST(5));
    PROTECT(outn = NEW_CHARACTER(5));
    set_list_elem(out,outn,t,"time",0);
    set_list_elem(out,outn,deme,"deme",1);
    set_list_elem(out,outn,ell,"lineages",2);
    set_list_elem(out,outn,sat,"saturation",3);
    set_list_elem(out,outn,etype,"event_type",4);
    SET_NAMES(out,outn);
    lineage_count(REAL(t),INTEGER(deme),INTEGER(ell),
                  INTEGER(sat),INTEGER(etype));
    UNPROTECT(7);
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
      err("maximum genealogy size exceeded!"); // #nocov
    } else if (size() > maxq) {
      ok = false;		// #nocov
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
    _ndeme = 1;
    return *this;
  };
  //! curtail the genealogy by removing nodes
  //! with times later than tnew
  void curtail (slate_t tnew) {
    if (!empty()) {
      node_t *p = back();
      while (!empty() && p->slate > tnew) {
        ball_t *b;
        while (p->size() > 1) {
          b = p->last_ball();
          switch (b->color) {
          case blue: case black:
            p->erase(b); delete b;
            break;
          case green:                                // #nocov
            err("in '%s': inconceivable!",__func__); // #nocov
            break;                                   // #nocov
          }
        }
        b = p->last_ball();
        switch (b->color) {
        case blue:
          b->color = black;
        case black:
          b->deme() = p->deme;
          swap(b,p->green_ball());
        case green:
          destroy_node(p);
          break;
        }
        if (!empty()) p = back();
      }
    }
    if (tnew < time()) _time = tnew;
    if (tnew < timezero()) _t0 = tnew;
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
    _ndeme = (_ndeme < G._ndeme) ? G._ndeme : _ndeme;
    return *this;
  };
  
private:
  
  //! Scan the Newick-format label string.
  //! This has format %c_%d_%d:%f
  size_t scan_label (const std::string& s, color_t* col,
                     name_t *deme, slate_t *time) const {
    size_t n = s.size();
    if (n < 5) 
      err("in '%s': invalid Newick format: empty or invalid label.",__func__);
    switch (s[0]) {
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
      err("in '%s': invalid Newick format: invalid label.",__func__);
      break;
    }
    size_t i = 1;
    size_t sz;
    while (i < n && s[i] == '_') i++;
    if (i == n)
      err("in '%s': invalid Newick format: no deme specified.",__func__);
    if (s[i] == '(' || s[i] == ')' || s[i] == ',' || s[i] == ';')
      err("in '%s': invalid Newick format: invalid deme.",__func__);
    try {
      *deme = name_t(stoi(s.substr(i),&sz));
      i += sz;
    }
    catch (const std::invalid_argument& e) {
      err("in '%s': invalid Newick format: invalid deme.",__func__);
    }
    catch (const std::out_of_range& e) {
      err("in '%s': invalid Newick format: deme out of range.",__func__);
    }
    // skip to branch length
    while (i < n && s[i] != ':' &&
           s[i] != '(' && s[i] != ')' && s[i] != ',' && s[i] != ';') i++;
    if (i == n || s[i] != ':')
      err("in '%s': invalid Newick format: missing or invalid branch length.",__func__);
    i++;
    try {
      *time = slate_t(stod(s.substr(i),&sz));
    }
    catch (const std::invalid_argument& e) {
      err("in '%s': invalid Newick format: invalid branch length.",__func__);
    }
    catch (const std::out_of_range& e) {
      err("in '%s': invalid Newick format: branch length out of range.",__func__);
    }
    i += sz;
    return i;
  };
  //! Scan the Newick string and put the ball
  //! into the indicated pocket, as appropriate.
  size_t scan_ball (const std::string& s, const slate_t t0, node_t *p) {
    color_t col;
    name_t deme;
    slate_t t;
    size_t i = scan_label(s,&col,&deme,&t);
    t += t0;
    _time = (_time < t) ? t : _time;
    _ndeme = (_ndeme <= deme) ? deme+1 : _ndeme;
    if (col != black) err("in '%s': bad Newick string (1)",__func__);
    if (p == 0) err("in '%s': bad Newick string (2)",__func__);
    ball_t *b = new ball_t(p,unique(),col,deme);
    p->insert(b);
    return i;
  };
  //! Scan the Newick string and create the indicated node.
  size_t scan_node (const std::string& s, const slate_t t0, node_t **q) {
    size_t i;
    color_t col;
    name_t deme;
    slate_t t;
    name_t u = unique();
    i = scan_label(s,&col,&deme,&t);
    t += t0;
    _ndeme = (_ndeme <= deme) ? deme+1 : _ndeme;
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
    *q = p;
    return i;
  };
  //! Parse a single-root Newick tree.
  //! This assumes the string starts with a '('.
  size_t scan_tree (const std::string& s,
                    const slate_t t0, node_t **root) {
    size_t n = s.size();
    size_t i = 1, j = 1, k;
    size_t stack = 1;
    while (j < n && stack > 0) {
      switch (s[j]) {
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
    node_t *p = 0;
    k = j;
    k += scan_node(s.substr(j),t0,&p);
    parse(s.substr(i,j-i-1),p->slate,p);
    *root = p;
    return k;
  };

public:
  
  //! Parse a Newick string and create the indicated genealogy.
  genealogy_t& parse (const std::string& s, slate_t t0, node_t *p = 0) {
    size_t i = 0;
    size_t n = s.size();
    while (i < n) {
      switch (s[i]) {
      case '(':
        {
          genealogy_t G(t0,unique());
          node_t *q = 0;
          i += G.scan_tree(s.substr(i),t0,&q);
          *this += G;
          if (p != 0) {
            ball_t *g = q->green_ball();
            q->erase(g); p->insert(g); g->holder() = p;
          }
        }
        break;
      case 'b':
        {
          node_t *q = 0;
          i += scan_node(s.substr(i),t0,&q);
          if (p != 0) {
            ball_t *g = q->green_ball();
            q->erase(g); p->insert(g); g->holder() = p;
          }
        }
        break;
      case 'o':
        i += scan_ball(s.substr(i),t0,p);
        break;
      case ',': case ';': case ' ': case '\n': case '\t':
        i++;
        break;
      case ')':
        err("in '%s': invalid Newick string: unbalanced parentheses.",__func__);
        break;
      default:
        err("in '%s': invalid Newick string.",__func__);
        break;
      }
    }
    return *this;
  };
};

#endif
