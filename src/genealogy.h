// -*- C++ -*-
// GENEALOGY class

#ifndef _GENEALOGY_H_
#define _GENEALOGY_H_

#include <string>
#include <cstring>
#include <utility>
#include <stdexcept>

#include "nodeseq.h"
#include "inventory.h"
#include "internal.h"

static const size_t MEMORY_MAX = (1<<28); // 256MB

//! Encodes a genealogy.

//! A genealogy consists of a sequence of nodes
//! and the current time.
class genealogy_t : public nodeseq_t {

private:

  name_t _unique;
  slate_t _t0;
  slate_t _time;
  size_t _ndeme;
  bool _obscured;
  bool _hided;

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
    _t0 = _time = R_NaReal;
    _obscured = false;
    _hided = false;
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
  //! update unique counter
  void update_uniq (void) {
    _unique++;
  };

public:

  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    return 5*sizeof(name_t) +
      2*sizeof(slate_t) + nodeseq_t::bytesize();
  };
  //! binary serialization
  friend raw_t* operator>> (const genealogy_t& G, raw_t* o) {
    name_t A[5];
    A[0] = magic; A[1] = G._unique; A[2] = name_t(G._ndeme);
    A[3] = name_t(G._obscured); A[4] = name_t(G._hided);
    slate_t B[2]; B[0] = G.timezero(); B[1] = G.time();
    memcpy(o,A,sizeof(A)); o += sizeof(A);
    memcpy(o,B,sizeof(B)); o += sizeof(B);
    return reinterpret_cast<const nodeseq_t&>(G) >> o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, genealogy_t& G) {
    G.clean();
    name_t A[5];
    slate_t B[2];
    memcpy(A,o,sizeof(A)); o += sizeof(A);
    memcpy(B,o,sizeof(B)); o += sizeof(B);
    if (A[0] != magic)
      err("in %s (%s line %d) corrupted genealogy serialization.",
          __func__,__FILE__,__LINE__);
    G._unique = A[1]; G._ndeme = size_t(A[2]);
    G._obscured = bool(A[3]); G._hided = bool(A[4]);
    G.timezero() = B[0]; G.time() = B[1];
    return o >> reinterpret_cast<nodeseq_t&>(G);
  };

public:
  // CONSTRUCTORS
  //! basic constructor for genealogy class
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
  //! constructor from RAW SEXP (containing binary serialization)
  genealogy_t (SEXP o) {
    if (LENGTH(o)==0)
      err("in %s (%s line %d): cannot deserialize a NULL.",
          __func__,__FILE__,__LINE__);
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
  //! is obscured?
  bool obscured (void) const {
    return _obscured;
  };
  //! is hided?
  bool hided (void) const {
    return _hided;
  };

public:

  //! lineage count, saturation, and event-type.
  void lineage_count (double *tout, int *deme,
                      int *ell, int *sat, int *etype) const {
    slate_t tcur = timezero();
    for (size_t j = 0; j < _ndeme; j++) {
      tout[j] = tcur;
      deme[j] = j+1;
      sat[j] = ell[j] = 0;
      etype[j] = 0;
    }
    for (const node_t *p : *this) {
      if (tcur < p->slate) {
        tout += _ndeme; ell += _ndeme; sat += _ndeme;
        deme += _ndeme; etype += _ndeme;
        tcur = p->slate;
        for (size_t j = 0; j < _ndeme; j++) {
          tout[j] = tcur;
          deme[j] = j+1;
          ell[j] = (ell-_ndeme)[j];
          sat[j] = 0;
          etype[j] = 0;
        }
      }
      p->lineage_incr(ell,sat,etype);
    }
    tout += _ndeme; ell += _ndeme; sat += _ndeme;
    deme += _ndeme; etype += _ndeme;
    tcur = time();
    for (size_t j = 0; j < _ndeme; j++) {
      tout[j] = tcur;
      sat[j] = ell[j] = 0;
      deme[j] = j+1;
      etype[j] = 3;
    }
  };
  //! lineage count and saturation
  SEXP lineage_count (void) const {
    SEXP tout, deme, ell, sat, etype, out, outn;
    int nt = ntime(timezero())+1;
    int nl = _ndeme*nt;
    PROTECT(tout = NEW_NUMERIC(nl));
    PROTECT(deme = NEW_INTEGER(nl));
    PROTECT(ell = NEW_INTEGER(nl));
    PROTECT(sat = NEW_INTEGER(nl));
    PROTECT(etype = NEW_INTEGER(nl));
    PROTECT(out = NEW_LIST(5));
    PROTECT(outn = NEW_CHARACTER(5));
    set_list_elem(out,outn,tout,"time",0);
    set_list_elem(out,outn,deme,"deme",1);
    set_list_elem(out,outn,ell,"lineages",2);
    set_list_elem(out,outn,sat,"saturation",3);
    set_list_elem(out,outn,etype,"event_type",4);
    SET_NAMES(out,outn);
    lineage_count(REAL(tout),INTEGER(deme),INTEGER(ell),
                  INTEGER(sat),INTEGER(etype));
    UNPROTECT(7);
    return out;
  };
  //! genealogy information in list format
  void gendat (double *tout, int *anc, int *lin,
               int *sat, int *type, int *deme,
               int *index, int *child) const {
    int n, k;
    node_it i;
    // build uniq->index map for O(1) parent lookup
    std::unordered_map<name_t,int> uniq_to_idx;
    uniq_to_idx.reserve(length());
    for (n = 0, i = begin(); i != end(); i++, n++) {
      uniq_to_idx[(*i)->uniq] = n;
    }
    for (k = 0, n = 0, i = begin(); i != end(); i++, n++) {
      node_t *p = *i;
      assert(!p->holds(black));
      tout[n] = p->slate;
      deme[n] = p->deme();
      if (p->is_root()) {
        type[n] = 0;
      } else if (p->holds(blue)) {
        type[n] = 1;
        deme[n] = p->ball(blue)->deme();
      } else {
        type[n] = 2;
      }
      lin[n] = p->lineage();
      sat[n] = p->nchildren();
      index[n] = k;
      k += sat[n];
      child[n] = NA_INTEGER;
      if (p->is_root()) {
        anc[n] = n;
      } else {
        auto it = uniq_to_idx.find(p->parent()->uniq);
        anc[n] = (it != uniq_to_idx.end()) ? it->second : n;
      }
    }
    tout[n] = time();
    for (k = 0, n = 0, i = begin(); i != end(); i++, n++) {
      node_t *p = *i;
      node_it j = i; j++;
      for (int m = n+1; j != end(); m++, j++) {
        node_t *q = *j;
        if (p->uniq == q->parent()->uniq) {
          child[k++] = m;
        }
      }
    }
  };
  //! genealogy information in list format
  SEXP gendat (void) const {
    SEXP tout, anc, lin, sat, type, deme, index, child, ns, nn;
    SEXP out, outn;
    size_t n = length();
    PROTECT(tout = NEW_NUMERIC(n+1));
    PROTECT(type = NEW_INTEGER(n));
    PROTECT(deme = NEW_INTEGER(n));
    PROTECT(lin = NEW_INTEGER(n));
    PROTECT(sat = NEW_INTEGER(n));
    PROTECT(index = NEW_INTEGER(n));
    PROTECT(child = NEW_INTEGER(n));
    PROTECT(anc = NEW_INTEGER(n));
    PROTECT(ns = NEW_INTEGER(1));
    PROTECT(nn = NEW_INTEGER(1));
    PROTECT(out = NEW_LIST(10));
    PROTECT(outn = NEW_CHARACTER(10));
    set_list_elem(out,outn,tout,"nodetime",0);
    set_list_elem(out,outn,type,"nodetype",1);
    set_list_elem(out,outn,deme,"deme",2);
    set_list_elem(out,outn,lin,"lineage",3);
    set_list_elem(out,outn,sat,"saturation",4);
    set_list_elem(out,outn,index,"index",5);
    set_list_elem(out,outn,child,"child",6);
    set_list_elem(out,outn,anc,"ancestor",7);
    set_list_elem(out,outn,ns,"nsample",8);
    set_list_elem(out,outn,nn,"nnode",9);
    SET_NAMES(out,outn);
    gendat(REAL(tout),INTEGER(anc),INTEGER(lin),INTEGER(sat),
           INTEGER(type),INTEGER(deme),INTEGER(index),INTEGER(child));
    *INTEGER(ns) = nsample();
    *INTEGER(nn) = length();
    UNPROTECT(12);
    return out;
  };

  //! number of samples
  size_t nsample (void) const {
    size_t n = 0;
    for (const node_t *p : *this) {
      if (p->holds(blue)) n++;
    }
    return n;
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
    std::string o = "t0 = " + std::to_string(double(timezero()))
      + "\ntime = " + std::to_string(double(time())) + "\n"
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
  //! put genealogy into compact Newick format (with reassortment markers).
  std::string compact_newick_str (void) const {
    return nodeseq_t::compact_newick(time());
  };

public:

  //! check the validity of the genealogy.
  void valid (void) const {};
  //! check the size of the genealogy.
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

  //! create a node holding its own green ball only.
  node_t* make_node (name_t d) {
    check_genealogy_size(0);
    name_t u = unique();
    node_t *p = new node_t(u,_time);
    ball_t *g = new ball_t(p,u,green,d);
    p->green_ball() = g;
    p->insert(g);
    return p;
  };
  //! create a node holding its own green ball and a ball of given color.
  node_t* make_node (color_t col, name_t d) {
    check_genealogy_size(0);
    name_t u = unique();
    node_t *p = new node_t(u,_time);
    ball_t *g = new ball_t(p,u,green,d);
    ball_t *b = new ball_t(p,u,col,d);
    p->green_ball() = g;
    p->insert(g);
    p->insert(b);
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
  //! reassort: ball a gets reassorted with ball b
  void reassort (ball_t *a, ball_t *b, slate_t t) {
    assert(a->is(black));
    assert(b->is(black));
    time() = t;
    node_t *p = a->holder();
    // create a new node with a black ball
    node_t *q = make_node(black, p->deme());
    ball_t *d = q->other(q->green_ball());
    swap(a,d);
    add(q,b);
    q->slate = t;
    // mark the remaining branch with a darkorange ball
    if (p->size() > 2) {
      err("Non-binary tree!");
    } else {
      drop(d);
      node_t *x = make_node(darkorange, a->deme());
      add(x,a);
      x->slate = t;
    }
  };
  //! indicate a reassortment event in un-reassorted segment trees
  void reassort_notice (ball_t *a, slate_t t) {
    assert(a->is(black));
    time() = t;
    node_t *x = make_node(lightorange, a->deme());
    x->slate = t;
    add(x,a);
  };
  //! set up for extraction of black balls
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
    // erase deme information from black balls
    pocket_t *blacks = colored(black);
    while (!blacks->empty()) {
      ball_t *a = *(blacks->begin());
      a->deme() = 0;
      blacks->erase(a);
    }
    delete blacks;
    // erase deme information from nodes
    for (node_t *p : *this) {
      p->deme() = 0;
    }
    // drop superfluous nodes
    comb();
    _ndeme = 1;
    _obscured = true;
    return *this;
  };
  //! drop all orange balls (hide reassortment markers)
  genealogy_t& hide (void) {
    pocket_t *lightoranges = colored(lightorange);
    while (!lightoranges->empty()) {
      ball_t *a = *(lightoranges->begin());
      node_t *p = a->holder();
      ball_t *b = p->other(a);
      swap(b,p->green_ball());
      destroy_node(p);
      lightoranges->erase(a);
    }
    delete lightoranges;
    pocket_t *darkoranges = colored(darkorange);
    while (!darkoranges->empty()) {
      ball_t *a = *(darkoranges->begin());
      node_t *p = a->holder();
      ball_t *b = p->other(a);
      swap(b,p->green_ball());
      destroy_node(p);
      darkoranges->erase(a);
    }
    delete darkoranges;
    _hided = true;
    return *this;
  };
  //! curtail the genealogy by removing nodes
  //! with times later than tnew and/or earlier than troot
  void curtail (slate_t tnew, slate_t troot) {
    if (tnew < troot) troot = tnew;
    if (!empty() && tnew < time()) {
      node_t *p = back();
      while (!empty() && p->slate > tnew) {
        ball_t *b;
        while (p->size() > 1) {
          b = p->last_ball();
          switch (b->color) {
          case black:
            p->erase(b); delete b;
            break;
          case green: case blue:
            assert(0);
            break;
          default:
            p->erase(b); delete b;
            break;
          }
        }
        b = p->last_ball();
        switch (b->color) {
        case blue:
          b->color = black;
          // fallthrough
        case black:
          b->deme() = p->deme();
          swap(b,p->green_ball());
          // fallthrough
        case green:
          destroy_node(p);
          break;
        default:
          destroy_node(p);
          break;
        }
        if (!empty()) p = back();
      }
    }
    time() = tnew;
    if (!empty() && troot > timezero()) {
      node_t *p = front();
      node_t *q;
      while (!empty() && p->slate < troot) {
        ball_t *b;
        assert(p->holds_own());
        while (p->size() > 1) {
          b = p->last_ball();
          switch (b->color) {
          case blue:
            p->erase(b); delete b;
            break;
          case black:
            q = make_node(b->deme());
            q->slate = troot;
            q->insert(b); p->erase(b);
            b->holder() = q;
            push_back(q);
            break;
          case green:
            q = b->child();
            if (q == p) {
              b = p->first_ball();
              q = b->child();
            }
            if (q->slate < troot) {
              q->insert(b); p->erase(b);
              b->holder() = q;
            } else {
              node_t *pp = make_node(b->deme());
              pp->slate = troot;
              pp->insert(b); p->erase(b);
              b->holder() = pp;
              push_back(pp);
            }
            break;
          default:
            p->erase(b); delete b;
            break;
          }
        }
        destroy_node(p);
        if (!empty()) p = front();
      }
      sort();
    }
    if (troot > timezero()) timezero() = troot;
  };
  //! merge two genealogies
  genealogy_t& operator+= (genealogy_t& G) {
    reinterpret_cast<nodeseq_t&>(*this) += reinterpret_cast<nodeseq_t&>(G);
    _t0 = (_t0 > G._t0) ? G._t0 : _t0;
    _time = (_time < G._time) ? G._time : _time;
    _unique = (_unique < G._unique) ? G._unique : _unique;
    _ndeme = (_ndeme < G.ndeme()) ? G.ndeme() : _ndeme;
    return *this;
  };

private:

  size_t scan_color (const std::string& s, color_t* col) const {
    return 1;
  };
  //! Scan the Newick-format label string.
  size_t scan_label (const std::string& s, color_t* col,
                     name_t *deme, slate_t *time) const {
    size_t n = s.size();
    if (n < 1)
      err("in '%s' (%s line %d): invalid Newick format: empty label.",__func__,__FILE__,__LINE__);
    size_t sz, i = 1;
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
      err("in '%s' (%s line %d): invalid Newick label: expected one of 'b','g','m', or 'o', got '%c'.",
          __func__,__FILE__,__LINE__,s[0]);
      break;
    }
    while (i < n && s[i] == '_') i++;
    if (i == n)
      err("in '%s': invalid Newick format: premature termination.",__func__);
    if (s[i] == '(' || s[i] == ')' || s[i] == ',' || s[i] == ';')
      err("in '%s' (%s line %d): invalid Newick format.",__func__,__FILE__,__LINE__);
    if (s[i] == ':') {
      *deme = 0;
    } else {
      try {
        *deme = name_t(stoi(s.substr(i),&sz));
        i += sz;
      }
      catch (const std::invalid_argument& e) {
        err("in '%s' (%s line %d): invalid Newick format: deme should be indicated with an integer.",
            __func__,__FILE__,__LINE__);
      }
      catch (const std::out_of_range& e) {
        err("in '%s': invalid Newick format: deme out of range.",__func__);
      }
      catch (const std::exception& e) {
        err("in '%s': parsing deme label: %s.",__func__,e.what());
      }
      catch (...) {
        err("in '%s': other deme-parsing error.",__func__);
      }
    }
    while (i < n && s[i] != ':' &&
           s[i] != '(' && s[i] != ')' && s[i] != ',' && s[i] != ';') i++;
    if (i == n || s[i] != ':')
      err("in '%s': invalid Newick format: missing or invalid branch length.",__func__);
    i++;
    try {
      *time = slate_t(stod(s.substr(i),&sz));
    }
    catch (const std::invalid_argument& e) {
      err("in '%s': invalid Newick format: branch length should be a non-negative decimal number.",__func__);
    }
    catch (const std::out_of_range& e) {
      err("in '%s': invalid Newick format: branch length out of range.",__func__);
    }
    catch (const std::exception& e) {
      err("in '%s': parsing branch-length: %s.",__func__,e.what());
    }
    catch (...) {
      err("in '%s': other branch-length parsing error.",__func__);
    }
    if (*time < 0.0) err("in '%s': negative branch length detected.",__func__);
    i += sz;
    return i;
  };
  //! Scan the Newick string and put the ball
  //! into the indicated pocket, as appropriate.
  size_t scan_ball (const std::string& s, const slate_t t0, node_t *p) {
    color_t col;
    name_t deme;
    slate_t t;
    ball_t *b;
    size_t i = scan_label(s,&col,&deme,&t);
    t += t0;
    _time = (_time < t) ? t : _time;
    _ndeme = (_ndeme <= deme) ? deme+1 : _ndeme;
    if (col != black) err("in '%s': bad Newick string (1)",__func__);
    if (p == 0) err("in '%s': bad Newick string (2)",__func__);
    b = new ball_t(p,unique(),col,deme);
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
    node_t *p = new node_t(u,t);
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
    sort();
    return *this;
  };
};

#endif
