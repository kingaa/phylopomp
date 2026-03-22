// -*- C++ -*-
// GENEALOGY class

#ifndef _GENEALOGY_H_
#define _GENEALOGY_H_

#include <regex>
#include <map>
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
  //! The number of demes.
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
    name_t A[3]; A[0] = magic; A[1] = G._unique; A[2] = name_t(G._ndeme);
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
    G._unique = A[1]; G._ndeme = size_t(A[2]);
    G.timezero() = B[0]; G.time() = B[1];
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
    int m, n, k;
    node_it i, j;
    for (k = 0, n = 0, i = begin(); i != end(); i++, n++) {
      node_t *p = *i;
      assert(!p->holds(black)); // tree should be pruned first
      tout[n] = p->slate;
      deme[n] = p->deme();
      if (p->is_root()) {
        type[n] = 0;            // root node
      } else if (p->holds(blue)) {
        type[n] = 1;            // sample node
        deme[n] = p->ball(blue)->deme();
      } else {
        type[n] = 2;            // internal node
      }
      lin[n] = p->lineage();    // 0-based indexing
      sat[n] = p->nchildren();
      index[n] = k;
      k += sat[n];
      child[n] = NA_INTEGER;
      if (p->is_root()) {
        anc[n] = n;             // 0-based indexing
      } else {
        for (m = 0, j = begin(); j != i; j++, m++) {
          node_t *q = *j;
          if (p->parent()->uniq == q->uniq) {
            anc[n] = m;
            break;
          }
        }
      }
    }
    tout[n] = time();
    for (k = 0, n = 0, i = begin(); i != end(); i++, n++) {
      node_t *p = *i;
      j = i; j++;
      for (m = n+1; j != end(); m++, j++) {
        node_t *q = *j;
        if (p->uniq == q->parent()->uniq) {
          child[k++] = m;
        }
      }
    }
  };
  //! genealogy information in list format
  SEXP gendat (void) const {
    SEXP t0, tout, anc, lin, sat, type, deme, index, child, ns, nr, nn;
    SEXP out, outn;
    size_t n = length();
    PROTECT(t0 = NEW_NUMERIC(1));
    PROTECT(tout = NEW_NUMERIC(n+1));
    PROTECT(type = NEW_INTEGER(n));
    PROTECT(deme = NEW_INTEGER(n));
    PROTECT(lin = NEW_INTEGER(n));
    PROTECT(sat = NEW_INTEGER(n));
    PROTECT(index = NEW_INTEGER(n));
    PROTECT(child = NEW_INTEGER(n));
    PROTECT(anc = NEW_INTEGER(n));
    PROTECT(ns = NEW_INTEGER(1));
    PROTECT(nr = NEW_INTEGER(1));
    PROTECT(nn = NEW_INTEGER(1));
    PROTECT(out = NEW_LIST(12));
    PROTECT(outn = NEW_CHARACTER(12));
    set_list_elem(out,outn,t0,"t0",0);
    set_list_elem(out,outn,tout,"nodetime",1);
    set_list_elem(out,outn,type,"nodetype",2);
    set_list_elem(out,outn,deme,"deme",3);
    set_list_elem(out,outn,lin,"lineage",4);
    set_list_elem(out,outn,sat,"saturation",5);
    set_list_elem(out,outn,index,"index",6);
    set_list_elem(out,outn,child,"child",7);
    set_list_elem(out,outn,anc,"ancestor",8);
    set_list_elem(out,outn,ns,"nsample",9);
    set_list_elem(out,outn,nr,"nroot",10);
    set_list_elem(out,outn,nn,"nnode",11);
    SET_NAMES(out,outn);
    gendat(REAL(tout),INTEGER(anc),INTEGER(lin),INTEGER(sat),
           INTEGER(type),INTEGER(deme),INTEGER(index),INTEGER(child));
    *REAL(t0) = double(timezero()); // zero-time
    *INTEGER(ns) = nsample();       // number of samples
    *INTEGER(nr) = nroot();         // number of roots
    *INTEGER(nn) = length();        // number of nodes
    UNPROTECT(14);
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

  //! number of roots
  size_t nroot (void) const {
    size_t n = 0;
    for (const node_t *p : *this) {
      if (p->holds_own()) n++;
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
  string_t describe (void) const {
    string_t o = "t0 = " + std::to_string(double(timezero()))
      + "\ntime = " + std::to_string(double(time())) + "\n"
      + nodeseq_t::describe();
    return o;
  };

public:

  //! machine-readable info
  virtual string_t yaml (string_t tab = "") const {
    string_t o;
    string_t t = tab + "  ";
    o = tab + "t0: " + std::to_string(timezero()) + "\n"
      + tab + "time: " + std::to_string(time()) + "\n"
      + tab + "nodes:\n" + nodeseq_t::yaml(tab);
    return o;
  };

public:

  //! put genealogy at current time into Newick format.
  string_t newick (void) const {
    return nodeseq_t::newick(time(),(ndeme() > 1));
  };

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

public:

  //! create a node holding its own green ball.
  //! insert into the genealogy.
  node_t* make_node (name_t d) {
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
    for (node_t *p : *this) {
      p->deme() = 0;
    }
    // drop superfluous nodes (holding just one ball).
    comb();
    _ndeme = 1;
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
          case green: case blue: // #nocov
            assert(0);           // #nocov
            break;               // #nocov
          }
        }
        b = p->last_ball();
        switch (b->color) {
        case blue:
          b->color = black;
        case black:
          b->deme() = p->deme();
          swap(b,p->green_ball());
        case green:
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
            move(b,p,q); push_back(q);
            break;
          case green:
            q = b->child();
            if (q == p) {
              b = p->first_ball();
              q = b->child();
            }
            if (q->slate < troot) {
              move(b,p,q);
            } else {
              node_t *pp = make_node(b->deme());
              pp->slate = troot;
              move(b,p,pp); push_back(pp);
            }
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
  //! merge two genealogies:
  //! 1. the node-sequences are merged;
  //! 2. the root time retreats as necessary;
  //! 3. the current time advances as necessary;
  //! 4. the unique-name stack advances as necessary.
  genealogy_t& operator+= (genealogy_t& G) {
    reinterpret_cast<nodeseq_t&>(*this) += reinterpret_cast<nodeseq_t&>(G);
    _t0 = (_t0 > G._t0) ? G._t0 : _t0;
    _time = (_time < G._time) ? G._time : _time;
    _unique = (_unique < G._unique) ? G._unique : _unique;
    _ndeme = (_ndeme < G.ndeme()) ? G.ndeme() : _ndeme;
    return *this;
  };

private:

  //! tips without descendants are reclassified as samples
  void repair_tips (void) {
    for (node_t *p : *this) {
      if (p->empty()) {
        ball_t *b = new ball_t(p,p->uniq,blue,p->deme());
        p->insert(b);
      }
    }
  };

  //! simple function for scanning a slate_t from a string
  //! (with error trapping)
  slate_t scan_slate (const string_t& s) const {
    double bl;
    try {
      bl = stod(s);
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
    if (bl < 0.0) err("in '%s': negative branch length detected.",__func__);
    return slate_t(bl);
  }
  //! simple function for scanning a name_t from a string
  //! (with error trapping)
  name_t scan_name (const string_t& s) const {
    int d;
    try {
      d = stoi(s);
      if (d < 0) err("in '%s': negative deme number detected.",__func__);
    }
    catch (const std::invalid_argument& e) {
      err("in '%s': invalid Newick format: deme should be indicated with an integer.",__func__);
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
    return name_t(d);
  }
  //! simple function for scanning the color
  color_t scan_color (const std::string& s) const {
    std::string copy(s);
    const std::map<string_t,color_t> options({
        {"sample",blue},{"extant",black},{"migration",green},
        {"node",green},{"branch",green},{"root",green}
      });
    color_t col = green;
    std::transform(copy.begin(),copy.end(),copy.begin(),
                   [](unsigned char c){return std::tolower(c);});
    try {
      col = options.at(copy);
    }
    catch (const std::out_of_range& e) {
      err("in %s: invalid metadata: type '%s' not recognized.",__func__,s.c_str());
    }
    return col;
  }
  //! Scan the label string.
  //! This has format [&&PhyloPOMP:deme=%d,type=%s]%s:%f
  node_t *scan_label (string_t::const_iterator b,
                      string_t::const_iterator e) {
    color_t col = green;
    name_t deme = 0;
    slate_t bl = 0;
    string_t s(b,e);
    const std::regex wre("^(.*?):([+-]?\\d*(?:\\.\\d+)?){1}.?$");
    std::smatch wm;
    if (std::regex_match(s,wm,wre)) {
      bl = scan_slate(wm[2].str());
      const string_t label = wm[1].str();
      //! FIXME: allow multiple metadata tags
      const std::regex mre("^.*?\\[&&PhyloPOMP:(.+?)\\].*$");
      std::smatch mm;
      if (std::regex_match(label,mm,mre)) {
        string_t meta = mm[1].str();
        const std::regex dre("deme=(\\w*)",std::regex_constants::icase);
        const std::regex tre("type=(\\w*)",std::regex_constants::icase);
        std::smatch sm;
        if (std::regex_search(meta,sm,dre)) {
          deme = scan_name(sm[1].str());
        }
        if (std::regex_search(meta,sm,tre)) {
          col = scan_color(sm[1].str());
        }
      }
    } else if (s.size() != 0) {
      warn("node label '%s' ignored: zero branch length assumed.",s.c_str());
    }
    node_t *q = make_node(deme);
    if (col == blue || col == black) {
      ball_t *b = new ball_t(q,q->uniq,col,deme);
      q->insert(b);
    }
    q->slate = bl;
    return q;
  };

public:

  //! Parse a Newick string and create the indicated genealogy.
  genealogy_t& parse (const string_t& s) {
    node_t *p = 0, *q;
    slate_t tf = timezero();
    string_t::const_reverse_iterator pos1 = s.crbegin(), pos2 = pos1;
    int stack = 0;
    while (pos1 != s.crend()) {
      switch (*pos1) {
      case ']':
        while (pos1 != s.crend() && *pos1 != '[') pos1++; // skip metadata
        if (pos1 != s.crend()) pos1++;
        break;
      case ';':
        p = 0;
        pos1++;
        pos2 = pos1;
        break;
      case ')': case '(': case ',':
        q = scan_label(pos1.base(),pos2.base());
        ndeme() = (ndeme() > q->deme()+1) ? ndeme() : q->deme()+1;
        if (p != 0) {
          q->slate += p->slate;
          tf = (q->slate > tf) ? q->slate : tf;
          if (q->holds(black)) {
            move(q->last_ball(),q,p);
            destroy_node(q);
            q = p;
          } else {
            attach(p,q);
            push_back(q);
          }
        } else {
          q->slate += timezero();
          tf = (q->slate > tf) ? q->slate : tf;
          if (q->slate > timezero()) {
            p = make_node(q->deme());
            push_front(p); attach(p,q);
          }
          p = q;
          push_back(q);
        }
        switch (*pos1) {
        case ')':
          p = q;
          pos1++;
          stack++;
          break;
        case '(':
          while (pos1 != s.crend() && *pos1 == '(') {
            p = p->parent();
            pos1++;
            stack--;
          }
          if (pos1 != s.crend() && *pos1 == ',') pos1++;
          break;
        default:
          pos1++;
          break;
        }
        pos2 = pos1;
        break;
      default:
        pos1++;
        break;
      }
    }
    if (stack != 0)
      err("in '%s': invalid Newick format: unbalanced parentheses.",__func__);
    time() = tf;
    sort(); repair_tips(); drop_zlb();
    return *this;
  };
};

#endif
