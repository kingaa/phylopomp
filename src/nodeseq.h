// -*- C++ -*-
// NODE SEQUENCE CLASS

#ifndef _NODESEQ_H_
#define _NODESEQ_H_

#include <list>
#include <unordered_map>
#include "node.h"
#include "internal.h"

typedef typename std::list<node_t*>::const_iterator node_it;
typedef typename std::list<node_t*>::iterator node_nit;
typedef typename std::list<node_t*>::const_reverse_iterator node_rev_it;

//! A sequence of nodes.
class nodeseq_t : public std::list<node_t*> {

private:

  //! clean up: delete all nodes, reset globals
  void clean (void) {
    for (node_t *p : *this) delete p;
    clear();
  };

public:

  //! destructor
  ~nodeseq_t (void) {
    clean();
  };

public:

  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    size_t s = sizeof(size_t);
    for (node_t *p : *this)
      s += p->bytesize();
    return s;
  };
  //! binary serialization
  friend raw_t* operator>> (const nodeseq_t& G, raw_t* o) {
    size_t nnode = G.size();
    memcpy(o,&nnode,sizeof(size_t)); o += sizeof(size_t);
    for (node_t *p : G) {
      o = (*p >> o);
    }
    return o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, nodeseq_t& G) {
    G.clean();
    std::unordered_map<name_t,node_t*> node_names;
    std::unordered_map<name_t,ball_t*> ball_names;
    size_t nnode = 0;
    memcpy(&nnode,o,sizeof(size_t)); o += sizeof(size_t);
    node_names.reserve(nnode);
    ball_names.reserve(nnode);
    for (size_t i = 0; i < nnode; i++) {
      node_t *p = new node_t();
      o = (o >> *p);
      G.push_back(p);
      node_names.insert({p->uniq,p});
    }
    for (node_t *q : G) {
      q->repair_owners(node_names,&ball_names);
    }
    G.repair_owners(ball_names);
    G.trace_lineages();
    return o;
  };

private:

  //! Needed in deserialization.
  //! This function repairs the links green balls and their names.
  void repair_owners (std::unordered_map<name_t,ball_t*>& names) {
    std::unordered_map<name_t,ball_t*>::const_iterator n;
    for (node_t *p : *this) {
      n = names.find(p->uniq);
      assert(n != names.end());
      ball_t *b = n->second;
      p->green_ball() = b;
    }
  };

public:

  //! Order relation among nodes.
  //! An ancestor node should always come before its descendants.
  //! Nodes should be ordered by time, then arbitrarily.
  static bool compare (node_t* p, node_t* q) {
    return (p->slate < q->slate) ||
      ((p->slate == q->slate) &&
       ((p==q->green_ball()->holder()) ||
        ((q!=p->green_ball()->holder()) && (p->uniq < q->uniq))));
  };

  //! order nodes in order of increasing time
  void sort (void) {
    std::list<node_t*>::sort(compare);
  };

private:

  //! Earliest time in the sequence.
  slate_t dawn (void) const {
    return (empty()) ? R_NaReal : front()->slate;
  };
  //! Latest time in the sequence.
  slate_t dusk (void) const {
    return (empty()) ? R_NaReal : back()->slate;
  }

public:

  //! Get all balls of a color.
  pocket_t* colored (color_t col) const {
    pocket_t *p = new pocket_t;
    for (node_t *q : *this) {
      for (ball_t *b : *q ) {
        if (b->is(col)) p->insert(b);
      }
    }
    return p;
  };
  //! Number of distinct timepoints.
  size_t ntime (slate_t t) const {
    size_t count = 1;
    for (node_t *p : *this) {
      if (t < p->slate) {
        t = p->slate;
        count++;
      }
    }
    return count;
  };
  //! Number of nodes in the sequence.
  size_t length (void) const {
    return this->size();
  };
  //! traverse to nth node, retrieve pointer
  node_t *position (int n) {
    int i = 0;
    node_it k = cbegin();
    while (i < n && k != cend()) {
      i++; k++;
    }
    assert(k != cend());
    return *k;
  };

public:

  //! move ball b from p to q
  void move (ball_t *b, node_t *p, node_t *q) {
    assert(b->holder() == p);
    p->erase(b); q->insert(b);
  };
  //! swap balls a and b, wherever they lie
  void swap (ball_t *a, ball_t *b) {
    node_t *p = a->holder();
    node_t *q = b->holder();
    if (p != q) {
      p->erase(a); q->insert(a);
      q->erase(b); p->insert(b);
    }
  };
  //! attach node q as descendant of node p.
  //! note that this does not push q into the nodeseq.
  void attach (node_t *p, node_t *q) {
    move(q->green_ball(),q,p);
  };
  //! detach node p from its parent.
  //! note that this does not remove q from the nodeseq.
  void detach (node_t *p) {
    move(p->green_ball(),p->parent(),p);
  };
  //! add node p; take as parent the node holding ball a.
  //! the deme of p is changed to match that of a
  void add (node_t *p, ball_t *a) {
    swap(a,p->green_ball());
    p->deme() = a->deme();
    push_back(p);
  };
  //! drop the black ball 'a' and the node if either
  //! (1) the node becomes thereby a dead root, or
  //! (2) the node's pocket becomes thereby empty.
  void drop (ball_t *a) {
    assert(a->is(black));
    node_t *p = a->holder();
    if (p->size() > 1) {
      p->erase(a);
      delete a;
      if (p->dead_root()) {     // remove isolated root
        destroy_node(p);
      }
    } else {
      swap(a,p->green_ball());
      destroy_node(p);
      drop(a);                  // recurse
    }
  };
  //! remove a dead root node
  void destroy_node (node_t *p) {
    assert(p->dead_root());
    remove(p);
    delete p;
  };
  //! drop all dead roots
  void weed (void) {
    node_nit j = begin();
    while (j != end()) {
      if ((*j)->dead_root()) {
        destroy_node(*(j++));
      } else {
        j++;
      }
    }
  };
  //! drop all inline nodes
  //! i.e., those holding just one ball that is green.
  void comb (void) {
    for (node_t *p : *this) {
      if (p->size() == 1 && p->holds(green)) {
        swap(p->last_ball(),p->green_ball());
      }
    }
    weed();
  };
  //! drop all zero-length branches
  void drop_zlb (void) {
    for (node_t *p : *this) {
      if (!p->holds_own() && p->slate == p->parent()->slate) {
        while (!p->empty()) {
          ball_t *b = p->last_ball();
          p->erase(b); p->parent()->insert(b);
          //FIXME: do we also need to change ball-demes?
        }
        detach(p);
      }
    }
    weed();
  };

private:

  //! trace back a single lineage.
  //! this results in the deme slot for all green balls along
  //! the lineage of 'b' begin replaced by the lineage of 'b'.
  void trace_lineage (ball_t *b, name_t u) {
    node_t *p = b->holder();
    while (p->lineage() == null_lineage) {
      p->lineage() = u;
      p = p->parent();
    }
  };

public:

  //! trace back all sample lineages.
  //! this results in the deme slots of all green balls being
  //! replaced by the unique names of the lineages they trace.
  void trace_lineages (void) {
    // we trace each lineage in turn.
    // because we move from early to late,
    // the order is guaranteed to be valid.
    name_t u = 0;
    for (node_t *p : *this ) {
      for (ball_t *b : *p) {
        if (b->color==blue) {
          trace_lineage(b,u);
          u++;
        }
      }
    }
  };

public:

  //! human-readable info
  string_t describe (void) const;
  //! human- & machine-readable info
  string_t yaml (string_t tab = "") const;
  //! R list description
  SEXP structure (void) const;
  //! put genealogy at time `t` into Newick format.
  string_t newick (slate_t t, bool showdeme, bool extended) const;

};

#endif
