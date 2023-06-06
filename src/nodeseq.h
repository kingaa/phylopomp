// -*- C++ -*-
// NODE SEQUENCE CLASS

#ifndef _NODESEQ_H_
#define _NODESEQ_H_

#include <list>
#include <unordered_map>
#include <string>
#include <cstring>
#include "internal.h"
#include "node.h"

typedef typename std::list<node_t*>::const_iterator node_it;
typedef typename std::list<node_t*>::iterator node_nit;
typedef typename std::list<node_t*>::const_reverse_iterator node_rev_it;

//! A sequence of nodes.
class nodeseq_t : public std::list<node_t*> {

private:
  //! clean up: delete all nodes, reset globals
  void clean (void) {
    for (node_it i = begin(); i != end(); i++) delete *i;
    clear();
  };

public:
  //! destructor
  ~nodeseq_t (void) {
    clean();
  };
  
  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    size_t s = sizeof(size_t);
    for (node_it i = begin(); i != end(); i++)
      s += (*i)->bytesize();
    return s;
  };
  //! binary serialization
  friend raw_t* operator>> (const nodeseq_t& G, raw_t* o) {
    size_t nnode = G.size();
    memcpy(o,&nnode,sizeof(size_t)); o += sizeof(size_t);
    for (node_it i = G.begin(); i != G.end(); i++) {
      o = (**i >> o);
    }
    return o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, nodeseq_t& G) {
    G.clean();
    std::unordered_map<name_t,node_t*> node_names;
    std::unordered_map<name_t,ball_t*> ball_names;
    size_t nnode;
    memcpy(&nnode,o,sizeof(size_t)); o += sizeof(size_t);
    node_names.reserve(nnode);
    ball_names.reserve(nnode);
    for (size_t i = 0; i < nnode; i++) {
      node_t *p = new node_t();
      o = (o >> *p);
      G.push_back(p);
      node_names.insert({p->uniq,p});
    }
    for (node_it i = G.begin(); i != G.end(); i++) {
      (*i)->repair_owners(node_names,&ball_names);
    }
    G.repair_owners(ball_names);
    return o;
  };

private:
  //! Needed in deserialization.
  //! This function repairs the links green balls and their names.
  void repair_owners (std::unordered_map<name_t,ball_t*>& names) {
    std::unordered_map<name_t,ball_t*>::const_iterator n;
    for (node_it i = begin(); i != end(); i++) {
      node_t *p = *i;
      n = names.find(p->uniq);
      if (n != names.end()) {
        ball_t *b = n->second;
        p->green_ball() = b;
      } else {
        err("in '%s': cannot find node %ld",__func__,p->uniq); // #nocov
      }
      
    }
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
    for (node_it i = begin(); i != end(); i++) {
      for (ball_it j = (*i)->begin(); j != (*i)->end(); j++) {
        if ((*j)->is(col)) p->insert(*j);
      }
    }
    return p;
  };
  //! Number of distinct timepoints.
  size_t ntime (slate_t t) const {
    size_t count = 1;
    for (node_it i = begin(); i != end(); i++) {
      if (t < (*i)->slate) {
        t = (*i)->slate;
        count++;
      }
    }
    return count;
  };
  //! human-readable info
  std::string describe (void) const {
    std::string o = "";
    for (node_it p = begin(); p != end(); p++) {
      o += (*p)->describe();
    }
    return o;
  };
  //! human- & machine-readable info
  virtual std::string yaml (std::string tab = "") const {
    std::string o = "";
    std::string t = tab + "  ";
    for (node_it p = begin(); p != end(); p++) {
      o += tab + "- " + (*p)->yaml(t);
    }
    return o;
  };
  //! R list description
  SEXP structure (void) const {
    SEXP Nodes;
    PROTECT(Nodes = NEW_LIST(size()));
    int k = 0;
    for (node_it i = begin(); i != end(); i++) {
      SET_ELEMENT(Nodes,k++,(*i)->structure());
    }
    UNPROTECT(1);
    return Nodes;
  };
  //! put genealogy at time `t` into Newick format.
  std::string newick (slate_t t) const {
    slate_t te = dawn();
    std::string o = "";
    for (node_it i = begin(); i != end(); i++) {
      if ((*i)->is_root()) {
        o += (*i)->newick(t,te) + ";";
      }
    }
    return o;
  };

  //! swap balls a and b, wherever they lie
  void swap (ball_t *a, ball_t *b) {
    node_t *p = a->holder();
    node_t *q = b->holder();
    if (p != q) {
      p->erase(a); q->insert(a); a->holder() = q;
      q->erase(b); p->insert(b); b->holder() = p;
    }
  };

  //! add node p; take as parent the node holding ball a.
  //! the deme of p is changed to match that of a
  void add (node_t *p, ball_t *a) {
    swap(a,p->green_ball());
    p->deme = a->deme();
    push_back(p);
  };

  //! drop the black ball 'a' and the node if either
  //! (1) the node becomes thereby a dead root, or
  //! (2) the node's pocket becomes thereby empty.
  void drop (ball_t *a) {
    if (!a->is(black))
      err("in '%s': inconceivable! (color: %s)",__func__,colores[a->color]); // #nocov
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
    if (!p->dead_root())
      err("in '%s': invalid call.",__func__); // #nocov
    remove(p);
    delete p;
  };

  //! remove a dead root node
  void destroy_node (node_nit i) {
    if (!(*i)->dead_root())
      err("in '%s': invalid call.",__func__); // #nocov
    erase(i);
    delete *i;
  };

  //! pass through the sequence, dropping superfluous nodes
  //! i.e., those holding just one ball that is green.
  void comb (void) {
    for (node_rev_it i = crbegin(); i != crend(); i++) {
      if ((*i)->size() == 1 && (*i)->holds(green)) {
        swap((*i)->last_ball(),(*i)->green_ball());
      }
    }
    node_nit j = begin();
    while (j != end()) {
      if ((*j)->dead_root()) {
        destroy_node(*(j++));
      } else {
        j++;
      }
    }
  };
  //! merge two node sequences
  nodeseq_t& operator+= (nodeseq_t& other) {
    this->merge(other,node_compare);
    return *this;
  };
};

#endif
