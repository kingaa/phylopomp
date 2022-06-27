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
      (*i)->repair_owners(node_names,&ball_names);   // in pocket.h
    }
    G.repair_owners(ball_names);                     // in nodeseq.h
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
        err("in '%s': cannot find node %ld (nodeseq.h)",__func__,p->uniq); // #nocov
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
  std::string newick (slate_t t, bool compact = true) const {
    slate_t te = dawn();
    std::string o = "(i_NA_NA:0.0,i_NA_NA:0.0";
    for (node_it i = begin(); i != end(); i++) {
      if ((*i)->is_root()) {
        o += ",(" + ((compact) ? (*i)->compact_newick(t,te) : (*i)->newick(t,te)) + ")i_NA_NA:0.0";
      }
    }
    o += ")i_NA_NA;";
    return o;
  };

};

#endif

