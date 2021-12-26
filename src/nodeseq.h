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

class nodeseq_t : public std::list<node_t*> {

 private:
  // clean up: delete all nodes, reset globals
  void clean (void) {
    for (node_it i = begin(); i != end(); i++) delete *i;
    clear();
  };

 public:
  // destructor
  ~nodeseq_t (void) {
    clean();
  };
  
  // SERIALIZATION
  // size of serialized binary form
  size_t bytesize (void) const {
    size_t s = sizeof(size_t);
    for (node_it i = begin(); i != end(); i++)
      s += (*i)->bytesize();
    return s;
  };
  // binary serialization
  friend raw_t* operator<< (raw_t* o, const nodeseq_t& G) {
    size_t nnode = G.size();
    memcpy(o,&nnode,sizeof(size_t)); o += sizeof(size_t);
    for (node_it i = G.begin(); i != G.end(); i++) {
      o = (o << **i);
    }
    return o;
  };
  // binary deserialization
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
      (*i)->set_owners(node_names,&ball_names);
    }
    G.set_green_balls(ball_names);
    return o;
  };
  void set_green_balls (std::unordered_map<name_t,ball_t*>& names) {
    std::unordered_map<name_t,ball_t*>::const_iterator n;
    for (node_it i = begin(); i != end(); i++) {
      node_t *p = *i;
      n = names.find(p->uniq);
      if (n != names.end()) {
	ball_t *b = n->second;
	p->green_ball(b);
      } else {
	err("cannot find node id %ld",p->uniq); // # nocov
      }
      
    }
  };
  
private:

  slate_t dawn (void) const {
    return (empty()) ? R_NaReal : front()->slate;
  };
  slate_t dusk (void) const {
    return (empty()) ? R_NaReal : back()->slate;
  }

 public:
  
  // all balls of a color
  pocket_t* colored (color_t col) const {
    pocket_t *p = new pocket_t;
    for (node_it i = begin(); i != end(); i++) {
      for (ball_it j = (*i)->pocket.begin(); j != (*i)->pocket.end(); j++) {
	if ((*j)->is(col)) p->insert(*j);
      }
    }
    return p;
  };
  // number of roots
  size_t nroot (void) const {
    size_t n = 0;
    for (node_it i = begin(); i != end(); i++) {
      if ((*i)->is_root()) n++;
    }
    return n;
  };
  // number of distinct timepoints
  size_t ntime (void) const {
    size_t count = 1;
    slate_t tcur = R_NegInf;
    for (node_it i = begin(); i != end(); i++) {
      if (tcur < (*i)->slate) {
	tcur = (*i)->slate;
	count++;
      }
    }
    return count;
  };
  size_t lineage_count (double *t, int *ell) const {
    size_t count = 0;
    slate_t tcur = R_NegInf;
    size_t n = nroot();
    *ell = double(n);
    for (node_it i = begin(); i != end(); i++) {
      n += (*i)->nchildren(true)-1;
      if (tcur < (*i)->slate) {
	*(t++) = tcur = (*i)->slate;
	*(ell++) = n;
	count++;
      }
    }
    return count;
  };
  // human-readable info
  std::string describe (void) const {
    std::string o = "";
    for (node_it p = begin(); p != end(); p++) {
      o += (*p)->describe();
    }
    return o;
  };
  // human- & machine-readable info
  virtual std::string yaml (std::string tab = "") const {
    std::string o = "";
    std::string t = tab + "  ";
    for (node_it p = begin(); p != end(); p++) {
      o += tab + "- " + (*p)->yaml(t);
    }
    return o;
  };
  // R list description
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
  // put genealogy at current time into Newick format.
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

