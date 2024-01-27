// -*- C++ -*-
// The POCKET class

#ifndef _POCKET_H_
#define _POCKET_H_

#include <set>
#include <unordered_map>
#include <string>
#include <cstring>
#include "internal.h"
#include "ball.h"

//! Ordering for balls in pockets.

//! Without this, order depends on machine state,
//! defeating reproducibility.
//! The correctness of the algorithms depends on this order.
struct ball_order {
  bool operator() (const ball_t* a, const ball_t* b) const {
    return (a->color < b->color) ||
      ((a->color == b->color) && (a->uniq < b->uniq));
  };
};

typedef typename std::set<ball_t*,ball_order>::const_iterator ball_it;
typedef typename std::set<ball_t*,ball_order>::const_reverse_iterator ball_rev_it;

//! A pocket is a set of balls.

//! An order relation among balls ensures the uniqueness of the internal representation.
class pocket_t : public std::set<ball_t*,ball_order> {

private:

  //! delete balls and clear pocket
  void clean (void) {
    for (ball_it i = begin(); i != end(); i++) delete *i;
    clear();
  };

public:

  // SERIALIZATION
  //! size of binary serialization
  size_t bytesize (void) const {
    return sizeof(size_t) + size()*(ball_t::bytesize);
  };
  //! binary serialization
  friend raw_t* operator>> (const pocket_t &p, raw_t *o) {
    size_t psize = p.size();
    memcpy(o,&psize,sizeof(size_t)); o += sizeof(size_t);
    for (ball_it i = p.begin(); i != p.end(); i++)
      o = (**i >> o);
    return o;
  };
  //! binary deserialization.
  //! this leaves the balls without knowledge of their holder.
  friend raw_t* operator>> (raw_t *o, pocket_t &p) {
    p.clean();
    size_t psize;
    memcpy(&psize,o,sizeof(size_t)); o += sizeof(size_t);
    for (size_t i = 0; i < psize; i++) {
      ball_t *b = new ball_t();
      o = (o >> *b);
      p.insert(b);
    }
    return o;
  };

protected:
  //! Needed in deserialization.
  //! Inform all balls as to their holder.
  void repair_holder (node_t* p) {
    for (ball_it i = begin(); i != end(); i++) {
      (*i)->holder() = p;
    }
  };

public:
  //! Needed in deserialization.
  //! This function repairs the links green balls and their names.
  void repair_owners (const std::unordered_map<name_t,node_t*>& node_name,
                      std::unordered_map<name_t,ball_t*> *ball_name) {
    std::unordered_map<name_t,node_t*>::const_iterator n;
    for (ball_it i = begin(); i != end(); i++) {
      ball_t *b = *i;
      if (b->is(green)) {
        n = node_name.find(b->uniq);
        if (n != node_name.end()) {
          node_t *p = n->second;
          b->owner() = p;
          ball_name->insert({b->uniq,b});
        } else {
          err("in '%s': cannot find ball %ld",__func__,b->uniq); // #nocov
        }
      }
    }
  };

public:

  //! destructor
  ~pocket_t (void) {
    clean();
  };

public:

  //! does this node hold the given ball?
  bool holds (ball_t *b) const {
    ball_it i = find(b);
    return (i != end());
  };
  //! does this node hold a ball of this color?
  bool holds (color_t c) const {
    bool result = false;
    for (ball_it i = begin(); !result && i != end(); i++) {
      result = ((*i)->color == c);
    }
    return result;
  };
  //! retrieve the last ball
  ball_t* last_ball (void) const {
    return *crbegin();
  };
  //! retrieve the first ball of the specified color.
  ball_t* ball (const color_t c) const {
    for (ball_it i = begin(); i != end(); i++) {
      if ((*i)->color == c) return *i;
    }
    err("no ball of color %s",colores[c]); // # nocov
    return 0;
  };
  //! return a pointer to another ball
  ball_t* other (const ball_t *b) const {
    for (ball_it i = begin(); i != end(); i++) {
      if (*i != b) return *i;
    }
    err("error in '%s': there is no other.",__func__); // # nocov
    return 0;
  };
  //! human-readable info
  std::string describe (void) const {
    std::string s = "{";
    ball_it i = begin();
    s += (*i)->describe(); ++i;
    while (i != end()) {
      s += ", " + (*i)->describe(); ++i;
    }
    s += "}";
    return s;
  };
  //! R list description
  SEXP structure (void) const {
    SEXP o;
    PROTECT(o = NEW_LIST(size()));
    int k = 0;
    for (ball_rev_it i = crbegin(); i != crend(); i++) {
      SET_ELEMENT(o,k++,(*i)->structure());
    }
    UNPROTECT(1);
    return o;
  };
  //! human/machine-readable info
  std::string yaml (std::string tab = "") const {
    std::string o = "";
    std::string t = tab + "  ";
    for (ball_it i = begin(); i != end(); i++) {
      o += tab + "- " + (*i)->yaml(t);
    }
    return o;
  };
};

#endif
