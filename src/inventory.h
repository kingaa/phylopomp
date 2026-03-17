// -*- C++ -*-
// INVENTORY CLASS

#ifndef _INVENTORY_H_
#define _INVENTORY_H_

#include <utility>
#include "ball.h"
#include "pocket.h"
#include "node.h"
#include "internal.h"

//! Representation for the inventory process.

//! An inventory consists of an array of demes.
//! Each deme is a set of black balls.
template <size_t NDEME = 1>
class inventory_t {

private:

  pocket_t _inven[NDEME];
  const static size_t ndeme = NDEME;

public:

  // CONSTRUCTORS, DESTRUCTORS, ETC.
  //! basic constructor for inventory class
  inventory_t (void) = default;
  //! constructor from serialized binary form
  inventory_t (raw_t *o) {
    o >> *this;
  };
  //! constructor from node sequence (via 'extant' operation).
  inventory_t (std::pair<node_it,node_it>&& I) {
    clean();
    for (node_it i = I.first; i != I.second; i++) {
      for (ball_t *b : **i) {
        insert(b);
      }
    }
  };
  //! copy constructor
  inventory_t (const inventory_t &) = default;
  //! move constructor
  inventory_t (inventory_t &&) = delete;
  //! copy an inventory by iterating over a node sequence
  inventory_t& operator= (std::pair<node_it,node_it>&& I) {
    clean();
    for (node_it i = I.first; i != I.second; i++) {
      for (ball_t *b : **i) {
        insert(b);
      }
    }
    return *this;
  };
  //! copy assignment operator
  inventory_t & operator= (const inventory_t &) = default;
  //! move assignment operator
  inventory_t & operator= (inventory_t &&) = delete;
  //! destructor
  ~inventory_t (void) {
    clean();
  };
  //! memory cleanup
  void clean (void) {
    clear();
  };
  //! memory cleanup
  void clear (void) {
    for (size_t i = 0; i < ndeme; i++)
      _inven[i].clear();
  };
  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    size_t s = 0;
    for (size_t i = 0; i < ndeme; i++)
      s += _inven[i].bytesize();
    return s;
  };
  //! binary serialization
  friend raw_t* operator>> (const inventory_t& I, raw_t* o) {
    for (size_t i = 0; i < ndeme; i++) {
      o = (I._inven[i] >> o);
    }
    return o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t* o, inventory_t& I) {
    I.clean();
    for (size_t i = 0; i < ndeme; i++) {
      o = (o >> I._inven[i]);
    }
    return o;
  };
  //! Total number of balls in an inventory.
  size_t size (void) const {
    size_t n = 0;
    for (name_t i = 0; i < ndeme; i++) {
      n += size(i);
    }
    return n;
  };
  //! size of deme
  size_t size (name_t i) const {
    return _inven[i].size();
  };
  //! return the `n`-th deme
  pocket_t& operator[] (const name_t n) {
    return _inven[n];
  };
  //! const access to the `n`-th deme
  const pocket_t& operator[] (const name_t n) const {
    return _inven[n];
  };
  //! are all demes empty?
  bool empty (void) const {
    bool q = true;
    for (name_t i = 0; i < ndeme; i++) {
      q = q && _inven[i].empty();
    }
    return q;
  };
  //! choose one random ball from deme `i`
  ball_t* random_ball (name_t i = 0) const {
    name_t n = _inven[i].size();
    assert(n > 0);
    name_t draw = random_integer(n);
    ball_it k = _inven[i].begin();
    while (draw-- > 0) k++;
    return *k;
  };
  //! choose a random set of `n` balls from deme `i`
  pocket_t* random_balls (name_t i = 0, int n = 1) const {
    pocket_t *p = new pocket_t();
    if (n == 1) {
      ball_t *b = random_ball(i);
      p->insert(b);
    } else if (n > 1) {
      int N = _inven[i].size();
      assert(N > 0);
      assert(n <= N);
      ball_it j = _inven[i].begin();
      int k = 0, m = 0;
      while (m < n && k < N) {
        int u = random_integer(N-k);
        if (u < n-m) {
          p->insert(*j);
          m++;
        }
        k++; j++;
      }
    } else {
      assert(0);
    }
    return p;
  };
  //! retrieve ball by index in deme `i`
  //! (for synchronized cross-segment operations)
  ball_t* get_ball_idx (name_t idx = 0, name_t i = 0) const {
    size_t n = _inven[i].size();
    if (n < 1)
      err("in '%s': cannot draw from empty inventory %ld",__func__,i);
    if (idx >= n)
      err("in '%s': index exceeds size of inventory in deme %ld",__func__,i);
    ball_it k = _inven[i].begin();
    while (idx-- > 0) k++;
    return *k;
  };
  //! add a black ball to a deme.
  void insert (ball_t *b) {
    if (b->is(black)) {
      _inven[b->deme()].insert(b);
    }
  };
  //! remove a black ball from its deme.
  void erase (ball_t *b) {
    if (b->is(black)) {
      assert(!(_inven[b->deme()].empty()));
      _inven[b->deme()].erase(b);
    }
  };
};

#endif
