// -*- C++ -*-
// INVENTORY CLASS

#ifndef _INVENTORY_H_
#define _INVENTORY_H_

#include "ball.h"
#include "pocket.h"
#include "node.h"
#include "genealogy.h"
#include "internal.h"

//! Implementation of the inventory process.

//! An inventory consists of an array of demes.
//! Each deme is a set of black balls.
template <size_t NDEME>
class inventory_t {

private:

  const static size_t ndeme = NDEME;
  pocket_t _inven[ndeme];

public:

  // CONSTRUCTORS, DESTRUCTORS, ETC.
  //! basic constructor for inventory class
  inventory_t (void) = default;
  //! constructor from serialized binary form
  inventory_t (raw_t *o) {
    o >> *this;
  };
  //! copy constructor
  inventory_t (const inventory_t &) = default;
  //! move constructor
  inventory_t (inventory_t &&) = delete;
  //! constructs an inventory from a genealogy
  inventory_t (const genealogy_t& G) {
    assert(G.ndeme()==ndeme);
    clean();
    for (node_t *p : G) {
      for (ball_t *b : *p) {
        insert(b);              // 'insert' checks color
      }
    }
  };
  //! copy an inventory from a genealogy
  inventory_t & operator= (const genealogy_t& G) {
    assert(G.ndeme()==ndeme);
    clean();
    for (node_t *p : G) {
      for (ball_t *b : *p) {
        insert(b);              // 'insert' checks color
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

private:

  //! memory cleanup
  void clean (void) {
    clear();
  };
  //! memory cleanup
  void clear (void) {
    for (size_t i = 0; i < ndeme; i++)
      _inven[i].clear();
  };

public:

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

public:

  //! Total number of balls in an inventory.
  //! i.e., the sum of the sizes of all demes
  size_t size (void) const {
    size_t n = 0;
    for (name_t i = 0; i < ndeme; i++) {
      n += size(i);
    }
    return n;
  };
  //! size of deme
  size_t size (name_t i) const {
    return inven(i).size();
  };
  //! return the `i`-th deme
  const pocket_t& operator[] (const name_t i) const {
    return inven(i);
  };
  //! return the `i`-th deme
  pocket_t& operator[] (const name_t i) {
    return inven(i);
  };

private:

  //! access the `i`-th deme
  const pocket_t& inven (const name_t i) const {
    assert(i > 0);
    return _inven[i-1];
  };
  //! access the `i`-th deme
  pocket_t& inven (const name_t i) {
    assert(i > 0);
    return _inven[i-1];
  };

public:

  //! add a black ball to a deme.
  //! this checks the color of the ball.
  //! if it is not black, nothing is done.
  void insert (ball_t *b) {
    if (b->is(black)) {
      inven(b->deme()).insert(b);
    }
  };
  //! remove a black ball from its deme.
  //! this checks the color of the ball.
  //! if it is not black, nothing is done.
  void erase (ball_t *b) {
    if (b->is(black)) {
      assert(!inven(b->deme()).empty());
      inven(b->deme()).erase(b);
    }
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
  ball_t* random_ball (name_t i) const {
    name_t n = inven(i).size();
    assert(n > 0);
    name_t draw = random_integer(n);
    ball_it k = inven(i).begin();
    while (draw-- > 0) k++;
    return *k;
  };
  //! choose a random set of `n` balls from deme `i`
  pocket_t* random_balls (name_t i, int n = 1) const {
    pocket_t *p = new pocket_t();
    if (n == 1) {
      ball_t *b = random_ball(i);
      p->insert(b);
    } else if (n > 1) {
      int N = inven(i).size();
      assert(N > 0);
      assert(n <= N);
      ball_it j = inven(i).begin();
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
      assert(0);                // #nocov
    }
    return p;
  };
};

#endif
