// -*- C++ -*-
// INVENTORY CLASS
// An inventory consists of an array of demes.
// Each deme is a set of black balls.

#ifndef _INVENTORY_H_
#define _INVENTORY_H_

#include "internal.h"
#include "ball.h"

template<size_t ndeme = 1>
class inventory_t {
private:
  pocket_t _inven[ndeme];
public:
  // basic constructor for inventory class
  inventory_t (void) = default;
  // copy constructor
  inventory_t (const inventory_t &) = default;
  /// move constructor
  inventory_t (inventory_t &&) = delete;
  // copy assignment operator
  inventory_t & operator= (const inventory_t &) = delete;
  // move assignment operator
  inventory_t & operator= (inventory_t &&) = delete;
  // destructor
  ~inventory_t (void) {
    clean();
  };
  void clean (void) {
    clear();
  };
  void clear (void) {
    for (size_t i = 0; i < ndeme; i++)    
      _inven[i].clear();
  };
  // size of inventory
  size_t size (void) const {
    size_t n = 0;
    for (name_t i = 0; i < ndeme; i++) {
      n += size(i);
    }
    return n;
  };
  // size of deme
  size_t size (name_t i) const {
    return _inven[i].size();
  };
  // n-th deme
  pocket_t& operator[] (const name_t n) {
    return _inven[n];
  };
  // are all demes empty?
  bool empty (void) const {
    bool q = true;
    for (name_t i = 0; i < ndeme; i++) {
      q = q && _inven[i].empty();
    }
    return q;
  };
  // random ball
  ball_t* random_ball (name_t i) const {
    name_t n = _inven[i].size();
    if (n < 1) err("cannot draw from empty inventory %ld",i); // # nocov
    name_t draw = random_integer(n);
    ball_it k = _inven[i].begin();
    while (draw-- > 0) k++;
    return *k;
  };
  // random pair of balls
  void random_pair (ball_t* ballI, ball_t* ballJ, name_t i, name_t j) const {
    if (i != j) {
      ballI = random_ball(i);
      ballJ = random_ball(j);
    } else {
      name_t n = _inven[i].size();
      if (n < 2) err("cannot draw from inventory %ld",i); // # nocov
      name_t d1 = random_integer(n-1);
      name_t d2 = random_integer(n);
      bool toggle = false;
      if (d1 >= d2) {
	toggle = true;
	d1++;
	n = d1; d1 = d2; d2 = n;
      }
      ball_it k = _inven[i].begin();
      while (d1 > 0) {
	d1--; d2--; k++;
      }
      if (toggle) ballJ = *k;
      else ballI = *k;
      while (d2 > 0) {
	d2--; k++;
      }
      if (toggle) ballI = *k;
      else ballJ = *k;
    }
  };
  // add black ball to deme i;
  void insert (ball_t *b, name_t i) {
    if (b->is(black)) {
      _inven[i].insert(b);
      b->deme = i;
    }
  };
  // remove black ball from its deme
  void erase (ball_t *b) {
    if (b->is(black)) {
      if (_inven[b->deme].empty())
	err("in 'inventory::remove': empty deme %ld.",b->deme); // # nocov
      _inven[b->deme].erase(b);
    }
  };
};
  
#endif
