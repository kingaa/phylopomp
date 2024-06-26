// -*- C++ -*-
// BALL CLASS
#ifndef _BALL_H_
#define _BALL_H_

#include <string>
#include <cstring>
#include "internal.h"

//! BALL COLORS

//! NB: The correctness of the algorithms depends on
//! green being the first color and black being the last.
typedef enum {green, blue, black} color_t;
static const char* colores[] = {"green", "blue", "black"};
static const char* colorsymb[] = {"g", "b", "o"};
static const name_t undeme = name_t(NA_INTEGER);

class node_t;

//! Balls function as pointers.

//! Each ball has:
//! - a globally unique name
//! - a color
//! - a deme
//! - a "holder": a pointer to the node in whose pocket it lies
//! - an "owner": a pointer to the node in which it was originally created
class ball_t {

private:
  node_t *_holder;
  node_t *_owner;
  name_t _deme;

public:
  name_t uniq;
  color_t color;

public:

  //! size of binary serialization
  static const size_t bytesize = 2*sizeof(name_t)+sizeof(color_t);
  //! binary serialization
  friend raw_t* operator>> (const ball_t &b, raw_t *o) {
    name_t buf[2] = {b.uniq, b._deme};
    memcpy(o,buf,sizeof(buf)); o += sizeof(buf);
    memcpy(o,&b.color,sizeof(color_t)); o += sizeof(color_t);
    return o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t *o, ball_t &b) {
    name_t buf[2];
    memcpy(buf,o,sizeof(buf)); o += sizeof(buf);
    b.uniq = buf[0]; b._deme = buf[1];
    memcpy(&b.color,o,sizeof(color_t)); o += sizeof(color_t);
    b._holder = 0;              // must be set elsewhere
    b._owner = 0;               // must be set elsewhere
    return o;
  };

public:

  //! basic constructor for ball class
  ball_t (node_t *who = 0, name_t u = 0,
          color_t col = green, name_t d = undeme) {
    _holder = _owner = who;
    uniq = u;
    color = col;
    _deme = d;
  };
  //! copy constructor
  ball_t (const ball_t&) = delete;
  //! move constructor
  ball_t (ball_t&&) = delete;
  //! copy assignment operator
  ball_t & operator= (const ball_t&) = delete;
  //! move assignment operator
  ball_t & operator= (ball_t&&) = delete;
  //! destructor
  ~ball_t (void) = default;

public:

  //! view deme
  name_t deme (void) const {
    return _deme;
  };
  //! change deme
  name_t& deme (void) {
    return _deme;
  };
  //! view owner of a green ball
  node_t* owner (void) const {
    assert(color==green);
    return _owner;
  };
  //! change owner of a green ball
  node_t*& owner (void) {
    assert(color==green);
    return _owner;
  };
  //! a child is the owner of a green ball
  node_t* child (void) const {
    assert(color==green);
    return _owner;
  };
  //! in whose pocket do I lie?
  node_t* holder (void) const {
    return _holder;
  };
  //! in whose pocket do I lie?
  node_t*& holder (void) {
    return _holder;
  };
  //! is a given ball of the given color?
  bool is (color_t c) const {
    return color==c;
  };
  //! human-readable colors
  std::string color_name (void) const {
    return colores[color];
  };
  //! machine-readable color symbols
  std::string color_symbol (void) const {
    if (is(green) && _holder==_owner)
      return "m";               // brown balls
    else
      return colorsymb[color];
  };

public:

  //! human-readable info
  std::string describe (void) const {
    std::string o = color_name()
      + "(" + std::to_string(uniq) + ",";
    if (_deme != undeme) {
      o += std::to_string(_deme);
    }
    o += ")";
    return o;
  };
  //! machine-readable info
  std::string yaml (std::string tab = "") const {
    std::string o;
    o = "color: " + color_name() + "\n"
      + tab + "name: " + std::to_string(uniq) + "\n";
    if (color==black) {
      o += tab + "deme: " + std::to_string(_deme) + "\n";
    }
    return o;
  };
  //! R list description
  SEXP structure (void) const {
    SEXP O, On, Name, Color, Deme;
    int size = (is(black)) ? 3 : 2;
    PROTECT(O = NEW_LIST(size));
    PROTECT(On = NEW_CHARACTER(size));
    PROTECT(Name = NEW_INTEGER(1));
    *INTEGER(Name) = int(uniq);
    PROTECT(Color = NEW_CHARACTER(1));
    SET_STRING_ELT(Color,0,mkChar(color_symbol().c_str()));
    set_list_elem(O,On,Name,"name",0);
    set_list_elem(O,On,Color,"color",1);
    if (is(black)) {
      PROTECT(Deme = NEW_INTEGER(1));
      *INTEGER(Deme) = int(_deme);
      set_list_elem(O,On,Deme,"deme",2);
      UNPROTECT(1);
    }
    SET_NAMES(O,On);
    UNPROTECT(4);
    return O;
  };
  //! element of a newick representation
  std::string newick (const slate_t &t) const {
    return color_symbol()
      + "_" + std::to_string(_deme)
      + "_" + std::to_string(uniq)
      + ":" + std::to_string(t);
  };
};

#endif
