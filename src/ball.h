// -*- C++ -*-
// BALL CLASS
#ifndef _BALL_H_
#define _BALL_H_
  
#include <string>
#include <cstring>
#include "internal.h"

//! BALL COLORS

//! NB: The correctness of the algorithms depends on green being the first color.
typedef enum {green, black, blue, red, grey, purple} color_t;
static const char* colores[] = {"green", "black", "blue", "red", "grey", "purple"};
static const char* colorsymb[] = {"g", "o", "b", "r", "z", "p"};

class node_t;

//! Balls function as pointers.

//! Each ball has:
//! - a globally unique name
//! - a color
//! - a "holder": a pointer to the node in whose pocket it lies
//! - an "owner": a pointer to the node in which it was originally created
//! - a deme
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
    memcpy(o,&b.uniq,sizeof(name_t)); o += sizeof(name_t);
    memcpy(o,&b._deme,sizeof(name_t)); o += sizeof(name_t);
    memcpy(o,&b.color,sizeof(color_t)); o += sizeof(color_t);
    return o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t *o, ball_t &b) {
    memcpy(&b.uniq,o,sizeof(name_t)); o += sizeof(name_t);
    memcpy(&b._deme,o,sizeof(name_t)); o += sizeof(name_t);
    memcpy(&b.color,o,sizeof(color_t)); o += sizeof(color_t);
    b._holder = 0;              // must be set elsewhere
    b._owner = 0;               // must be set elsewhere
    return o;
  };
public:
  //! basic constructor for ball class
  ball_t (node_t *who = 0, name_t u = 0, color_t col = green, name_t d = 0) {
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
  //! view/change deme
  name_t& deme (void) {
    if (color != black)
      err("meddle not in the deme of a non-black ball!"); // #nocov
    return _deme;
  };
  //! view/change owner
  node_t*& owner (void) {
    if (color != green)
      err("meddle not with the owner of a ball that is not green."); // #nocov
    return _owner;
  };
  //! in whose pocket do I lie?
  node_t*& holder (void) {
    return _holder;
  };
  node_t* child (void) const {
    return _owner;
  };
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
  //! human-readable info
  std::string describe (void) const {
    std::string o = color_name()
      + "(" + std::to_string(uniq);
    if (is(black)) {
      o += "," + std::to_string(_deme);
    }
    o += ")";
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
  //! element of a newick representation
  std::string newick (const slate_t &t) const {
    return color_symbol()
      + "_" + std::to_string(_deme)
      + "_" + std::to_string(uniq)
      + ":" + std::to_string(t);
  };
  //! arbitrary order relation
  friend bool compare (const ball_t*a, const ball_t* b) {
    return (a->uniq < b->uniq) ||
      ((a->uniq == b->uniq) && (a->color < b->color));
  }
};

#endif
