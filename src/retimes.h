// -*- C++ -*-
// The REASSORTMENT TIMES class

#ifndef _RETIMES_H_
#define _RETIMES_H_

#include <list>
#include "internal.h"

typedef typename std::list<slate_t*>::const_iterator retime_it;

//! A sequence of times
class retimes_t : public std::list<slate_t*> {
private:
  //! clean up: delete all times, reset globals
  void clean (void) {
    for (retime_it i = begin(); i != end(); i++) delete *i;
    clear();
  };
  
public:
  //! destructor
  ~retimes_t (void) {
    clean();
  };
  
  // SERIALIZATION
  //! size of serialized binary form
  size_t bytesize (void) const {
    return sizeof(size_t) + size()*sizeof(slate_t);
  };
  //! binary serialization
  //! not sure
  friend raw_t* operator>> (const retimes_t& r, raw_t *o) {
    size_t rsize = r.size();
    memcpy(o,&rsize,sizeof(size_t)); o += sizeof(size_t);
    for (retime_it i = r.begin(); i != r.end(); i++) 
      memcpy(o,&**i,sizeof(slate_t)); o += sizeof(slate_t);
    return o;
  };
  //! binary deserialization
  friend raw_t* operator>> (raw_t *o, retimes_t &r) {
    r.clean();
    size_t rsize;
    memcpy(&rsize,o,sizeof(size_t)); o += sizeof(size_t);
    for (retime_it i = r.begin(); i != r.end(); i++) {
      memcpy(&**i,o,sizeof(slate_t)); o += sizeof(slate_t);
    }
    return o;
  };
};

#endif