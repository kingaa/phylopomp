// -*- C++ -*-
// Generic Genealogy Process (GP) Simulator (C++)
// State of the GP is represented as a "tableau".

#ifndef _GP_H_
#define _GP_H_

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <vector>
#include <string>
#include <cstring>

#define err(...) errorcall(R_NilValue,__VA_ARGS__)
#define warn(...) warningcall(R_NilValue,__VA_ARGS__)
#define rprint(S) Rprintf("%s\n",(S).c_str())

typedef Rbyte raw_t; // must match with R's 'Rbyte' (see Rinternals.h)
typedef unsigned int name_t;

const name_t na = name_t(R_NaInt);
const double inf = R_PosInf;
const double default_slate = R_NegInf;
const size_t MEMORY_MAX = (1<<26); // roughly 1/4 of mem/cpu

// interface with R's integer RNG
static int random_integer (int n) {
  return int(floor(R_unif_index(double(n))));
}

// helper function for filling a return list
static int set_list_elem (SEXP list, SEXP names, SEXP element,
                          const char *name, int pos) {
  SET_ELEMENT(list,pos,element);
  SET_STRING_ELT(names,pos,mkChar(name));
  return ++pos;
}

// BALL COLORS
// green must be first, numbers in sequence.
static const name_t ncolors = 6;
static const char *colores[] = {"green", "black", "blue", "red", "grey"};
static const char *colorsymb[] = {"g", "o", "b", "r", "z"};
typedef enum {green = 0, black = 1, blue = 2, red = 3, grey = 4} color_t;

// GP TABLEAU CLASS
// the class to hold the state of the genealogy process (a "tableau")..
// STATE is a datatype that holds the state of the Markov process.
template <class STATE>
class gp_tableau_t  {

protected:

  typedef STATE state_t;
  class ball_t;
  class player_t;

protected:
  
  typedef std::vector<ball_t*> balls_t;
  typedef std::vector<player_t*> players_t;

  name_t _unique;                           // next unique name
  union {player_t *left; name_t leftmost;}; // player seated farthest to left
  union {player_t *right; name_t rightmost;}; // player seated farthest to right
  double _t0;                                 // initial time
  double _time;                               // current time
  players_t player;             // pointers to all players
  balls_t balls[ncolors];       // one for each color class
  bool use_ghosts;              // insert a ghost to track state after death?
  state_t state;                // current state of the GP

protected:

  // BALL CLASS
  // each ball has:
  // - a color
  // - a unique name within its color
  // - the name of the player in whose hand it lies.
  class ball_t {
  private:
    name_t _hand;
  public:
    name_t name;
    color_t color;
    // basic constructor
    ball_t (color_t col = blue, name_t nom = na, name_t who = na) {
      color = col;
      name = nom;
      _hand = who;
    };
    // copy constructor
    ball_t (const ball_t &b) = default;
    // move constructor
    ball_t (ball_t &&b) = delete;
    // copy assignment operator
    ball_t & operator= (const ball_t & b) = delete;
    // move assignment operator
    ball_t & operator= (ball_t && b) = delete;
    // destructor
    ~ball_t (void) = default;
    // in whose hand do I lie?
    name_t hand (void) const {
      return _hand;
    };
    // change hands
    void hand (name_t &who) {
      _hand = who;
    };
    bool is (color_t c) const {
      return color==c;
    };
    // human-readable colors
    std::string color_name (void) const {
      return colores[color];
    };
    // machine-readable color symbols
    std::string color_symbol (void) const {
      return colorsymb[color];
    };
    // human-readable info
    std::string describe (const gp_tableau_t *T) const {
      if (color==green) {
        return color_name() + "(" + std::to_string(T->player[name]->uniq) + ")";
      } else {
        return color_name() + "(" + std::to_string(name) + ")";
      }
    };
    // machine-readable description
    std::string illustrate (const gp_tableau_t *T) const {
      if (color==green) {
        return color_symbol() + "," + std::to_string(T->player[name]->uniq);
      } else {
        return color_symbol() + "," + std::to_string(name);
      }
    };
    // size of binary serialization
    size_t size (void) const {
      return sizeof(name_t) + sizeof(color_t);
    };
    // binary serialization
    friend raw_t* operator<< (raw_t *o, const ball_t &b) {
      memcpy(o,&b.name,sizeof(name_t)); o += sizeof(name_t);
      memcpy(o,&b.color,sizeof(color_t)); o += sizeof(color_t);
      return o;
    };
    // binary deserialization
    friend raw_t* operator>> (raw_t *o, ball_t &b) {
      memcpy(&b.name,o,sizeof(name_t)); o += sizeof(name_t);
      memcpy(&b.color,o,sizeof(color_t)); o += sizeof(color_t);
      b._hand = na;
      return o;
    };
  };

  // PLAYER CLASS
  // each player has:
  // - a unique name
  // - two balls
  // - knowledge of the players to left and right
  // - a slate to hold the time of seating
  // - knowledge of the Markov process state at time of seating
  class player_t {

  public:
    
    name_t uniq, name;
    ball_t *ballA, *ballB;
    union {player_t *left; name_t lname;};
    union {player_t *right; name_t rname;};
    double slate;
    state_t state;

    player_t (void) = delete;
    // basic constructor
    player_t (color_t col, gp_tableau_t *T) {
      if (col == green) err("bad dog!");
      name = T->nplayers();
      name_t i = T->nballs(col);
      ballA = new ball_t(green,name,name);
      ballB = new ball_t(col,i,name);
      left = 0;
      right = 0;
      slate = default_slate;
      uniq = T->unique();
      T->balls[green].push_back(ballA);
      T->balls[col].push_back(ballB);
      T->player.push_back(this);
    };
    // copy constructor
    player_t (const player_t &p) = delete;
    // move constructor
    player_t (player_t && p) = delete;
    // copy assignment operator
    player_t & operator= (const player_t & p) = delete;
    // move assignment operator
    player_t & operator= (player_t && p) = delete;
    // destructor
    ~player_t (void) {
      delete ballA;
      delete ballB;
    };
    // does this player hold this ball?
    bool holds (ball_t *b) const {
      return (ballA == b) || (ballB == b);
    };
    // does this player hold a ball of this color?
    bool holds (color_t c) const {
      return (ballA->color==c) || (ballB->color==c);
    };
    bool holds_own (void) const {
      return (ballA->is(green) && ballA->name == name) ||
        (ballB->is(green) && ballB->name == name);
    };
    bool is_root (void) const {
      return (holds_own() && !holds(grey));
    };
    // retrieve the first ball of the specified color.
    // if necessary, ballA and ballB are swapped so that
    // internally, the requested ball will be ballA.
    // ballB will be the other.
    ball_t *ball (color_t c) {
      ball_t *b = 0;
      if (ballA->is(c)) {
        b = ballA;
      } else if (ballB->is(c)) { // swap A & B
        b = ballB;
        ballB = ballA;
        ballA = b;
      } else {
        err("no ball of color %s",colores[c]);
      }
      return b;
    };
    // return a pointer to the other ball
    ball_t *other (const ball_t *b) const {
      ball_t *o = 0;
      if (b == ballA) o = ballB;
      else if (b == ballB) o = ballA;
      else err("inconceivable! 'other' error");
      return o;
    };
    // human-readable info
    std::string describe (const gp_tableau_t *T) const {
      return "player(" + std::to_string(uniq) + ") {"
        + ballA->describe(T) + ","
        + ballB->describe(T) + "}, t = "
        + std::to_string(slate) + "\n";
    };
    // machine-readable info
    std::string illustrate (const gp_tableau_t *T) const {
      return std::to_string(uniq) + ","
        + ballA->illustrate(T) + ","
        + ballB->illustrate(T) + ","
        + std::to_string(slate);
    };
    // size of binary serialization
    size_t size (void) const {
      return 4*sizeof(name_t)+sizeof(double)+sizeof(state_t)+2*ballA->size();
    };
    // binary serialization
    friend raw_t* operator<< (raw_t *o, const player_t &p) {
      name_t buf[] = {
        p.uniq, p.name, 
        (p.left != 0)  ? p.left->name  : na,
        (p.right != 0) ? p.right->name : na
      };
      memcpy(o,buf,sizeof(buf)); o += sizeof(buf);
      memcpy(o,&p.slate,sizeof(double)); o += sizeof(double);
      memcpy(o,&p.state,sizeof(state_t)); o += sizeof(state_t);
      return o << *p.ballA << *p.ballB;
    };
    // binary deserialization
    friend raw_t* operator>> (raw_t *o, player_t &p) {
      name_t buf[4], *b = buf;
      memcpy(buf,o,sizeof(buf)); o += sizeof(buf);
      memcpy(&p.slate,o,sizeof(double)); o += sizeof(double);
      memcpy(&p.state,o,sizeof(state_t)); o += sizeof(state_t);
      p.uniq = *b++; p.name = *b++; p.lname = *b++; p.rname = *b++;
      return o >> *p.ballA >> *p.ballB;
    };
  };

private:

  // get the next unique name and generate a new one
  name_t unique (void) {
    name_t u = _unique;
    _unique++;
    return u;
  };
  // draw a random integer in the interval [0,n-1]
  void draw_one (name_t n, name_t *x) const {
    *x = random_integer(n);
  };
  // draw a pair of random integers in the interval [0,n-1]
  void draw_two (name_t n, name_t *x) const {
    x[0] = random_integer(n);
    x[1] = random_integer(n-1);
    if (x[1] >= x[0]) x[1]++;
  };

  // swap balls a and b, wherever they lie
  void swap (ball_t *a, ball_t *b) {
    player_t *A = holder(a);
    player_t *B = holder(b);
    if (a == b || A == B) return;
    if (a == A->ballA)
      A->ballA = b;
    else if (a == A->ballB)
      A->ballB = b;
    else
      err("inconceivable! swap error A");
    if (b == B->ballA)
      B->ballA = a;
    else if (b == B->ballB)
      B->ballB = a;
    else
      err("inconceivable! swap error B");
    a->hand(B->name);
    b->hand(A->name);
  };
  
  player_t* make_player (color_t col) {
    if (max_size_exceeded(1))
      err("maximum tableau size exceeded!");
    return new player_t(col,this);
  };
  
  // clean up
  void clean (void) {
    for (name_t i = 0; i < nplayers(); i++) delete player[i];
    player.clear();
    // relies on sequential ordering of color_t enum:
    for (name_t i = 0; i < ncolors; i++) balls[i].clear();
    left = 0;
    right = 0;
    _time = default_slate;
    _unique = 0;
  };

public:

  // size of serialized binary form
  size_t size (void) const {
    size_t s = (3+ncolors)*sizeof(name_t)+2*sizeof(double)+sizeof(bool)+sizeof(state_t);
    if (!empty()) s += nplayers()*player[0]->size();
    return s;
  };

  // binary serialization
  friend raw_t* operator<< (raw_t *o, const gp_tableau_t &T) {
    name_t buf[ncolors+3], *b = buf;
    double buf2[] = {T._t0, T._time};
    bool buf3[] = {T.use_ghosts};
    for (name_t i = 0; i < ncolors; i++, b++) {
      *b = T.nballs(static_cast<color_t>(i));
    }
    *b++ = T.left->name;
    *b++ = T.right->name;
    *b++ = T._unique;
    memcpy(o,buf,sizeof(buf)); o += sizeof(buf);
    memcpy(o,buf2,sizeof(buf2)); o += sizeof(buf2);
    memcpy(o,buf3,sizeof(buf3)); o += sizeof(buf3);
    memcpy(o,&T.state,sizeof(state_t)); o += sizeof(state_t);
    for (name_t i = 0; i < T.nplayers(); i++)
      o = (o << *T.player[i]);
    return o;
  };

  // binary deserialization
  friend raw_t* operator>> (raw_t *o, gp_tableau_t &T) {
    name_t buf[ncolors+3], *b = buf;
    double buf2[2];
    bool buf3[1];
    memcpy(buf,o,sizeof(buf)); o += sizeof(buf);
    T.clean();
    memcpy(buf2,o,sizeof(buf2)); o += sizeof(buf2);
    T._t0 = buf2[0]; T._time = buf2[1];
    memcpy(buf3,o,sizeof(buf3)); o += sizeof(buf3);
    T.use_ghosts = buf3[0];
    memcpy(&T.state,o,sizeof(state_t)); o += sizeof(state_t);
    name_t np = *b++;
    for (name_t i = 1; i < ncolors; i++, b++) {
      for (name_t j = 0; j < *b; j++) {
        T.make_player(static_cast<color_t>(i));
      }
    }
    T.leftmost  = *b++;
    T.rightmost = *b++;
    T._unique = *b++;
    T.left  = (T.leftmost  != na) ? T.player[T.leftmost]  : 0;
    T.right = (T.rightmost != na) ? T.player[T.rightmost] : 0;
    for (name_t i = 0; i < np; i++) {
      player_t *p = T.player[i];
      o = (o >> *p);
      if (p->name != i) err("yikes! %d %d\n",i,p->name);
      p->left  = (p->lname != na) ? T.player[p->lname] : 0;
      p->right = (p->rname != na) ? T.player[p->rname] : 0;
      T.balls[p->ballA->color][p->ballA->name] = p->ballA;
      T.balls[p->ballB->color][p->ballB->name] = p->ballB;
      p->ballA->hand(i); p->ballB->hand(i);
    }
    return o;
  };

public:
  
  // basic constructor
  //  t0 = initial time
  gp_tableau_t (double t0 = 0, bool ghosts = true) {
    clean();
    _time = _t0 = t0;
    use_ghosts = ghosts;
  };
  // constructor from serialized binary form
  gp_tableau_t (raw_t *o) {
    o >> *this;
  };
  // copy constructor
  gp_tableau_t (const gp_tableau_t & T) {
    raw_t *o = new raw_t[T.size()];
    o << T;
    o >> *this;
    delete[] o;
  };
  // move constructor
  gp_tableau_t (gp_tableau_t &&) = delete;
  // copy assignment operator
  gp_tableau_t & operator= (const gp_tableau_t &) = delete;
  // move assignment operator
  gp_tableau_t & operator= (gp_tableau_t &&) = delete;

  // destructor
  virtual ~gp_tableau_t (void) {
    clean();
  };

  // is empty?
  bool empty (void) const {
    return (left == 0);
  };

  // get current time.
  double time (void) const {
    return _time;
  };

protected:

  // set current time.
  void time (double &t) {
    _time = t;
  };

  // get anchor player
  player_t *anchor (void) const {
    return left;
  };

  // get lead (rightmost) player
  player_t *lead (void) const {
    return right;
  };

  ball_t *green_ball (const player_t *p) const {
    return balls[green][p->name];
  };

  player_t *holder (const ball_t *b) const {
    return player[b->hand()];
  };

  player_t *parent (const player_t *p) const {
    return player[green_ball(p)->hand()];
  };

  player_t *child (const ball_t *g) const {
    if (!g->is(green)) err("in 'child'");
    return player[g->name];
  };

  double dawn (void) const {
    player_t *p = anchor();
    while (p != 0 && !R_FINITE(p->slate)) p = p->right;
    return (p != 0) ? p->slate : R_NaReal;
  };

  double dusk (void) const {
    if (nballs(black) > 0) {
      return _time;
    } else if (nballs(blue) > 0) {
      return holder(balls[blue].back())->slate;
    } else {
      return R_NaReal;
    }
  };

  // get number of players
  name_t nplayers (void) const {
    return player.size();
  };
  
  // get number of balls of a given color
  name_t nballs (const color_t &col) const {
    return balls[col].size();
  };

  // get number of black balls
  name_t nlive (void) const {
    return balls[black].size();
  };
  
public:

  bool live (void) const {
    return (balls[black].size() > 0 && !max_size_exceeded());
  }

  // report all the seating times and lineage count
  name_t lineage_count (double *t = 0, int *ct = 0) const {
    player_t *p = anchor();
    name_t n = 0;
    int count = 0;
    while (p != 0) {
      if (!p->holds(grey)) {
        if (t != 0) *t = p->slate;
        if (ct != 0) {
          count--;
          if (p->ballA->is(green)) count++;
          if (p->ballB->is(green)) count++;
          *ct = count;
        }
        if (!p->holds_own()) {
          n++;
          if (t != 0) t++;
          if (ct != 0) ct++;
        }
      }
      p = p->right;
    }
    return n;
  };

  friend SEXP lineage_count (const gp_tableau_t & T) {
    SEXP t, ct, rv, rvn;
    int nt = T.lineage_count();
    PROTECT(t = NEW_NUMERIC(nt));
    PROTECT(ct = NEW_INTEGER(nt));
    PROTECT(rv = NEW_LIST(2));
    PROTECT(rvn = NEW_CHARACTER(2));
    set_list_elem(rv,rvn,t,"time",0);
    set_list_elem(rv,rvn,ct,"lineages",1);
    SET_NAMES(rv,rvn);
    T.lineage_count(REAL(t),INTEGER(ct));
    UNPROTECT(4);
    return rv;
  }

  // human-readable info
  std::string describe (void) const {
    player_t *p = anchor();
    std::string o = "";
    while (p != 0) {
      o += "pop = " + std::to_string(pop(p->state)) + " ";
      o += p->describe(this);
      p = p->right;
    }
    o += "time = " + std::to_string(time()) + "\n";
    return o;
  };

  // machine-readable info
  std::string illustrate (void) const {
    std::string o = "player,ballAcol,ballA,ballBcol,ballB,slate,t\n";
    player_t *p = anchor();
    while (p != 0) {
      o += p->illustrate(this) + "," + std::to_string(time()) + "\n";
      p = p->right;
    }
    return o;
  };

  // create a human-readable description
  friend SEXP describe (const gp_tableau_t &T) {
    SEXP out;
    PROTECT(out = NEW_CHARACTER(1));
    SET_STRING_ELT(out,0,mkChar(T.describe().c_str()));
    UNPROTECT(1);
    return out;
  }

  // create a machine-readable description
  friend void illustrate (SEXP x, int k, const gp_tableau_t &T) {
    SET_STRING_ELT(x,k,mkChar(T.illustrate().c_str()));
  }

  friend SEXP illustrate (const gp_tableau_t &T) {
    SEXP out;
    PROTECT(out = NEW_CHARACTER(1));
    SET_STRING_ELT(out,0,mkChar(T.illustrate().c_str()));
    UNPROTECT(1);
    return out;
  }

  // check the validity of the gp_tableau.
  void valid (void) const {

    if (player.empty()) {

      if (left != 0 || right != 0)
        err("oy vey!");

    } else {

      player_t *p = 0;

      if (nplayers() != nballs(green)) err("ai yi yi!");
      if (nballs(black)+nballs(red)+nballs(blue)+nballs(grey) != nplayers()) err("caramba!");

      // check each player
      for (name_t j = 0; j < nplayers(); j++) {
        p = player[j];
        if (p->name != j)
          err("player %d has incorrect name (%d)",j,p->name);
        if (p->ballA->hand() != j)
          err("ballA is not in hand (%d)\n%s",p->ballA->hand(),p->describe(this).c_str());
        if (p->ballB->hand() != j)
          err("ballB is not in hand (%d)\n%s",p->ballB->hand(),p->describe(this).c_str());
        if (p->left==0 && p->right==0 && !p->holds_own())
          err("zombie player!\n%s",p->describe(this).c_str());
      }

      // check each color-class of balls
      for (name_t i = 0; i < ncolors; i++) { // relies on sequential ordering of color_t enum
        color_t ii = static_cast<color_t>(i);
        for (name_t j = 0; j < nballs(ii); j++) {
          ball_t *b = balls[i][j];
          if (b->color != ii)
            err("%s ball %d is %s",colores[i],b->name,colores[b->color]);
          if (b->name != j)
            err("ball %d is misnamed",b->name);
          if (!holder(b)->holds(b))
            err("ball %d (color %s) is not in hand of its player",j,b->color_name().c_str());
        }
      }

      // the rightmost player is the "lead"
      p = lead();
      if (p->right != 0)
        err("invalid lead:\n%s\n%s",p->describe(this).c_str(),describe().c_str());
      if (_time < p->slate)
        err("invalid 'time': %le < %le",_time,p->slate);

      // the leftmost player is the "anchor"
      p = anchor();
      if (p->left != 0)
        err("invalid anchor:\n%s\n%s",p->describe(this).c_str(),describe().c_str());

      // check seating arrangement
      name_t n = 1;
      p = anchor();
      while (p->right != 0) {
        n++;
        if (p->right->slate < p->slate) // seating times are out of order
          err("times out of order\n%s%s",p->right->describe(this).c_str(),
              p->describe(this).c_str());
        if (p->right->left != p) err("seven years' bad luck"); // right and left are not mirrored
        p = p->right;
      }
      if (n != nplayers()) err("cannot traverse right %d %d",n,nplayers());
      if (p != right) err("rightmost player is not rightmost");
      while (p->left != 0) {
        n--;
        if (p->left->right != p) err("seven more years!"); // right and left are not mirrored
        p = p->left;
      }
      if (n != 1) err("cannot traverse left %d",n);
      if (p != left) err("leftmost player is not leftmost");

    }
  };

private:

  // recursive function to put genealogy into Newick format.
  std::string newick (name_t &name, const double &tpar) const {

    std::string o = "(";
    player_t *p = player[name];
    ball_t *a = p->ballA;
    ball_t *b = p->ballB;
    double t = p->slate;

    switch (a->color) {
    case black:
      o += "o_" + std::to_string(a->name) + ":" + std::to_string(_time - t) + ",";
      break;
    case red:
      o += "r_" + std::to_string(a->name) + ":0.0,";
      break;
    case blue:
      o += "b_" + std::to_string(a->name) + ":0.0,";
      break;
    case green:
      o += newick(a->name,t) + ",";
      break;
    default:
      err("InCoNcEiVaBlE!");
      break;
    }

    switch (b->color) {
    case black:
      o += "o_" + std::to_string(b->name) + ":" + std::to_string(_time - t);
      break;
    case red:
      o += "r_" + std::to_string(b->name) + ":0.0";
      break;
    case blue:
      o += "b_" + std::to_string(b->name) + ":0.0";
      break;
    case green:
      o += newick(b->name,t);
      break;
    default:
      err("InCoNcEiVaBlE!");
      break;
    }

    o += ")g_" + std::to_string(p->uniq) + ":" + std::to_string(t - tpar);

    return o;
  };

public:

  // put genealogy at current time into Newick format.
  std::string newick (void) const {
    valid();
    player_t *p = anchor();
    double te = dawn(), tl = dusk();
    std::string o = std::to_string(tl) + "(i_:0.0,i_:0.0";
    while (p != 0) {
      if (p->is_root()) {
        ball_t *b = p->other(green_ball(p));
        switch (b->color) {
        case green:
          o += ",((" + newick(b->name,te) + ")g_" + std::to_string(p->name) + ":0.0)i_:0.0";
          break;
        case black:
          o += ",o_" + std::to_string(b->name) + ":" + std::to_string(tl-te);
          break;
        default:
          err("c'est impossible!");
          break;
        }
      }
      p = p->right;
    }
    o += ")i_;";
    return o;
  };

  // extract the tree structure in Newick form.
  // store in element k of character-vector x.
  friend void newick (SEXP x, int k, const gp_tableau_t &T) {
    SET_STRING_ELT(x,k,mkChar(T.newick().c_str()));
  }

  friend SEXP newick (const gp_tableau_t &T) {
    SEXP x;
    std::string s = T.newick();
    PROTECT(x = NEW_CHARACTER(1));
    SET_STRING_ELT(x,0,mkChar(s.c_str()));
    UNPROTECT(1);
    return x;
  }

private:
  
  std::string compact_newick (name_t &name, const double &tpar) const {

    std::string o = "";
    player_t *p = player[name];
    ball_t *a = p->ballA;
    ball_t *b = p->ballB;
    
    // the following depends strongly on the integer equivalents of the color enum:
    // (typedef enum {green = 0, black = 1, blue = 2, red = 3, grey = 4} color_t;)
    int cc = 10*static_cast<short>(a->color)+static_cast<short>(b->color);

    switch (cc) {
    case 00:                    // green+green
      {
        std::string as = compact_newick(a->name,p->slate);
        std::string bs = compact_newick(b->name,p->slate);
        if (!as.empty() && !bs.empty()) {
          o = "(" + as + "," + bs + ")g_" + std::to_string(p->name) + ":" + std::to_string(p->slate - tpar);
        } else if (!as.empty() && bs.empty()) {
          o = as;
        } else if (as.empty() && !bs.empty()) {
          o = bs;
        }
      }
      break;
    case 23: case 32:           // blue+red
      o = "r_" + std::to_string(p->name) + ":" + std::to_string(p->slate - tpar);
      break;
    case 02:                    // green,blue
      {
        std::string as = compact_newick(a->name,p->slate);
        if (!as.empty()) {
          o = "(" + as + ")b_" + std::to_string(p->name) + ":" + std::to_string(p->slate - tpar);
        } else {
          o = "r_" + std::to_string(p->name) + ":" + std::to_string(p->slate - tpar);
        }
      }
      break;
    case 20:                    // blue,green
      {
        std::string bs = compact_newick(b->name,p->slate);
        if (!bs.empty()) {
          o = "(" + bs + ")b_" + std::to_string(p->name) + ":" + std::to_string(p->slate - tpar);
        } else {
          o = "r_" + std::to_string(p->name) + ":" + std::to_string(p->slate - tpar);
        }
      }
      break;
    case 01: case 03:           // green,black or green,red
      o = compact_newick(a->name,tpar);
      break;
    case 10: case 30:           // black,green or red,green
      o = compact_newick(b->name,tpar);
      break;
    default:
      break;
    }

    return o;
  };

public:
  
  // put genealogy at current time into compact Newick format.
  std::string compact_newick (void) const {
    player_t *p = anchor();
    double te = dawn(), tl = dusk();
    std::string o = std::to_string(tl) + "(i_:0.0,i_:0.0";
    while (p != 0) {
      if (p->is_root()) {
        ball_t *b = p->other(green_ball(p));
        switch (b->color) {
        case green:
          {
            std::string bs = compact_newick(b->name,te);
            if (!bs.empty()) {
              o += ",((" + bs + ")g_" + std::to_string(p->name) + ":0.0)i_:0.0";
            }
          }
          break;
        default:
          break;
        }
      }
      p = p->right;
    }
    o += ")i_;";
    return o;
  };

  // extract the tree structure in Newick form.
  // store in element k of character-vector x.
  friend void compact_newick (SEXP x, int k, const gp_tableau_t &T) {
    SET_STRING_ELT(x,k,mkChar(T.compact_newick().c_str()));
  }

  friend SEXP compact_newick (const gp_tableau_t &T) {
    SEXP x;
    std::string s = T.compact_newick();
    PROTECT(x = NEW_CHARACTER(1));
    SET_STRING_ELT(x,0,mkChar(s.c_str()));
    UNPROTECT(1);
    return x;
  }

private:

  // insert player p to the right of player q
  // if q == 0, push onto the right end
  void insert_right (player_t *p, player_t *q = 0) {
    if (empty()) {
      if (q != 0) err("cannot insert_right");
      left = right = p;
      p->left = p->right = 0;
    } else {
      if (q == 0) q = lead();
      p->right = q->right;
      p->left = q;
      q->right = p;
      if (p->right != 0) {
        p->right->left = p;
      } else {
        right = p;
      }
    }
  };

  // insert player p to the left of player q
  // if q == 0, push onto the left end
  void insert_left (player_t *p, player_t *q = 0) {
    if (empty()) {
      if (q != 0) err("cannot insert_left");
      left = right = p;
      p->left = p->right = 0;
    } else {
      if (q == 0) q = anchor();
      p->left = q->left;
      p->right = q;
      q->left = p;
      if (p->left != 0) {
        p->left->right = p;
      } else {
        left = p;
      }
    }
  };

  // remove a player from the seating queue
  void extract (player_t *p) {
    if (!p->holds_own())
      err("cannot extract player:\n%s",p->describe(this).c_str());
    ball_t *pg = green_ball(p);
    ball_t *po = p->other(pg);
    if (po->is(green)) err("naughty kitty!");
    player_t *L = p->left;
    player_t *R = p->right;
    if (L == 0 && R == 0) {
      left = right = 0;
    } else {
      if (L != 0) L->right = R;
      else left = R;
      if (R != 0) R->left = L;
      else right = L;
    }
    p->left = 0;
    p->right = 0;
    p->slate = default_slate;
  }

  // seat the player holding ball a.
  // take as parent the player holding ball b.
  // a will not change hands.
  // b does change hands.
  // the newly seated player will be in the rightmost position
  // holding balls a and b.
  // the parent will have exchanged b for the green ball with
  // the newly seated player's name.
  void seat (const ball_t *a, ball_t *b) {
    if (a->is(green)) err("do not seat by green ball!");
    player_t *p = holder(a);
    ball_t *g = green_ball(p);
    if (!p->holds(g)) err("cannot seat player:\n%s",p->describe(this).c_str());
    swap(g,b);
    insert_right(p);
    p->slate = _time;
  };

  void drop_player (player_t *p) {
    if (!p->holds_own())
      err("cannot drop a player that does not hold his own ball.");
    ball_t *g = green_ball(p);
    ball_t *a = p->other(g);
    if (a->is(green))
      err("cannot drop a player holding two green balls.");
    swap(a,balls[a->color].back());
    swap(g,balls[green].back());
    player_t *q = player.back();
    name_t n = p->name;
    p->name = q->name; q->name = n;
    q->ballA->hand(q->name);
    q->ballB->hand(q->name);
    player[n] = q;
    balls[green].pop_back();
    balls[a->color].pop_back();
    player.pop_back();
    delete p;
  }

  // unseat the player holding ball a.
  void unseat (ball_t *a) {
    if (a->is(green)) err("do not unseat by green ball!");
    player_t *p = holder(a);
    ball_t *g = green_ball(p);
    swap(p->other(a),g);
    extract(p);
    drop_player(p);
  };

  // change a ball's color
  void change (ball_t *b, color_t to) {
    color_t from = b->color;
    if (from == green || to == green)
      err("green balls cannot change color.");
    ball_t *a = balls[from].back();
    swap(b,a);
    a->color = to;
    a->name = balls[to].size();
    balls[from].pop_back();
    balls[to].push_back(a);
  };

  // successively drop all players holding the given color.
  // it is necessary to proceed from right to left.
  void drop (color_t col) {
    while (balls[col].size() > 0) {
      unseat(balls[col].back());
    }
  };

  // drop zero-length branches associated with samples
  void drop_redundant (void) {
    player_t *p = lead();
    while (p != 0) {
      player_t *pp = p->left;
      if (p->holds(red) && p->holds(green)) {
        unseat(p->ball(red));
      }
      p = pp;
    }
  };

  // draw random black ball
  ball_t *random_black_ball (void) {
    name_t draw;
    draw_one(nlive(),&draw);
    return balls[black][draw];
  };

  bool max_size_exceeded (size_t grace = 0) const {
    static size_t maxq = MEMORY_MAX/(sizeof(player_t)+2*sizeof(ball_t));
    return (player.size() > maxq+grace);
  };

private:

  void birth (const state_t &s) {
    ball_t *a = random_black_ball();
    player_t *p = make_player(black);
    p->state = s;
    seat(p->ball(black),a);
  };

  void death (const state_t &s) {
    ball_t *a = random_black_ball();
    unseat(a);
    if (use_ghosts) {
      player_t *p = make_player(grey);
      p->slate = _time;
      p->state = s;
      insert_right(p);
    }
  };

  // graft a new lineage
  void graft (const state_t &s) {
    player_t *p = make_player(black);
    p->slate = _time;
    p->state = s;
    insert_right(p);
  };

  void sample (const state_t &s) {
    if (live()) {
      ball_t *a = random_black_ball();
      player_t *p = make_player(blue);
      player_t *q = make_player(red);
      q->state = p->state = s;
      seat(p->ball(blue),a);
      seat(q->ball(red),a);
    }
  };

public:

  void birth (void) {
    birth(this->state);
  };

  void death (void) {
    death(this->state);
  };

  void graft (void) {
    graft(this->state);
  };

  void sample (void) {
    sample(this->state);
  };
  
  // returns time of next event
  virtual double clock (void) const = 0;
  // updates clocks
  virtual void update_clocks (void) = 0;
  // makes a move
  virtual void move (void) = 0;
  // branching rate and population size
  virtual double branch_rate (state_t &) const = 0;
  virtual double pop (state_t &) const = 0;

public:

  // run process to a specified time.
  // return number of events that have occurred.
  int play (double tfin) {
    int count = R_NaInt;

    if (max_size_exceeded())
      warn("maximum tableau size reached.");
    
    if (!live()) return count;

    double next = clock();

    count = 0;
    while (next < tfin && live()) {
      _time = next;
      move();
      next = clock();
      count++;
    }
    if (next > tfin)  _time = tfin; // relies on Markov property

    return count;
  };

  // take one step of the process
  // return new time.
  double play1 (void) {
    _time = clock();
    if (live()) {
      move();
    }
    return _time;
  };

  // prune the tree
  gp_tableau_t &prune (void) {
    drop(black);
    drop(grey);
    drop_redundant();
    valid();
    return *this;
  };

  // walk backward from each sample.
  // calls to the RNG are made here.
  void walk (double *t, double *haz) const {

    valid();
    GetRNGstate();

    std::vector<int> ell(nplayers(),0);
    std::vector<int> breadcrumb(nplayers(),0);

    if (empty()) return;

    // we walk through the tableau left to right to find the samples
    player_t *P = anchor();
    bool keepfirst = false;
    while (P != 0) {
      if (P->holds(blue)) {      // a sample!
        player_t *p = P;
        player_t *pl = p->left;
        ball_t *g = green_ball(p);
        
        *haz = 0;
        *t = P->slate;
        
        while (pl != 0) {
          // for ease of reading:
          double L = ell[pl->name];
          double N = pop(pl->state);

          if (N < L) err("hijole! %lg %lg\n%s",N,L,pl->describe(this).c_str());

          if (keepfirst) {
            
            if (pl->holds(blue) && breadcrumb[pl->name] < 1) {
              if (N <= L) err("hijole madre! %lg %lg\n%s",N,L,pl->describe(this).c_str());
              double nug = 1/(N-L); // Prob[direct descent]
              if (pl->holds(g)) { // direct descent event
                breadcrumb[pl->name]++;
                *haz -= log(1-runif(0,nug));
              } else if (nug < 1) {            // direct descent avoided
                *haz -= log(1-nug);
              } else {
                err("coalescence should have been assured! (1)");
              }
            }

            if (N > 1 && L > 0) {
              *haz += L/choose(N,2)*branch_rate(pl->state) * (p->slate - pl->slate);
            }
          }
          
          ell[pl->name]++;

          if (!pl->holds(g)) {
            // no ancestor, keep walking
            p = pl;
            pl = p->left;
          } else if (breadcrumb[pl->name] > 0) {
            // an ancestor we've previously encountered, stop
            pl = 0;
          } else if (pl->holds_own()) {
            // end of the line, stop walking
            if (keepfirst) {
              *haz += rexp(1);
            }
            breadcrumb[pl->name]++;
            pl = 0;
          } else {
            // an ancestor we've not yet encountered.
            // we note our encounter with a breadcrumb
            breadcrumb[pl->name]++;
            // and proceed to the next generation.
            g = green_ball(pl);
            p = pl;
            pl = p->left;
          }
        }
        if (keepfirst) {
          t++;
          haz++;
        }
        keepfirst = true;
      }
      P = P->right;
    }
    PutRNGstate();
  };

  // create the serialized state:
  friend SEXP walk (const gp_tableau_t &T) {
    SEXP t, L, rvn, rv = R_NilValue;
    int n = T.nballs(blue)-1;
    n = (n > 0) ? n : 0;
    PROTECT(t = NEW_NUMERIC(n));
    PROTECT(L = NEW_NUMERIC(n));
    PROTECT(rv = NEW_LIST(2));
    PROTECT(rvn = NEW_CHARACTER(2));
    set_list_elem(rv,rvn,t,"time",0);
    set_list_elem(rv,rvn,L,"Lambda",1);
    SET_NAMES(rv,rvn);
    T.walk(REAL(t),REAL(L));
    UNPROTECT(4);
    return rv;
  };

};

// create the serialized state:
template <class GPTYPE>
SEXP serial (const GPTYPE &T) {
  SEXP out;
  PROTECT(out = NEW_RAW(T.size()));
  RAW(out) << T;
  UNPROTECT(1);
  return out;
}

// extract/compute basic information.
template <class GPTYPE>
SEXP get_info (SEXP X, SEXP Prune) {
  int nprotect = 0;
  
  // reconstruct the tableau from its serialization
  GPTYPE gp(RAW(X));
  // check validity
  gp.valid();
  
  // extract current time
  SEXP tout;
  PROTECT(tout = NEW_NUMERIC(1)); nprotect++;
  *REAL(tout) = gp.time();

  // prune if requested
  if (*(INTEGER(AS_INTEGER(Prune)))) gp.prune();

  // pack up return values in a list
  int nout = 7;
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout)); nprotect++;
  PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
  k = set_list_elem(out,outnames,tout,"time",k);
  k = set_list_elem(out,outnames,describe(gp),"description",k);
  k = set_list_elem(out,outnames,illustrate(gp),"illustration",k);
  k = set_list_elem(out,outnames,lineage_count(gp),"lineages",k);
  k = set_list_elem(out,outnames,walk(gp),"cumhaz",k);
  k = set_list_elem(out,outnames,newick(gp),"tree",k);
  k = set_list_elem(out,outnames,compact_newick(gp),"compact_tree",k);
  SET_NAMES(out,outnames);

  UNPROTECT(nprotect);
  return out;
}

// play a genealogy process
// this requires that the RNG state has been handled elsewhere
template<class GPTYPE>
SEXP playGP (GPTYPE *gp, SEXP Times, SEXP Tree, SEXP Ill) {
  int nprotect = 0;
  int nout = 3;
  int ntimes = LENGTH(Times);

  gp->valid();

  SEXP times, count;
  PROTECT(times = AS_NUMERIC(duplicate(Times))); nprotect++;
  PROTECT(count = NEW_INTEGER(ntimes)); nprotect++;

  SEXP tree = R_NilValue;
  int do_tree = *(INTEGER(AS_INTEGER(Tree)));
  if (do_tree) {
    PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  SEXP ill = R_NilValue;
  int do_ill = *(INTEGER(AS_INTEGER(Ill)));
  if (do_ill) {
    PROTECT(ill = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  int *xc = INTEGER(count);
  double *xt = REAL(times);

  if (gp->time() > xt[0]) err("must not have t0 = %lg > %g = times[1]!",gp->time(),xt[0]);

  for (int k = 0; k < ntimes; k++, xc++, xt++) {
    *xc = gp->play(*xt);
    if (do_tree) newick(tree,k,*gp);
    if (do_ill) illustrate(ill,k,*gp);
    R_CheckUserInterrupt();
  }
      
  gp->valid();
    
  // pack everything up in a list
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout)); nprotect++;
  PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
  k = set_list_elem(out,outnames,times,"time",k);
  k = set_list_elem(out,outnames,count,"count",k);
  if (do_tree) {
    k = set_list_elem(out,outnames,tree,"tree",k);
  }
  if (do_ill) {
    k = set_list_elem(out,outnames,ill,"illustration",k);
  }
  k = set_list_elem(out,outnames,serial(*gp),"state",k);
  SET_NAMES(out,outnames);

  UNPROTECT(nprotect);
  return out;
}

// play a sampled  genealogy process
// this requires that the RNG state has been handled elsewhere
template<class GPTYPE>
SEXP playSGP (GPTYPE *gp, SEXP Times, SEXP Tree, SEXP Ill) {
  int nprotect = 0;
  int nout = 3;
  int ntimes = LENGTH(Times);

  gp->valid();

  SEXP times, count;
  PROTECT(times = AS_NUMERIC(duplicate(Times))); nprotect++;
  PROTECT(count = NEW_INTEGER(ntimes)); nprotect++;

  SEXP tree = R_NilValue;
  int do_tree = *(INTEGER(AS_INTEGER(Tree)));
  if (do_tree) {
    PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  SEXP ill = R_NilValue;
  int do_ill = *(INTEGER(AS_INTEGER(Ill)));
  if (do_ill) {
    PROTECT(ill = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  int *xc = INTEGER(count);
  double *xt = REAL(times);

  if (gp->time() > xt[0]) err("must not have t0 = %lg > %g = times[1]!",gp->time(),xt[0]);

  for (int k = 0; k < ntimes; k++, xc++, xt++) {
    *xc = gp->play(*xt);
    gp->sample();
    if (do_tree) newick(tree,k,*gp);
    if (do_ill) illustrate(ill,k,*gp);
    R_CheckUserInterrupt();
  }
      
  gp->valid();
    
  // pack everything up in a list
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout)); nprotect++;
  PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
  k = set_list_elem(out,outnames,times,"time",k);
  k = set_list_elem(out,outnames,count,"count",k);
  if (do_tree) {
    k = set_list_elem(out,outnames,tree,"tree",k);
  }
  if (do_ill) {
    k = set_list_elem(out,outnames,ill,"illustration",k);
  }
  k = set_list_elem(out,outnames,serial(*gp),"state",k);
  SET_NAMES(out,outnames);

  UNPROTECT(nprotect);
  return out;
}

template<class GPTYPE>
SEXP playWChain (GPTYPE *gp, SEXP N, SEXP Tree, SEXP Ill) {
  int nprotect = 0;
  int nout = 2;
  int ntimes = *(INTEGER(AS_INTEGER(N)));

  gp->valid();

  SEXP times;
  PROTECT(times = NEW_NUMERIC(ntimes)); nprotect++;

  SEXP tree = R_NilValue;
  int do_tree = *(INTEGER(AS_INTEGER(Tree)));
  if (do_tree) {
    PROTECT(tree = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  SEXP ill = R_NilValue;
  int do_ill = *(INTEGER(AS_INTEGER(Ill)));
  if (do_ill) {
    PROTECT(ill = NEW_CHARACTER(ntimes)); nprotect++;
    nout++;
  }

  double *xt = REAL(times);

  GetRNGstate();
  for (int k = 0; k < ntimes; k++, xt++) {
    *xt = gp->play1();
    if (do_tree) newick(tree,k,*gp);
    if (do_ill) illustrate(ill,k,*gp);
    R_CheckUserInterrupt();
  }
  PutRNGstate();
      
  gp->valid();
    
  // pack everything up in a list
  int k = 0;
  SEXP out, outnames;
  PROTECT(out = NEW_LIST(nout)); nprotect++;
  PROTECT(outnames = NEW_CHARACTER(nout)); nprotect++;
  k = set_list_elem(out,outnames,times,"time",k);
  if (do_tree) {
    k = set_list_elem(out,outnames,tree,"tree",k);
  }
  if (do_ill) {
    k = set_list_elem(out,outnames,ill,"illustration",k);
  }
  k = set_list_elem(out,outnames,serial(*gp),"state",k);
  SET_NAMES(out,outnames);

  UNPROTECT(nprotect);
  return out;
}

#endif
