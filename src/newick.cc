#include "genealogy.h"
#include "internal.h"

//! Element of a Newick representation.
//! This should only be called at tip-nodes.
string_t
ball_t::newick
(const slate_t &t, bool showdeme) const
{
  assert(color==black);
  string_t o = "[&&PhyloPOMP:type=extant";
  if (showdeme)
    o += ",deme=" + std::to_string(deme());
  o += "]"
    + std::to_string(uniq) +
    ":" + std::to_string(t);
  return o;
}

//! Newick format with phylopomp extension
//! Deme and node-type information is returned in a metadata wrapper.
string_t
node_t::newick
(const slate_t& tnow, const slate_t& tpar,
 bool showdeme, bool extended) const
{
  string_t o1 = "", o2 = "", o3 = "";
  int n = nchildren();
  if (n > 0) {
    o1 = "("; o3 = ")";
  }
  if (extended) {
    o3 += "[&&PhyloPOMP:";
    if (holds(blue))
      o3 += "type=sample";
    else if (holds_own())
      o3 += "type=root";
    else
      o3 += "type=node";
    if (showdeme)
      o3 += ",deme=" + std::to_string(deme());
    o3 += "]";
  }
  n = 0;
  for (ball_t *b : *this) {
    node_t *p = 0;
    switch (b->color) {
    case green:
      p = b->child();
      if (p != this) {
        if (n++ > 0) o2 += ",";
        o2 += p->newick(tnow,slate,showdeme,extended);
      }
      break;
    case black:
      assert(extended);
      if (n++ > 0) o2 += ",";
      o2 += b->newick(tnow-slate,showdeme);
      break;
    case blue:
      break;
    }
  }
  return o1 + o2 + o3
    + std::to_string(uniq)
    + ":" + std::to_string(slate - tpar);
}

//! put genealogy at time `t` into Newick format.
string_t
nodeseq_t::newick
(slate_t t, bool showdeme, bool extended) const
{
  slate_t te = dawn();
  string_t o = "";
  for (node_t *p : *this) {
    if (p->is_root()) {
      o += p->newick(t,te,showdeme,extended) + ";";
    }
  }
  return o;
}

//! put genealogy at current time into Newick format.
string_t
genealogy_t::newick
(bool extended) const
{
  return nodeseq_t::newick(time(),(ndeme() > 1),extended);
}
