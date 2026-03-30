#include "ball.h"
#include "pocket.h"
#include "node.h"
#include "nodeseq.h"
#include "genealogy.h"
#include "internal.h"

//! human-readable info
string_t
ball_t::describe
(void) const
{
  string_t o = color_name()
    + "(" + std::to_string(uniq) + ",";
  if (deme() != undeme) {
    o += std::to_string(deme());
  }
  o += ")";
  return o;
}


string_t
pocket_t::describe
(void) const
{
  string_t s = "{";
  ball_it i = begin();
  s += (*i)->describe(); ++i;
  while (i != end()) {
    s += ", " + (*i)->describe(); ++i;
  }
  s += "}";
  return s;
}

string_t
node_t::describe
(void) const
{
  string_t s = "node("
    + std::to_string(uniq)
    + "," + std::to_string(deme()) + ",";
  if (lineage() != null_lineage) {
    s += std::to_string(lineage());
  }
  s += ")" + pocket_t::describe();
  s += ", t = " + std::to_string(slate) + "\n";
  return s;
}

string_t
nodeseq_t::describe
(void) const
{
  string_t o = "";
  for (node_t *p : *this) {
    o += p->describe();
  }
  return o;
}

string_t
genealogy_t::describe
(void) const
{
  string_t o = "t0 = " + std::to_string(double(timezero()))
    + "\ntime = " + std::to_string(double(time())) + "\n"
    + nodeseq_t::describe();
  return o;
}
