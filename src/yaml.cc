// YAML output

#include "genealogy.h"
#include "internal.h"

string_t
ball_t::yaml
(string_t tab) const
{
  string_t o;
  o = "color: " + color_name() + "\n"
    + tab + "name: " + std::to_string(uniq) + "\n";
  if (color==black) {
    o += tab + "deme: " + std::to_string(deme()) + "\n";
  }
  return o;
}

string_t
pocket_t::yaml
(string_t tab) const
{
  string_t o = "";
  string_t t = tab + "  ";
  for (ball_t *b : *this) {
    o += tab + "- " + b->yaml(t);
  }
  return o;
}

string_t
node_t::yaml
(string_t tab) const
{
  string_t t = tab + "  ";
  string_t o = "name: " + std::to_string(uniq) + "\n"
    + tab + "time: " + std::to_string(slate) + "\n"
    + tab + "deme: " + std::to_string(deme()) + "\n";
  if (lineage() != null_lineage) {
    o += tab + "lineage: " + std::to_string(lineage()) + "\n";
  }
  o += tab + "pocket:\n" + pocket_t::yaml(tab);
  return o;
}

string_t
nodeseq_t::yaml
(string_t tab) const
{
  string_t o = "";
  string_t t = tab + "  ";
  for (node_t *p : *this) {
    o += tab + "- " + p->yaml(t);
  }
  return o;
}

string_t
genealogy_t::yaml
(string_t tab) const
{
  string_t o;
  string_t t = tab + "  ";
  o = tab + "t0: " + std::to_string(timezero()) + "\n"
    + tab + "time: " + std::to_string(time()) + "\n"
    + tab + "ndeme: " + std::to_string(ndeme()) + "\n"
    + tab + "nodes:\n" + nodeseq_t::yaml(tab);
  return o;
}

extern "C" {

  //! extract a YAML description
  SEXP yaml (SEXP State) {
    genealogy_t A = State;
    return mkString(A.yaml().c_str());
  }

}
