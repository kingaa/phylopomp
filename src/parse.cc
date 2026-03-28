#include "genealogy.h"
#include "generics.h"
#include "internal.h"
#include <regex>
#include <unordered_map>

void
nodeseq_t::drop_zlb
(void)
{
  for (node_t *p : *this) {
    if (!p->holds_own() && p->slate == p->parent()->slate) {
      while (!p->empty()) {
        ball_t *b = p->last_ball();
        p->erase(b); p->parent()->insert(b);
        //FIXME: do we also need to change ball-demes?
      }
      detach(p);
    }
  }
}

void
genealogy_t::repair_tips
(void)
{
  for (node_t *p : *this) {
    if (p->empty()) {
      ball_t *b = new ball_t(p,p->uniq,blue,p->deme());
      p->insert(b);
    } else if (p->holds(black))
      swap(p->last_ball(),p->green_ball());
  }
}

//! simple function for scanning a slate_t from a string
//! (with error trapping)
static
slate_t scan_slate
(const string_t& s)
{
  double bl;
  try {
    bl = (s.empty()) ? 0.0 : stod(s);
  }
  catch (const std::invalid_argument& e) {
    err("in '%s': invalid Newick format: branch length should be a non-negative decimal number.",__func__);
  }
  catch (const std::out_of_range& e) {
    err("in '%s': invalid Newick format: branch length out of range.",__func__);
  }
  catch (const std::exception& e) {
    err("in '%s': parsing branch-length: %s.",__func__,e.what());
  }
  catch (...) {
    err("in '%s': other branch-length parsing error.",__func__);
  }
  if (bl < 0.0) err("in '%s': negative branch length detected.",__func__);
  return slate_t(bl);
}

//! simple function for scanning a name_t from a string
//! (with error trapping)
static
name_t scan_name
(const string_t& s)
{
  int d;
  try {
    d = stoi(s);
    if (d < 0) err("in '%s': negative deme number detected.",__func__);
  }
  catch (const std::invalid_argument& e) {
    err("in '%s': invalid Newick format: deme should be indicated with an integer.",__func__);
  }
  catch (const std::out_of_range& e) {
    err("in '%s': invalid Newick format: deme out of range.",__func__);
  }
  catch (const std::exception& e) {
    err("in '%s': parsing deme label: %s.",__func__,e.what());
  }
  catch (...) {
    err("in '%s': other deme-parsing error.",__func__);
  }
  return name_t(d);
}

//! simple function for scanning the color
static
color_t scan_color
(const std::string& s)
{
  std::string copy(s);
  const std::unordered_map<string_t,color_t> options({
      {"sample",blue},{"extant",black},{"migration",green},
      {"node",green},{"branch",green},{"root",green}
    });
  color_t col = green;
  std::transform(copy.begin(),copy.end(),copy.begin(),
                 [](unsigned char c){return std::tolower(c);});
  try {
    col = options.at(copy);
  }
  catch (const std::out_of_range& e) {
    err("in %s: invalid metadata: type '%s' not recognized.",__func__,s.c_str());
  }
  return col;
}

//! Scan the label string.
//! This has format [&&PhyloPOMP deme=%d type=%s]%s:%f
node_t *genealogy_t::scan_branch
(string_t::const_iterator b,
 string_t::const_iterator e)
{
  color_t col = green;
  name_t deme = 0;
  slate_t bl = 0;
  if (b != e) {
    string_t s(b,e);
    const std::regex wre("^(.*?)(?::([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?))?$");
    std::smatch wm;
    if (std::regex_match(s,wm,wre)) {
      const string_t label = wm[1].str();
      const std::regex dre("^.*?\\[&&PhyloPOMP.+?deme=(\\w+).*?\\].*$");
      const std::regex tre("^.*?\\[&&PhyloPOMP.+?type=(\\w+).*?\\].*$");
      std::smatch mm;
      if (std::regex_match(label,mm,dre)) {
        deme = scan_name(mm[1].str());
      }
      if (std::regex_match(label,mm,tre)) {
        col = scan_color(mm[1].str());
      }
      if (wm[2].str().empty())
        warn("in '%s': in branch string '%s': no branch-length detected: assuming zero branch length.",
             __func__,s.c_str());
      bl = scan_slate(wm[2].str());
    } else {
      assert(0);                // #nocov
      //      err("in '%s': branch-string '%s' is improperly formatted.",__func__,s.c_str()); // unreachable?
    }
  }
  node_t *q = make_node(deme);
  if (col != green) {
    ball_t *b = new ball_t(q,q->uniq,col,deme);
    q->insert(b);
  }
  q->slate = bl;
  return q;
}

//! Parse a Newick string and create the indicated genealogy.
genealogy_t&
genealogy_t::parse
(const string_t& s)
{
  node_t *p = 0, *q;
  slate_t tf = timezero();
  string_t::const_reverse_iterator b = s.crbegin(), e = b;
  bool open = false;            // open for scanning node specs
  int stack = 0, sqstack = 0;
  if (!s.empty() && *b != ';')
    err("in '%s': invalid Newick format: no final semicolon.",__func__);
  while (b != s.crend()) {
    switch (*b) {
    case ';':                   // root
      if (stack != 0)
        err("in '%s': invalid Newick: unbalanced parentheses.",__func__);
      p = make_node();
      p->slate = timezero();
      push_front(p);
      b++;
      e = b;
      open = true;
      break;
    case ')':                   // internal node
      if (open) {
        q = scan_branch(b.base(),e.base());
        if (q->holds(black))
          err("in '%s': 'type=extant' on internal node.",__func__);
        q->slate += p->slate;
        attach(p,q);
        push_back(q);
        tf = (q->slate > tf) ? q->slate : tf;
        ndeme() = (ndeme() > q->deme()) ? ndeme() : q->deme();
        p = q;
      } else {
        err("in '%s': invalid Newick: missing comma or semicolon.",__func__);
      }
      stack++;
      b++;
      e = b;
      open = true;
      break;
    case '(':                   // tip node, eldest sister
      if (open) {
        q = scan_branch(b.base(),e.base());
        q->slate += p->slate;
        attach(p,q);
        push_back(q);
        tf = (q->slate > tf) ? q->slate : tf;
        ndeme() = (ndeme() > q->deme()) ? ndeme() : q->deme();
      }
      p = p->parent();
      b++;
      stack--;
      e = b;
      open = false;
      break;
    case ',':                   // tip node, younger sister
      if (stack == 0)
        err("in '%s': invalid Newick string: misplaced comma.",__func__);
      if (open) {
        q = scan_branch(b.base(),e.base());
        q->slate += p->slate;
        attach(p,q);
        push_back(q);
        tf = (q->slate > tf) ? q->slate : tf;
        ndeme() = (ndeme() > q->deme()) ? ndeme() : q->deme();
      }
      b++;
      e = b;
      open = true;
      break;
    case ']':                   // skip metadata
      sqstack++;
      while (b != s.crend() && sqstack > 0) {
        b++;
        if (*b == ']') sqstack++;
        if (*b == '[') sqstack--;
      }
      if (sqstack != 0)
        err("in '%s': invalid Newick format: unbalanced square brackets.",__func__);
      if (b != s.crend()) b++;
      break;
    default:
      b++;
      break;
    }
  }
  if (stack != 0)
    err("in '%s': invalid Newick format: unbalanced parentheses.",__func__);
  if (open) {
    q = scan_branch(b.base(),e.base());
    q->slate += p->slate;
    attach(p,q);
    push_back(q);
    tf = (q->slate > tf) ? q->slate : tf;
    ndeme() = (ndeme() > q->deme()) ? ndeme() : q->deme();
  }
  time() = tf;
  sort(); repair_tips(); drop_zlb(); weed();
  return *this;
}

extern "C" {

  //! A parser for Newick code.
  //! Returns a genealogy in the phylopomp format.
  SEXP parse_newick (SEXP X, SEXP T0, SEXP Tf) {
    PROTECT(X = AS_CHARACTER(X));
    PROTECT(T0 = AS_NUMERIC(T0));
    PROTECT(Tf = AS_NUMERIC(Tf));
    double t0 = *REAL(T0);
    double tf = *REAL(Tf);
    // parse the Newick representation into a genealogy:
    string_t x = CHAR(STRING_ELT(X,0));
    genealogy_t G(t0);
    G.parse(x);
    if (!ISNA(tf)) {
      G.curtail(tf,t0);
    }
    G.trace_lineages();
    UNPROTECT(3);
    return serial(G);
  }

}
