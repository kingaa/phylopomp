#include "genealogy.h"
#include "generics.h"
#include "internal.h"
#include <regex>
#include <unordered_map>

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
      const std::regex dre("^.*?\\[&&PhyloPOMP.+?deme=(\\w*).*?\\].*$");
      const std::regex tre("^.*?\\[&&PhyloPOMP.+?type=(\\w*).*?\\].*$");
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
      assert(0);		// #nocov
      //      err("in '%s': branch-string '%s' is improperly formatted.",__func__,s.c_str()); // unreachable?
    }
    if (col == black)
      err("in '%s': cannot parse Newick with extant tips.",__func__);
  }
  node_t *q = make_node(deme);
  if (col == blue) {
    ball_t *b = new ball_t(q,q->uniq,blue,deme);
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
  string_t::const_reverse_iterator pos1 = s.crbegin(), pos2 = pos1;
  int stack = 0, sqstack = 0;
  bool needs_deme = false;      // FIXME: this is a kluge
  if (!s.empty() && *pos1 != ';')
    err("in '%s': invalid Newick format: no final semicolon.",__func__);
  while (pos1 != s.crend()) {
    switch (*pos1) {
    case ';':
      p = make_node(0); needs_deme = true;
      p->slate = timezero();
      push_front(p);
      pos1++;
      pos2 = pos1;
      break;
    case ')': case '(': case ',':
      q = scan_branch(pos1.base(),pos2.base());
      ndeme() = (ndeme() > q->deme()+1) ? ndeme() : q->deme()+1;
      q->slate += p->slate;
      tf = (q->slate > tf) ? q->slate : tf;
      attach(p,q);
      push_back(q);
      if (needs_deme) {
        q->parent()->deme() = q->deme();
        needs_deme = false;
      }
      switch (*pos1) {
      case ')':
        p = q;
        pos1++;
        stack++;
        break;
      case '(':
        while (pos1 != s.crend() && *pos1 == '(') {
          p = p->parent();
          pos1++;
          stack--;
        }
        if (pos1 != s.crend() && *pos1 == ',') pos1++;
        break;
      default:
        pos1++;
        break;
      }
      pos2 = pos1;
      break;
    case ']':                   // skip metadata
      sqstack++;
      while (pos1 != s.crend() && sqstack > 0) {
        pos1++;
        if (*pos1 == ']') sqstack++;
        if (*pos1 == '[') sqstack--;
      }
      if (sqstack != 0)
        err("in '%s': invalid Newick format: unbalanced square brackets.",__func__);
      if (pos1 != s.crend()) pos1++;
      break;
    default:
      pos1++;
      break;
    }
  }
  if (stack != 0)
    err("in '%s': invalid Newick format: unbalanced parentheses.",__func__);
  time() = tf;
  sort(); repair_tips(); drop_zlb();
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
