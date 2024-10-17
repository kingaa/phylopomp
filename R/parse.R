##' parse a Newick string
##'
##' Parses a Newick description and returns a binary version of the genealogy.
##'
##' \code{parse_newick} can only handle a subset of the full Newick specification.
##' In particular, labels are assumed to be of the form <TYPE>_<DEME>_<LABEL>,
##' i.e., each label has three parts, separated by underscores (\sQuote{_}).
##' The parts are as follows.
##' \itemize{
##' \item TYPE must be a single character from among the following: \sQuote{b}, \sQuote{g}, \sQuote{m}, \sQuote{o}.
##' \itemize{
##' \item \sQuote{b} signifies a sample.
##' \item \sQuote{g} signifies an internal node.
##' \item \sQuote{m} signifies a root.
##' \item \sQuote{o} indicates an extant lineage.
##' }
##' \item DEME must be a non-negative integer, specifying the deme in which the branch resides.
##' If deme information is not present, use 0.
##' \item LABEL is ignored and may be left out.
##' }
##' @name parse_newick
##' @param x character; the Newick description.
##' See Details for specifics.
##' @param t0 numeric; the root time.
##' @param tf numeric; the current or final time.
##' @importFrom tibble tibble
##' @return
##' An object of class \dQuote{gpgen}.
##' @rdname parse
##' @export
parse_newick <- function (x, t0 = 0, tf = NA) {
  .Call(P_parse_newick,x,t0,tf) |>
    structure(class="gpgen")
}
