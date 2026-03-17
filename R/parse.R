##' Parse a Newick string
##'
##' Parses a Newick description and returns a bare genealogy.
##'
##' Labels are assumed to be of the form TYPE_DEME_LABEL,
##' where TYPE is one of: \sQuote{b} (sample), \sQuote{g} (internal),
##' \sQuote{m} (root), \sQuote{o} (extant lineage).
##'
##' @name parse_newick
##' @param x character; the Newick description.
##' @param t0 numeric; the root time.
##' @param tf numeric; the current or final time.
##' @return An object of class \sQuote{gpgen}.
##' @rdname parse
##' @export
parse_newick <- function (x, t0 = 0, tf = NA) {
  .Call(P_parse_newick,x,t0,tf) |>
    structure(class="gpgen")
}
